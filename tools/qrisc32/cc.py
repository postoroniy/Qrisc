"""Tiny qrisc32 C-subset compiler.

This is intentionally small: one int main(void), int scalars, fixed int arrays,
structured control flow, integer expressions, and no ABI/libc.
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path


class CompileError(ValueError):
    pass


Token = tuple[str, str]


def tokenize(text: str) -> list[Token]:
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
    text = re.sub(r"//.*", "", text)
    token_re = re.compile(
        r"\s+|"
        r"(?P<num>0x[0-9a-fA-F]+|\d+)|"
        r"(?P<id>[A-Za-z_][A-Za-z0-9_]*)|"
        r"(?P<op>==|!=|<=|>=|<<|>>|[{}()\[\];,=+\-*&|^<>])"
    )
    tokens: list[Token] = []
    pos = 0
    while pos < len(text):
        match = token_re.match(text, pos)
        if not match:
            raise CompileError(f"unexpected character {text[pos]!r}")
        pos = match.end()
        if match.group().isspace():
            continue
        if match.lastgroup == "num":
            tokens.append(("num", match.group()))
        elif match.lastgroup == "id":
            tokens.append(("id", match.group()))
        else:
            tokens.append(("op", match.group()))
    tokens.append(("eof", ""))
    return tokens


@dataclass
class Num:
    value: int


@dataclass
class Var:
    name: str


@dataclass
class ArrayRef:
    name: str
    index: object


@dataclass
class Unary:
    op: str
    expr: object


@dataclass
class Bin:
    op: str
    left: object
    right: object


@dataclass
class Block:
    statements: list[object]


@dataclass
class Decl:
    name: str
    size: int | None
    init: object | list[int] | None


@dataclass
class Assign:
    target: object
    expr: object


@dataclass
class If:
    cond: object
    then_stmt: object
    else_stmt: object | None


@dataclass
class While:
    cond: object
    body: object


@dataclass
class For:
    init: object | None
    cond: object | None
    post: object | None
    body: object


@dataclass
class Break:
    pass


@dataclass
class Continue:
    pass


@dataclass
class Return:
    expr: object | None


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.pos = 0

    def peek(self) -> str:
        return self.tokens[self.pos][1]

    def kind(self) -> str:
        return self.tokens[self.pos][0]

    def take(self, value: str | None = None) -> str:
        got = self.peek()
        if value is not None and got != value:
            raise CompileError(f"expected {value!r}, got {got!r}")
        self.pos += 1
        return got

    def take_id(self) -> str:
        if self.kind() != "id":
            raise CompileError(f"expected identifier, got {self.peek()!r}")
        return self.take()

    def accept(self, value: str) -> bool:
        if self.peek() == value:
            self.pos += 1
            return True
        return False

    def parse_program(self) -> Block:
        self.take("int")
        if self.take_id() != "main":
            raise CompileError("only int main(void) is supported")
        self.take("(")
        if self.peek() == "void":
            self.take("void")
        self.take(")")
        return self.parse_block()

    def parse_block(self) -> Block:
        self.take("{")
        stmts: list[object] = []
        while not self.accept("}"):
            stmts.append(self.parse_statement())
        return Block(stmts)

    def parse_statement(self) -> object:
        tok = self.peek()
        if tok == "{":
            return self.parse_block()
        if tok == "int":
            return self.parse_decl(expect_semicolon=True)
        if tok == "if":
            self.take("if")
            self.take("(")
            cond = self.parse_expr()
            self.take(")")
            then_stmt = self.parse_statement()
            else_stmt = self.parse_statement() if self.accept("else") else None
            return If(cond, then_stmt, else_stmt)
        if tok == "while":
            self.take("while")
            self.take("(")
            cond = self.parse_expr()
            self.take(")")
            return While(cond, self.parse_statement())
        if tok == "for":
            self.take("for")
            self.take("(")
            init = None if self.accept(";") else self.parse_for_part()
            if init is not None:
                self.take(";")
            cond = None if self.accept(";") else self.parse_expr()
            if cond is not None:
                self.take(";")
            post = None if self.accept(")") else self.parse_for_part()
            if post is not None:
                self.take(")")
            return For(init, cond, post, self.parse_statement())
        if tok == "break":
            self.take("break")
            self.take(";")
            return Break()
        if tok == "continue":
            self.take("continue")
            self.take(";")
            return Continue()
        if tok == "return":
            self.take("return")
            expr = None if self.peek() == ";" else self.parse_expr()
            self.take(";")
            return Return(expr)
        stmt = self.parse_for_part()
        self.take(";")
        return stmt

    def parse_for_part(self) -> object:
        if self.peek() == "int":
            return self.parse_decl(expect_semicolon=False)
        target = self.parse_lvalue()
        if self.accept("="):
            return Assign(target, self.parse_expr())
        raise CompileError("expected assignment")

    def parse_decl(self, expect_semicolon: bool) -> Decl:
        self.take("int")
        name = self.take_id()
        size = None
        init: object | list[int] | None = None
        if self.accept("["):
            size = int(self.take(), 0)
            self.take("]")
        if self.accept("="):
            if size is not None and self.accept("{"):
                values: list[int] = []
                if not self.accept("}"):
                    while True:
                        values.append(self.parse_const_expr())
                        if self.accept("}"):
                            break
                        self.take(",")
                init = values
            else:
                init = self.parse_expr()
        if expect_semicolon:
            self.take(";")
        return Decl(name, size, init)

    def parse_const_expr(self) -> int:
        sign = -1 if self.accept("-") else 1
        if self.kind() != "num":
            raise CompileError("array initializers must be integer constants")
        return sign * int(self.take(), 0)

    def parse_lvalue(self) -> object:
        name = self.take_id()
        if self.accept("["):
            index = self.parse_expr()
            self.take("]")
            return ArrayRef(name, index)
        return Var(name)

    def parse_expr(self) -> object:
        return self.parse_bit_or()

    def parse_bit_or(self) -> object:
        expr = self.parse_bit_xor()
        while self.accept("|"):
            expr = Bin("|", expr, self.parse_bit_xor())
        return expr

    def parse_bit_xor(self) -> object:
        expr = self.parse_bit_and()
        while self.accept("^"):
            expr = Bin("^", expr, self.parse_bit_and())
        return expr

    def parse_bit_and(self) -> object:
        expr = self.parse_equality()
        while self.accept("&"):
            expr = Bin("&", expr, self.parse_equality())
        return expr

    def parse_equality(self) -> object:
        expr = self.parse_relational()
        while self.peek() in ("==", "!="):
            op = self.take()
            expr = Bin(op, expr, self.parse_relational())
        return expr

    def parse_relational(self) -> object:
        expr = self.parse_shift()
        while self.peek() in ("<", "<=", ">", ">="):
            op = self.take()
            expr = Bin(op, expr, self.parse_shift())
        return expr

    def parse_shift(self) -> object:
        expr = self.parse_add()
        while self.peek() in ("<<", ">>"):
            op = self.take()
            expr = Bin(op, expr, self.parse_add())
        return expr

    def parse_add(self) -> object:
        expr = self.parse_mul()
        while self.peek() in ("+", "-"):
            op = self.take()
            expr = Bin(op, expr, self.parse_mul())
        return expr

    def parse_mul(self) -> object:
        expr = self.parse_unary()
        while self.accept("*"):
            expr = Bin("*", expr, self.parse_unary())
        return expr

    def parse_unary(self) -> object:
        if self.accept("-"):
            return Unary("-", self.parse_unary())
        return self.parse_primary()

    def parse_primary(self) -> object:
        if self.accept("("):
            expr = self.parse_expr()
            self.take(")")
            return expr
        if self.kind() == "num":
            return Num(int(self.take(), 0))
        return self.parse_lvalue()


@dataclass
class VarInfo:
    reg: int | None = None
    base_word: int | None = None
    size: int | None = None


class CodeGen:
    def __init__(self, schedule: str = "compact"):
        if schedule not in {"compact", "safe", "legacy", "none"}:
            raise CompileError(f"unsupported schedule mode: {schedule}")
        self.schedule = schedule
        self.lines: list[str] = []
        self.dmem: list[int] = []
        self.vars: dict[str, VarInfo] = {}
        self.next_reg = 1
        self.scratch = list(range(23, 15, -1))
        self.label_id = 0
        self.break_stack: list[str] = []
        self.continue_stack: list[str] = []
        self.const_env: dict[str, int] = {}

    def emit(self, line: str = "") -> None:
        self.lines.append(line)
        stripped = line.strip()
        if (
            self.schedule == "legacy"
            and stripped
            and not stripped.startswith("#")
            and not stripped.endswith(":")
            and stripped != "NOP"
        ):
            self.lines.extend(["  NOP"] * 6)

    def label(self, prefix: str) -> str:
        name = f"__{prefix}_{self.label_id}"
        self.label_id += 1
        return name

    def alloc_reg(self) -> int:
        if self.next_reg > 12:
            raise CompileError("too many scalar variables for tiny register allocator")
        reg = self.next_reg
        self.next_reg += 1
        return reg

    def tmp(self) -> int:
        if not self.scratch:
            raise CompileError("expression too complex for scratch register pool")
        return self.scratch.pop()

    def free(self, reg: int) -> None:
        if 16 <= reg <= 23 and reg not in self.scratch:
            self.scratch.append(reg)

    def reg(self, idx: int) -> str:
        return f"r{idx}"

    def load_const(self, dst: int, value: int) -> None:
        value &= 0xFFFF_FFFF
        self.emit(f"  LDRH {self.reg(dst)}, 0x{(value >> 16) & 0xFFFF:04x}")
        self.emit(f"  LDRL {self.reg(dst)}, 0x{value & 0xFFFF:04x}")

    def compile(self, block: Block) -> str:
        self.emit("# generated by qrisc32-cc")
        self.load_const(0, 0)
        self.compile_stmt(block)
        self.emit("__halt:")
        self.emit("  JMP __halt")
        self.emit("  NOP")
        dmem_lines = []
        if self.dmem:
            values = ", ".join(str(v & 0xFFFF_FFFF) for v in self.dmem)
            dmem_lines = ["# data memory image", f".dmem {values}", ""]
        return "\n".join([*dmem_lines, *self.lines, ""])

    def compile_stmt(self, stmt: object) -> None:
        if isinstance(stmt, Block):
            for item in stmt.statements:
                self.compile_stmt(item)
        elif isinstance(stmt, Decl):
            self.declare(stmt)
        elif isinstance(stmt, Assign):
            self.assign(stmt.target, stmt.expr)
        elif isinstance(stmt, If):
            else_label = self.label("else")
            end_label = self.label("endif")
            self.branch_false(stmt.cond, else_label)
            self.compile_stmt(stmt.then_stmt)
            if stmt.else_stmt is not None:
                self.emit(f"  JMP {end_label}")
            self.emit(f"{else_label}:")
            if stmt.else_stmt is not None:
                self.compile_stmt(stmt.else_stmt)
                self.emit(f"{end_label}:")
        elif isinstance(stmt, While):
            start = self.label("while")
            end = self.label("endwhile")
            self.emit(f"{start}:")
            self.branch_false(stmt.cond, end)
            self.break_stack.append(end)
            self.continue_stack.append(start)
            self.compile_stmt(stmt.body)
            self.continue_stack.pop()
            self.break_stack.pop()
            self.emit(f"  JMP {start}")
            self.emit(f"{end}:")
        elif isinstance(stmt, For):
            if self.try_unroll_for(stmt):
                return
            if stmt.init is not None:
                self.compile_stmt(stmt.init)
            start = self.label("for")
            post = self.label("forpost")
            end = self.label("endfor")
            self.emit(f"{start}:")
            if stmt.cond is not None:
                self.branch_false(stmt.cond, end)
            self.break_stack.append(end)
            self.continue_stack.append(post)
            self.compile_stmt(stmt.body)
            self.continue_stack.pop()
            self.break_stack.pop()
            self.emit(f"{post}:")
            if stmt.post is not None:
                self.compile_stmt(stmt.post)
            self.emit(f"  JMP {start}")
            self.emit(f"{end}:")
        elif isinstance(stmt, Break):
            if not self.break_stack:
                raise CompileError("break outside loop")
            self.emit(f"  JMP {self.break_stack[-1]}")
        elif isinstance(stmt, Continue):
            if not self.continue_stack:
                raise CompileError("continue outside loop")
            self.emit(f"  JMP {self.continue_stack[-1]}")
        elif isinstance(stmt, Return):
            if stmt.expr is not None:
                reg = self.expr(stmt.expr)
                self.free(reg)
            self.emit("  JMP __halt")
        else:
            raise CompileError(f"unsupported statement {stmt!r}")

    def declare(self, stmt: Decl) -> None:
        if stmt.name in self.vars:
            raise CompileError(f"duplicate variable {stmt.name}")
        if stmt.size is not None:
            base = len(self.dmem)
            values = list(stmt.init or [])
            if len(values) > stmt.size:
                raise CompileError(f"too many initializers for {stmt.name}")
            values.extend([0] * (stmt.size - len(values)))
            self.dmem.extend(values)
            self.vars[stmt.name] = VarInfo(base_word=base, size=stmt.size)
            return
        reg = self.alloc_reg()
        self.vars[stmt.name] = VarInfo(reg=reg)
        if stmt.init is None:
            self.load_const(reg, 0)
        else:
            value = self.expr(stmt.init)
            self.emit(f"  ADD {self.reg(reg)}, {self.reg(value)}, r0")
            self.free(value)

    def lookup(self, name: str) -> VarInfo:
        if name not in self.vars:
            raise CompileError(f"unknown variable {name}")
        return self.vars[name]

    def const_eval(self, expr: object) -> int | None:
        if isinstance(expr, Num):
            return expr.value
        if isinstance(expr, Var) and expr.name in self.const_env:
            return self.const_env[expr.name]
        if isinstance(expr, Unary) and expr.op == "-":
            value = self.const_eval(expr.expr)
            return None if value is None else -value
        if isinstance(expr, Bin):
            left = self.const_eval(expr.left)
            right = self.const_eval(expr.right)
            if left is None or right is None:
                return None
            return {
                "+": left + right,
                "-": left - right,
                "*": left * right,
                "<<": left << right,
                ">>": left >> right,
                "&": left & right,
                "^": left ^ right,
                "|": left | right,
            }.get(expr.op)
        return None

    def has_loop_control(self, stmt: object) -> bool:
        if isinstance(stmt, (Break, Continue)):
            return True
        if isinstance(stmt, Block):
            return any(self.has_loop_control(item) for item in stmt.statements)
        if isinstance(stmt, If):
            return self.has_loop_control(stmt.then_stmt) or (stmt.else_stmt is not None and self.has_loop_control(stmt.else_stmt))
        if isinstance(stmt, While):
            return True
        if isinstance(stmt, For):
            return True
        return False

    def match_simple_for(self, stmt: For) -> tuple[str, int, int, int] | None:
        if not isinstance(stmt.init, Assign) or not isinstance(stmt.init.target, Var):
            return None
        name = stmt.init.target.name
        start = self.const_eval(stmt.init.expr)
        if start is None:
            return None
        if not isinstance(stmt.cond, Bin) or stmt.cond.op != "<" or not isinstance(stmt.cond.left, Var) or stmt.cond.left.name != name:
            return None
        end = self.const_eval(stmt.cond.right)
        if end is None:
            return None
        if not isinstance(stmt.post, Assign) or not isinstance(stmt.post.target, Var) or stmt.post.target.name != name:
            return None
        post = stmt.post.expr
        if not (isinstance(post, Bin) and post.op == "+" and isinstance(post.left, Var) and post.left.name == name):
            return None
        step = self.const_eval(post.right)
        if step is None or step <= 0:
            return None
        return name, start, end, step

    def try_unroll_for(self, stmt: For) -> bool:
        match = self.match_simple_for(stmt)
        if match is None or self.has_loop_control(stmt.body):
            return False
        name, start, end, step = match
        count = max(0, (end - start + step - 1) // step)
        if count > 256:
            return False
        old = self.const_env.get(name)
        had_old = name in self.const_env
        for value in range(start, end, step):
            self.const_env[name] = value
            self.compile_stmt(stmt.body)
        if had_old:
            self.const_env[name] = old
        else:
            self.const_env.pop(name, None)
        return True

    def assign(self, target: object, expr: object) -> None:
        value = self.expr(expr)
        if isinstance(target, Var):
            info = self.lookup(target.name)
            if info.reg is None:
                raise CompileError(f"cannot assign whole array {target.name}")
            self.emit(f"  ADD {self.reg(info.reg)}, {self.reg(value)}, r0")
            self.free(value)
            return
        if isinstance(target, ArrayRef):
            info = self.lookup(target.name)
            if info.base_word is None:
                raise CompileError(f"{target.name} is not an array")
            offset_reg = self.array_offset_reg(info, target.index)
            if offset_reg is None:
                const_index = self.const_eval(target.index)
                assert const_index is not None
                self.emit(f"  STRP {self.reg(value)}, [r0 + {(info.base_word + const_index) * 4}]")
            else:
                self.emit(f"  STRP {self.reg(value)}, [r0 + {self.reg(offset_reg)}]")
                self.free(offset_reg)
            self.free(value)
            return
        raise CompileError("bad assignment target")

    def array_offset_reg(self, info: VarInfo, index: object) -> int | None:
        const_index = self.const_eval(index)
        if const_index is not None:
            if not 0 <= const_index < (info.size or 0):
                raise CompileError("constant array index out of range")
            return None
        idx = self.expr(index)
        two = self.tmp()
        self.load_const(two, 2)
        self.emit(f"  SHL {self.reg(idx)}, {self.reg(idx)}, {self.reg(two)}")
        self.free(two)
        if info.base_word:
            base = self.tmp()
            self.load_const(base, info.base_word * 4)
            self.emit(f"  ADD {self.reg(idx)}, {self.reg(idx)}, {self.reg(base)}")
            self.free(base)
        return idx

    def expr(self, expr: object) -> int:
        if isinstance(expr, Num):
            reg = self.tmp()
            self.load_const(reg, expr.value)
            return reg
        if isinstance(expr, Var):
            if expr.name in self.const_env:
                reg = self.tmp()
                self.load_const(reg, self.const_env[expr.name])
                return reg
            info = self.lookup(expr.name)
            if info.reg is None:
                raise CompileError(f"array {expr.name} used as scalar")
            reg = self.tmp()
            self.emit(f"  ADD {self.reg(reg)}, {self.reg(info.reg)}, r0")
            return reg
        if isinstance(expr, ArrayRef):
            info = self.lookup(expr.name)
            if info.base_word is None:
                raise CompileError(f"{expr.name} is not an array")
            dst = self.tmp()
            offset_reg = self.array_offset_reg(info, expr.index)
            if offset_reg is None:
                const_index = self.const_eval(expr.index)
                assert const_index is not None
                self.emit(f"  LDRP {self.reg(dst)}, [r0 + {(info.base_word + const_index) * 4}]")
            else:
                self.emit(f"  LDRP {self.reg(dst)}, [r0 + {self.reg(offset_reg)}]")
                self.free(offset_reg)
            return dst
        if isinstance(expr, Unary):
            value = self.expr(expr.expr)
            if expr.op != "-":
                raise CompileError(f"unsupported unary operator {expr.op}")
            minus_one = self.tmp()
            one = self.tmp()
            self.load_const(minus_one, -1)
            self.emit(f"  XOR {self.reg(value)}, {self.reg(value)}, {self.reg(minus_one)}")
            self.load_const(one, 1)
            self.emit(f"  ADD {self.reg(value)}, {self.reg(value)}, {self.reg(one)}")
            self.free(one)
            self.free(minus_one)
            return value
        if isinstance(expr, Bin):
            if expr.op in ("==", "!=", "<", "<=", ">", ">="):
                return self.compare_value(expr)
            left = self.expr(expr.left)
            right = self.expr(expr.right)
            op_map = {"+": "ADD", "*": "MUL", "&": "AND", "|": "OR", "^": "XOR", "<<": "SHL", ">>": "SHR"}
            if expr.op == "-":
                minus_one = self.tmp()
                one = self.tmp()
                self.load_const(minus_one, -1)
                self.emit(f"  XOR {self.reg(right)}, {self.reg(right)}, {self.reg(minus_one)}")
                self.load_const(one, 1)
                self.emit(f"  ADD {self.reg(right)}, {self.reg(right)}, {self.reg(one)}")
                self.emit(f"  ADD {self.reg(left)}, {self.reg(left)}, {self.reg(right)}")
                self.free(one)
                self.free(minus_one)
            elif expr.op in op_map:
                self.emit(f"  {op_map[expr.op]} {self.reg(left)}, {self.reg(left)}, {self.reg(right)}")
            else:
                raise CompileError(f"unsupported binary operator {expr.op}")
            self.free(right)
            return left
        raise CompileError(f"unsupported expression {expr!r}")

    def compare_value(self, expr: Bin) -> int:
        result = self.tmp()
        true_label = self.label("true")
        end_label = self.label("endcmp")
        self.load_const(result, 0)
        self.branch_true(expr, true_label)
        self.emit(f"  JMP {end_label}")
        self.emit(f"{true_label}:")
        self.load_const(result, 1)
        self.emit(f"{end_label}:")
        return result

    def branch_true(self, expr: object, label: str) -> None:
        false = self.label("skiptrue")
        self.branch_false(expr, false)
        self.emit(f"  JMP {label}")
        self.emit(f"{false}:")

    def branch_false(self, expr: object, label: str) -> None:
        if isinstance(expr, Bin) and expr.op in ("==", "!=", "<", "<=", ">", ">="):
            left = self.expr(expr.left)
            right = self.expr(expr.right)
            if expr.op in (">", "<="):
                self.emit(f"  CMP r0, {self.reg(right)}, {self.reg(left)}")
                self.emit(f"  {'JMPNC' if expr.op == '>' else 'JMPC'} {label}")
            else:
                self.emit(f"  CMP r0, {self.reg(left)}, {self.reg(right)}")
                branch = {
                    "==": "JMPNZ",
                    "!=": "JMPZ",
                    "<": "JMPNC",
                    ">=": "JMPC",
                }[expr.op]
                self.emit(f"  {branch} {label}")
            self.free(right)
            self.free(left)
            return
        value = self.expr(expr)
        self.emit(f"  CMP r0, {self.reg(value)}, r0")
        self.emit(f"  JMPZ {label}")
        self.free(value)


def compile_text(text: str, schedule: str = "compact") -> str:
    ast = Parser(tokenize(text)).parse_program()
    return CodeGen(schedule=schedule).compile(ast)


def compile_file(path: Path, schedule: str = "compact") -> str:
    return compile_text(path.read_text(), schedule=schedule)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Compile tiny C subset to qrisc32 assembly/hex.")
    parser.add_argument("input", type=Path)
    parser.add_argument("-S", action="store_true", help="Keep assembly output.")
    parser.add_argument("-o", "--output", type=Path, help="Assembly output path.")
    parser.add_argument("--hex", type=Path, help="Optional instruction hex output path.")
    parser.add_argument("--dmem", type=Path, help="Optional data-memory hex output path.")
    parser.add_argument(
        "--schedule",
        choices=("compact", "safe", "legacy", "none"),
        default="compact",
        help="Instruction scheduling mode. legacy keeps the old six-NOP padding.",
    )
    args = parser.parse_args(argv)

    asm_text = compile_file(args.input, schedule=args.schedule)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(asm_text)
    elif args.S or args.hex is None:
        print(asm_text, end="")

    if args.hex is not None:
        from .asm import assemble_text
        from .encoder import write_hex

        program = assemble_text(asm_text, args.input.with_suffix(".s"))
        write_hex(args.hex, program.imem)
        if args.dmem is not None:
            write_hex(args.dmem, program.dmem)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
