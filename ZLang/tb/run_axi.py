"""Check the ZLang native-JIT AXI protocol against delayed memories."""

from __future__ import annotations

import json

import zlang

from run_vectors import ROOT, VCD_DIR, words


SOURCE = ROOT / "ZLang" / "src" / "Qrisc32.zhl"
ADDRESS_FIELDS = (
    "id", "addr", "len", "size", "burst", "lock", "cache", "prot", "qos",
    "region", "user",
)
ADDRESS_CONSTANTS = {
    "id": 0, "len": 0, "size": 2, "burst": 1, "lock": 0, "cache": 0,
    "prot": 0, "qos": 0, "region": 0, "user": 0,
}
CHANNELS = {
    "i_ar": ("instruction_axi__ar", ADDRESS_FIELDS),
    "d_ar": ("data_axi__ar", ADDRESS_FIELDS),
    "d_aw": ("data_axi__aw", ADDRESS_FIELDS),
    "d_w": ("data_axi__w", ("data", "strb", "last", "user")),
}


def main() -> None:
    manifest = json.loads((ROOT / "tests" / "vectors.json").read_text())
    vector = next(item for item in manifest["vectors"] if item["name"] == "axi")
    imem = words(ROOT / "tests" / vector["hex"])
    dmem: list[int] = []
    VCD_DIR.mkdir(parents=True, exist_ok=True)
    program = zlang.sim.compile(SOURCE, top="Qrisc32Axi", engine="native")
    pending_i = None
    pending_d = None
    pending_b = None
    aw_addr = None
    w_data = None
    stalled = {}
    terminal = 0
    overlap = 0
    aw_seen = False
    counts = {name: 0 for name in CHANNELS}
    stalls = {name: 0 for name in CHANNELS}
    waves = (
        "core_pc", "core_id_valid", "core_ex_valid", "core_mem_valid",
        "core_wb_valid", "i_ar", "i_r", "d_ar", "d_r", "d_aw", "d_w", "d_b",
        "core_stall",
    )
    vcd_path = VCD_DIR / "axi.vcd"

    with program.create() as sim, vcd_path.open("w", encoding="ascii") as vcd:
        vcd.write("$date qrisc32 native AXI JIT $end\n$version ZLang $end\n")
        vcd.write("$timescale 1ns $end\n$scope module qrisc32_axi $end\n")
        ids = {name: chr(33 + i) for i, name in enumerate(waves)}
        for name in waves:
            vcd.write(f"$var wire {32 if name == 'core_pc' else 1} {ids[name]} {name} $end\n")
        vcd.write("$upscope $end\n$enddefinitions $end\n")
        sim.set("enable", 1)
        for cycle in range(10000):
            ready = {
                "i_ar": int(cycle % 5 in (1, 2)),
                "d_ar": int(cycle % 7 in (2, 3)),
                "d_aw": int(aw_seen and cycle % 5 in (0, 3)),
                "d_w": int(cycle % 7 in (1, 4)),
            }
            for name, (channel, _) in CHANNELS.items():
                sim.set(channel, {"ready": ready[name]})
            i_rvalid = int(pending_i is not None and cycle >= pending_i[1])
            d_rvalid = int(pending_d is not None and cycle >= pending_d[1])
            bvalid = int(pending_b is not None and cycle >= pending_b)
            sim.set("instruction_axi__r", {
                "valid": i_rvalid,
                "payload": {
                    "id": 0, "data": pending_i[2] if i_rvalid else 0,
                    "last": 1, "resp": 0, "user": 0,
                },
            })
            sim.set("data_axi__r", {
                "valid": d_rvalid,
                "payload": {
                    "id": 0, "data": pending_d[2] if d_rvalid else 0,
                    "last": 1, "resp": 0, "user": 0,
                },
            })
            sim.set("data_axi__b", {
                "valid": bvalid, "payload": {"id": 0, "resp": 0, "user": 0},
            })
            out = sim.eval()

            for name, (channel, fields) in CHANNELS.items():
                value = out[channel]
                valid = int(value["valid"])
                payload = tuple(int(value["payload"][field]) for field in fields)
                if name in stalled and (not valid or payload != stalled[name]):
                    raise AssertionError(f"{name}: payload changed under backpressure")
                stalled.pop(name, None)
                if valid and not ready[name]:
                    stalled[name] = payload
                    stalls[name] += 1
                if valid:
                    if name == "d_w":
                        if any(int(value["payload"][field]) != expected for field, expected in
                               (("strb", 15), ("last", 1), ("user", 0))):
                            raise AssertionError("d_w: invalid full-word write payload")
                    else:
                        address = int(value["payload"]["addr"])
                        if address & 3 or any(
                            int(value["payload"][field]) != expected
                            for field, expected in ADDRESS_CONSTANTS.items()
                        ):
                            raise AssertionError(f"{name}: invalid single-beat AXI4 address")
                if valid and ready[name]:
                    counts[name] += 1
            if out["data_axi__aw"]["valid"]:
                aw_seen = True

            vcd.write(f"#{cycle * 10}\n")
            for name in waves:
                if name in CHANNELS:
                    value = int(out[CHANNELS[name][0]]["valid"])
                elif name == "i_r":
                    value = i_rvalid
                elif name == "d_r":
                    value = d_rvalid
                elif name == "d_b":
                    value = bvalid
                else:
                    value = int(out[name])
                width = 32 if name == "core_pc" else 1
                vcd.write(f"b{value:0{width}b} {ids[name]}\n")

            if out["instruction_axi__ar"]["transfer"]:
                if pending_i is not None:
                    raise AssertionError("second instruction read before response")
                addr = int(out["instruction_axi__ar"]["payload"]["addr"])
                index = addr >> 2
                pending_i = (addr, cycle + 2 + cycle % 2,
                             imem[index] if index < len(imem) else 0)
            if out["instruction_axi__r"]["transfer"]:
                pending_i = None
            if out["data_axi__ar"]["transfer"]:
                if pending_d is not None:
                    raise AssertionError("second data read before response")
                index = int(out["data_axi__ar"]["payload"]["addr"]) >> 2
                pending_d = (index, cycle + 3 + cycle % 2,
                             dmem[index] if index < len(dmem) else 0)
            if out["data_axi__r"]["transfer"]:
                pending_d = None
            if out["data_axi__aw"]["transfer"]:
                if aw_addr is not None:
                    raise AssertionError("second AW before write response")
                aw_addr = int(out["data_axi__aw"]["payload"]["addr"]) >> 2
            if out["data_axi__w"]["transfer"]:
                if w_data is not None:
                    raise AssertionError("second W before write response")
                w_data = int(out["data_axi__w"]["payload"]["data"])
            if aw_addr is not None and w_data is not None and pending_b is None:
                if aw_addr >= 4096:
                    raise AssertionError("data address outside bench memory")
                if aw_addr >= len(dmem):
                    dmem.extend([0] * (aw_addr + 1 - len(dmem)))
                dmem[aw_addr] = w_data
                pending_b = cycle + 3
            if out["data_axi__b"]["transfer"]:
                pending_b = None
                aw_addr = None
                w_data = None
            if out["bus_error"]:
                raise AssertionError("AXI response error")
            if all(out[f"core_{stage}_valid"] for stage in ("id", "ex", "mem", "wb")):
                overlap += 1
            if out["core_terminal_jump"]:
                terminal += 1
            sim.edge("clk")
            if terminal >= 3 and pending_b is None:
                break
        else:
            raise AssertionError("AXI bench timed out")

    if not all(counts.values()):
        raise AssertionError(f"missing AXI channel traffic: {counts}")
    if not all(stalls.values()):
        raise AssertionError(f"missing AXI backpressure: {stalls}")
    if overlap == 0:
        raise AssertionError("core never held simultaneous ID/EX/MEM/WB instructions")
    for key, expected in vector["expected_dmem"].items():
        index = int(key)
        actual = dmem[index] if index < len(dmem) else 0
        if actual != expected:
            raise AssertionError(f"dmem[{index}]={actual}, expected {expected}")
    print(vector["pass"])


if __name__ == "__main__":
    main()
