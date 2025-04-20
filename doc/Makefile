LATEX ?= pdflatex
BUILD_DIR ?= build/risc_present
TEX := risc_present.tex
PDF := $(BUILD_DIR)/risc_present.pdf

.PHONY: all pdf clean distclean

all: pdf

pdf: $(PDF)

$(PDF): $(TEX) risc_present_assets/if_stage.jpg | $(BUILD_DIR)
	$(LATEX) -interaction=nonstopmode -halt-on-error -output-directory $(BUILD_DIR) $(TEX)
	$(LATEX) -interaction=nonstopmode -halt-on-error -output-directory $(BUILD_DIR) $(TEX)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

distclean: clean
