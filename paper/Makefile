BUILD_DIR=build

NAME=main
TEXINPUT=$(NAME).tex
PDFOUTPUT=$(BUILD_DIR)/$(NAME).pdf
FINALPDF=$(NAME).pdf
BIBFILE=$(NAME).bib

TEX=latexmk
TEXFLAGS=-pdf -outdir=$(BUILD_DIR)

.PHONY: all clean

all: $(FINALPDF)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(PDFOUTPUT): $(TEXINPUT) $(BIBFILE) | $(BUILD_DIR)
	$(TEX) $(TEXFLAGS) $(TEXINPUT)

$(FINALPDF): $(PDFOUTPUT)
	cp $< $@

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(FINALPDF)
