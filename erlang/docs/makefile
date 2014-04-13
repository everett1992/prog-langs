BUILD_DIR=build
TEX_FILE=writeup

all: $(TEX_FILE).pdf

$(TEX_FILE).pdf: $(BUILD_DIR) $(TEX_FILE).tex homework.sty
	@echo "Compiling $(TEX_FILE).tex with rubber"
	@rubber  --pdf --into $(BUILD_DIR) $(TEX_FILE).tex
	@cp $(BUILD_DIR)/$(TEX_FILE).pdf .

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

