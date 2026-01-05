MAKEFLAGS += -j
OUT := aiken-wasm

.PHONY: all browser vanilla docs preview-docs clean

all: browser vanilla

browser: $(OUT)-browser/$(OUT).d.ts

vanilla: $(OUT)-vanilla/$(OUT).d.ts

docs:
	@npx typedoc

preview-docs:
	@npx serve docs

clean:
	@echo "🧹 Cleaning..."
	rm -rf $(OUT)-browser $(OUT)-vanilla

node_modules/typedoc-theme-oxide:
	@npm i

$(OUT)-browser/$(OUT).d.ts:
	@echo "📦 Building WASM (browser)..."
	wasm-pack build --release --target browser -d $(OUT)-browser

$(OUT)-vanilla/$(OUT).d.ts:
	@echo "📦 Building WASM (vanilla)..."
	wasm-pack build --release --target no-modules -d $(OUT)-vanilla

