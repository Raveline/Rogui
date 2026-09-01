.PHONY: all build build-sdl build-wasm build-wasm-demo serve-wasm-demo \
        build-wasm-list-demo serve-wasm-list-demo build-lib \
        build-backends build-demos run-demo docs docs-open test test-browser \
        lint clean clean-all help

# Path to the ghc-wasm-meta env script that puts wasm32-wasi-ghc/-cabal on
# PATH. 
# Override on the command line (`make build-wasm GHC_WASM_ENV=/path/to/env`)
# if yours isn't at the default ghc-wasm-meta location.
GHC_WASM_ENV ?= $(HOME)/.ghc-wasm/env

NPM ?= npm

# Default target: build native (SDL) backend
all: build-sdl

# Build everything with native GHC (SDL backend + demos)
build-sdl:
	cabal build all

# Build core library only
build-lib:
	cabal build rogui

# Build all backends
build-backends:
	@echo "Building SDL backend with GHC..."
	cabal build rogui-sdl-backend

# Build demos (SDL only)
build-demos:
	cabal build rogui-demos

# Build the HTML5/wasm backend (requires wasm32-wasi-cabal from ghc-wasm-meta;
# see GHC_WASM_ENV above if it's not installed at the default location)
build-wasm:
	@test -f "$(GHC_WASM_ENV)" || { echo "GHC_WASM_ENV not found at $(GHC_WASM_ENV) -- install ghc-wasm-meta, or pass GHC_WASM_ENV=/path/to/env"; exit 1; }
	. $(GHC_WASM_ENV) && wasm32-wasi-cabal build --project-file=cabal.project.wasm all

# Vendor @bjorn3/browser_wasi_shim into a demo directory. Real file targets
# (not .PHONY): npm runs only when node_modules is missing or package.json
# is newer, so a normal build doesn't reinstall every time.
rogui-wasm-backend/app/node_modules: rogui-wasm-backend/app/package.json
	cd rogui-wasm-backend/app && $(NPM) install
	@touch $@

rogui-wasm-backend/app-list/node_modules: rogui-wasm-backend/app-list/package.json
	cd rogui-wasm-backend/app-list && $(NPM) install
	@touch $@

# Build the WASM demo and stage it, ready to serve, next to app/index.html.
build-wasm-demo: rogui-wasm-backend/app/node_modules
	@test -f "$(GHC_WASM_ENV)" || { echo "GHC_WASM_ENV not found at $(GHC_WASM_ENV) -- install ghc-wasm-meta, or pass GHC_WASM_ENV=/path/to/env"; exit 1; }
	. $(GHC_WASM_ENV) && wasm32-wasi-cabal build --project-file=cabal.project.wasm rogui-wasm-demo
	. $(GHC_WASM_ENV) && cp "$$(wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm rogui-wasm-demo)" \
		rogui-wasm-backend/app/rogui-wasm-demo.wasm
	. $(GHC_WASM_ENV) && node "$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
		-i rogui-wasm-backend/app/rogui-wasm-demo.wasm \
		-o rogui-wasm-backend/app/rogui-wasm-demo.jsffi.js
	cp rogui-wasm-backend/jsbits/rogui-runtime.js rogui-wasm-backend/app/rogui-runtime.js
	@echo "Staged in rogui-wasm-backend/app/. Run 'make serve-wasm-demo' (or serve that directory yourself) and open index.html."

# Serve the staged WASM demo directory over HTTP (fetch() of the tileset
# PNG needs a real origin, file:// won't work). 
serve-wasm-demo: rogui-wasm-backend/app/node_modules
	cd rogui-wasm-backend/app && python3 -m http.server 8000

# Same as build-wasm-demo, for the second, interactive demo (keyboard
# navigation, mouse clicks, window resize -- see app-list/Main.hs).
build-wasm-list-demo: rogui-wasm-backend/app-list/node_modules
	@test -f "$(GHC_WASM_ENV)" || { echo "GHC_WASM_ENV not found at $(GHC_WASM_ENV) -- install ghc-wasm-meta, or pass GHC_WASM_ENV=/path/to/env"; exit 1; }
	. $(GHC_WASM_ENV) && wasm32-wasi-cabal build --project-file=cabal.project.wasm rogui-wasm-list-demo
	. $(GHC_WASM_ENV) && cp "$$(wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm rogui-wasm-list-demo)" \
		rogui-wasm-backend/app-list/rogui-wasm-list-demo.wasm
	. $(GHC_WASM_ENV) && node "$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
		-i rogui-wasm-backend/app-list/rogui-wasm-list-demo.wasm \
		-o rogui-wasm-backend/app-list/rogui-wasm-list-demo.jsffi.js
	cp rogui-wasm-backend/jsbits/rogui-runtime.js rogui-wasm-backend/app-list/rogui-runtime.js
	@echo "Staged in rogui-wasm-backend/app-list/. Run 'make serve-wasm-list-demo' and open index.html."

# Serve the staged interactive WASM demo directory over HTTP. See
# serve-wasm-demo above re: the node_modules prerequisite.
serve-wasm-list-demo: rogui-wasm-backend/app-list/node_modules
	cd rogui-wasm-backend/app-list && python3 -m http.server 8001

# Generate Haddock documentation
docs:
	cabal haddock all --haddock-html --haddock-hyperlink-source

# Open documentation in browser
docs-open: docs
	@echo "Opening documentation in browser..."
	@xdg-open dist-newstyle/build/x86_64-linux/ghc-*/rogui-*/doc/html/rogui/index.html 2>/dev/null || \
	 open dist-newstyle/build/x86_64-linux/ghc-*/rogui-*/doc/html/rogui/index.html 2>/dev/null || \
	 echo "Could not open browser automatically. Documentation is at: dist-newstyle/build/.../doc/html/rogui/index.html"

# Run tests
test:
	cabal test all

# npm install for the headless-browser checks. Real file target, like the
# demo node_modules above: reruns only when package.json changes.
rogui-wasm-backend/test-browser/node_modules: rogui-wasm-backend/test-browser/package.json
	cd rogui-wasm-backend/test-browser && $(NPM) install
	@touch $@

# Run the headless-browser checks (rogui-wasm-backend/test-browser/): build
# and stage both WASM demos, serve each on a local port, drive it with a
# real browser via playwright-core, then tear the servers down. Needs
# Google Chrome or Chromium on PATH (playwright-core ships no browser of
# its own); override with ROGUI_TEST_CHROME=/path/to/chromium if neither
# is found. Ports 8100/8101 (not the serve-* targets' 8000/8001) so it
# doesn't collide with a demo you're already serving by hand.
test-browser: build-wasm-demo build-wasm-list-demo rogui-wasm-backend/test-browser/node_modules
	@set -e; \
	python3 -m http.server 8100 --bind 127.0.0.1 -d rogui-wasm-backend/app      >/dev/null 2>&1 & p1=$$!; \
	python3 -m http.server 8101 --bind 127.0.0.1 -d rogui-wasm-backend/app-list >/dev/null 2>&1 & p2=$$!; \
	trap 'kill $$p1 $$p2 2>/dev/null || true' EXIT; \
	for port in 8100 8101; do \
	  ok=; \
	  for _ in $$(seq 1 50); do \
	    curl -sf -o /dev/null "http://127.0.0.1:$$port/index.html" && { ok=1; break; }; \
	    sleep 0.1; \
	  done; \
	  test -n "$$ok" || { echo "server on port $$port never came up"; exit 1; }; \
	done; \
	cd rogui-wasm-backend/test-browser; \
	node smoke-hello.mjs      http://127.0.0.1:8100/index.html; \
	node interaction-list.mjs http://127.0.0.1:8101/index.html

# Run hlint on source files
lint:
	@echo "Linting core library..."
	@hlint rogui/src/ || true
	@echo "Linting SDL backend..."
	@hlint rogui-sdl-backend/src/ || true
	@echo "Linting demos..."
	@hlint rogui-demos/demos/ || true

# Clean build artifacts (keeps package store)
clean:
	cabal clean

# Clean everything, including staged WASM demo artifacts and vendored
# node_modules.
clean-all:
	cabal clean
	rm -rf dist-newstyle
	rm -rf rogui-wasm-backend/app/node_modules rogui-wasm-backend/app-list/node_modules \
	       rogui-wasm-backend/test-browser/node_modules
	rm -f rogui-wasm-backend/app/rogui-wasm-demo.wasm \
	      rogui-wasm-backend/app/rogui-wasm-demo.jsffi.js \
	      rogui-wasm-backend/app/rogui-runtime.js \
	      rogui-wasm-backend/app-list/rogui-wasm-list-demo.wasm \
	      rogui-wasm-backend/app-list/rogui-wasm-list-demo.jsffi.js \
	      rogui-wasm-backend/app-list/rogui-runtime.js

# Show help
help:
	@echo "RoGUI Makefile targets:"
	@echo ""
	@echo "Building:"
	@echo "  make build-sdl      - Build with native GHC (SDL backend + demos) [default]"
	@echo "  make build-lib      - Build core library only"
	@echo "  make build-demos    - Build demo applications"
	@echo ""
	@echo "Running:"
	@echo "  make run-demo       - Run demo applications"
	@echo ""
	@echo "Documentation:"
	@echo "  make docs           - Generate Haddock documentation"
	@echo "  make docs-open      - Generate and open documentation in browser"
	@echo ""
	@echo "Quality:"
	@echo "  make test           - Run the Haskell test suite (cabal test all)"
	@echo "  make test-browser   - Build the WASM demos and run the headless-browser checks"
	@echo "  make lint           - Run hlint on all source code"
	@echo ""
	@echo "Cleaning:"
	@echo "  make clean          - Remove build artifacts"
	@echo "  make clean-all      - Remove all build artifacts and package store"
	@echo ""
	@echo "Help:"
	@echo "  make help           - Show this help message"
