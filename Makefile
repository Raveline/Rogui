.PHONY: all build build-sdl build-wasm build-wasm-demo serve-wasm-demo \
        build-wasm-list-demo serve-wasm-list-demo build-lib \
        build-backends build-demos run-demo docs docs-open test lint clean clean-all help

# Path to the ghc-wasm-meta env script that puts wasm32-wasi-ghc/-cabal on
# PATH. Only sourcing it in your interactive shell (as the ghc-wasm-meta
# install instructions tell you to) doesn't help `make`: each recipe line
# below runs in its own fresh, non-interactive subshell that doesn't
# inherit it, so the wasm-* targets source it themselves instead.
# Override on the command line (`make build-wasm GHC_WASM_ENV=/path/to/env`)
# if yours isn't at the default ghc-wasm-meta location.
GHC_WASM_ENV ?= $(HOME)/.ghc-wasm/env

# npm, used to vendor @bjorn3/browser_wasi_shim (the browsers-ship-no-WASI
# shim that the demo index.html files import) into each demo directory's
# node_modules. Override if npm isn't on PATH in make's non-interactive
# shell, e.g. `make build-wasm-list-demo NPM=$(HOME)/.nvm/versions/node/vX/bin/npm`.
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

# Build the WASM demo and stage it, ready to serve, next to app/index.html:
# the compiled .wasm, its post-link.mjs JS FFI glue, and a copy of
# jsbits/rogui-runtime.js. The node_modules prerequisite vendors
# @bjorn3/browser_wasi_shim (index.html's WASI implementation -- browsers
# don't ship one); override NPM if npm isn't on make's PATH.
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
# PNG needs a real origin, file:// won't work). Depends on node_modules so
# `make serve-wasm-demo` on a fresh checkout doesn't 404 on the WASI shim
# import; it does NOT rebuild the .wasm (run build-wasm-demo for that).
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
	rm -rf rogui-wasm-backend/app/node_modules rogui-wasm-backend/app-list/node_modules
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
	@echo "  make test           - Run test suite"
	@echo "  make lint           - Run hlint on all source code"
	@echo ""
	@echo "Cleaning:"
	@echo "  make clean          - Remove build artifacts"
	@echo "  make clean-all      - Remove all build artifacts and package store"
	@echo ""
	@echo "Help:"
	@echo "  make help           - Show this help message"
