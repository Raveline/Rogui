.PHONY: all build build-sdl build-canvas build-lib build-backends build-demos \
        run-demo docs docs-open test lint clean clean-all help

# Default target: build native (SDL) backend
all: build-sdl

# Build everything with native GHC (SDL backend + demos)
build-sdl:
	cabal build all

# Build core library only
build-lib:
	cabal build rogui

# Build all backends (SDL with GHC, then Canvas with GHCJS)
build-backends:
	@echo "Building SDL backend with GHC..."
	cabal build rogui-sdl-backend

# Build demos (SDL only)
build-demos:
	cabal build rogui-demos

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

# Clean everything
clean-all:
	cabal clean
	rm -rf dist-newstyle

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
