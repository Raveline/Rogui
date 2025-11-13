.PHONY: all build build-lib build-exe docs docs-open test lint clean help

# Default target
all: build test lint

# Build the library only
build-lib:
	cabal build rogui:lib

# Build all executables
build-exe:
	cabal build rogui:exe:rogui
	cabal build rogui:exe:rogui-list-demo
	cabal build rogui:exe:rogui-grid-demo
	cabal build rogui:exe:rogui-colour-demo

# Build everything (library + executables)
build:
	cabal build all

# Generate Haddock documentation
docs:
	cabal haddock --haddock-html --haddock-hyperlink-source

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
	hlint src/ demos/ rogueharvest/

# Clean build artifacts
clean:
	cabal clean

# Show help
help:
	@echo "RoGUI Makefile targets:"
	@echo "  make build       - Build library and all executables"
	@echo "  make build-lib   - Build library only"
	@echo "  make build-exe   - Build all executables only"
	@echo "  make docs        - Generate Haddock documentation"
	@echo "  make docs-open   - Generate and open documentation in browser"
	@echo "  make test        - Run test suite"
	@echo "  make lint        - Run hlint on source code"
	@echo "  make clean       - Remove build artifacts"
	@echo "  make all         - Build, test, and lint (default)"
	@echo "  make help        - Show this help message"
