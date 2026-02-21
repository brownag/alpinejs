.PHONY: help install test check docs clean clean-docs docs-serve all dev

# Colors for terminal output
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

help:
	@echo "$(BLUE)alpinejs - Development Workflow$(NC)"
	@echo ""
	@echo "$(GREEN)Documentation:$(NC)"
	@echo "  make docs              Build documentation site (guides, examples, reference)"
	@echo "  make docs-clean        Remove all generated documentation"
	@echo "  make docs-serve        Serve docs locally at http://localhost:8000"
	@echo "  make docs-watch        Build docs and rebuild on file changes"
	@echo ""
	@echo "$(GREEN)Package Development:$(NC)"
	@echo "  make install           Install the package in development mode"
	@echo "  make test              Run test suite (testthat)"
	@echo "  make test-verbose      Run tests with verbose output"
	@echo "  make check             Run R CMD check (full validation)"
	@echo "  make document          Generate NAMESPACE and .Rd files from roxygen2"
	@echo ""
	@echo "$(GREEN)Maintenance:$(NC)"
	@echo "  make clean             Clean build artifacts and temp files"
	@echo "  make all               Full workflow: clean → document → check → test → docs"
	@echo "  make help              Show this help message"
	@echo ""
	@echo "$(YELLOW)Examples:$(NC)"
	@echo "  make docs              # Build and view at localhost:8000"
	@echo "  make check             # Validate package"
	@echo "  make all               # Complete validation and build"

# $(GREEN)Installation & Setup$(NC)
install:
	@echo "$(BLUE)Installing alpinejs...$(NC)"
	@Rscript -e "devtools::load_all()" && echo "$(GREEN)✓ Loaded$(NC)"
	@Rscript -e "devtools::install()" && echo "$(GREEN)✓ Installed$(NC)"

# $(GREEN)Documentation Building$(NC)
docs:
	@echo "$(BLUE)Building documentation site...$(NC)"
	@Rscript build_docs.R
	@echo "$(GREEN)✓ Documentation built$(NC)"
	@echo ""
	@echo "To preview locally, run:"
	@echo "  $(YELLOW)make docs-serve$(NC)"

docs-clean:
	@echo "$(BLUE)Cleaning documentation artifacts...$(NC)"
	@rm -rf docs .docs_build_cache
	@echo "$(GREEN)✓ Documentation cleaned$(NC)"

docs-serve:
	@echo "$(BLUE)Starting local documentation server...$(NC)"
	@echo "$(GREEN)Docs available at http://localhost:8000$(NC)"
	@echo "Press CTRL+C to stop the server"
	@cd docs && python3 -m http.server 8000

docs-watch:
	@echo "$(BLUE)Watching for file changes and rebuilding docs...$(NC)"
	@echo "Press CTRL+C to stop the watcher"
	@while true; do \
		Rscript build_docs.R > /dev/null 2>&1; \
		echo "$(YELLOW)Rebuilt at $$(date '+%H:%M:%S')$(NC)"; \
		inotifywait -e modify -r vignettes/ README.Rmd _site_template.html _site_config.R -q 2>/dev/null || sleep 2; \
	done

# $(GREEN)Testing & Validation$(NC)
test:
	@echo "$(BLUE)Running tests...$(NC)"
	@Rscript -e "devtools::test()" && echo "$(GREEN)✓ Tests passed$(NC)"

test-verbose:
	@echo "$(BLUE)Running tests (verbose)...$(NC)"
	@Rscript -e "devtools::test(reporter = 'progress')"

check:
	@echo "$(BLUE)Running R CMD check...$(NC)"
	@echo "$(YELLOW)This may take 1-2 minutes$(NC)"
	@Rscript -e "devtools::check()" && echo "$(GREEN)✓ Check passed$(NC)"

# Documentation from roxygen2
document:
	@echo "$(BLUE)Generating documentation...$(NC)"
	@Rscript -e "devtools::document()" && echo "$(GREEN)✓ Documentation generated$(NC)"

# $(GREEN)Cleanup$(NC)
clean:
	@echo "$(BLUE)Cleaning up build artifacts...$(NC)"
	@rm -f *.Rcheck && \
	rm -rf Meta && \
	rm -f .Rbuildignore.orig && \
	echo "$(GREEN)✓ Artifacts cleaned$(NC)"

clean-all: clean docs-clean
	@echo "$(GREEN)✓ All cleaned$(NC)"

# $(GREEN)Complete Workflows$(NC)
all: clean document check test docs
	@echo ""
	@echo "$(GREEN)═══════════════════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ All checks passed!$(NC)"
	@echo "$(GREEN)═══════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Documentation available at: $(YELLOW)docs/$(NC)"
	@echo "To preview: $(YELLOW)make docs-serve$(NC)"

dev: document test
	@echo ""
	@echo "$(GREEN)Development workflow complete$(NC)"
	@echo "Run $(YELLOW)make docs$(NC) to build documentation"

# $(GREEN)Utility targets$(NC)
.PHONY: list-exported-functions
list-exported-functions:
	@echo "$(BLUE)Exported functions:$(NC)"
	@grep "^export(" NAMESPACE | sed 's/export(//' | sed 's/)//' | sort

info:
	@echo "$(BLUE)Package Information$(NC)"
	@echo ""
	@Rscript -e "desc::desc()" 2>/dev/null || Rscript -e "cat('Version: '); packageVersion('alpinejs'); cat('​\n')"

# Default target
.DEFAULT_GOAL := help
