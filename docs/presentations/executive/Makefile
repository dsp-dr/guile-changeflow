# Makefile for Executive Presentation
# Supports multiple output formats from org-mode source

PRESENTATION := executive
SOURCE := $(PRESENTATION).org
OUTPUTS := $(PRESENTATION).html $(PRESENTATION).pdf $(PRESENTATION)-reveal.html $(PRESENTATION)-slides.html

.PHONY: all clean html pdf slides reveal-slides help

all: html pdf slides ## Build all presentation formats

html: $(PRESENTATION).html ## Generate HTML version

pdf: $(PRESENTATION).pdf ## Generate PDF version

slides: $(PRESENTATION)-slides.html ## Generate HTML slides

reveal-slides: $(PRESENTATION)-reveal.html ## Generate Reveal.js slides

$(PRESENTATION).html: $(SOURCE)
	@echo "🔨 Building HTML presentation..."
	emacs --batch --eval "(require 'ox-html)" $(SOURCE) -f org-html-export-to-html

$(PRESENTATION).pdf: $(SOURCE)
	@echo "🔨 Building PDF presentation..."
	emacs --batch --eval "(require 'ox-latex)" $(SOURCE) -f org-latex-export-to-pdf

$(PRESENTATION)-slides.html: $(SOURCE)
	@echo "🔨 Building HTML slides..."
	emacs --batch --eval "(require 'ox-html)" --eval "(setq org-html-divs '((preamble \"header\" \"preamble\") (content \"main\" \"content\") (postamble \"footer\" \"postamble\")))" $(SOURCE) --eval "(org-export-to-file 'html \"$@\")"

$(PRESENTATION)-reveal.html: $(SOURCE)
	@echo "🔨 Building Reveal.js slides..."
	emacs --batch --eval "(require 'ox-reveal)" $(SOURCE) --eval "(org-export-to-file 'reveal \"$@\")"

preview: $(PRESENTATION).html ## Preview HTML version in browser
	@echo "🌐 Opening presentation in browser..."
	@if command -v xdg-open > /dev/null; then \
		xdg-open $(PRESENTATION).html; \
	elif command -v open > /dev/null; then \
		open $(PRESENTATION).html; \
	else \
		echo "Please open $(PRESENTATION).html manually"; \
	fi

serve: $(PRESENTATION)-reveal.html ## Serve Reveal.js presentation locally
	@echo "🚀 Starting local server for Reveal.js presentation..."
	@if command -v python3 > /dev/null; then \
		python3 -m http.server 8000; \
	elif command -v python > /dev/null; then \
		python -m SimpleHTTPServer 8000; \
	else \
		echo "Python not found. Please serve $(PRESENTATION)-reveal.html manually"; \
	fi

validate: $(SOURCE) ## Validate org-mode syntax
	@echo "✅ Validating org-mode syntax..."
	@emacs --batch $(SOURCE) --eval "(org-mode)" --eval "(org-lint)"

stats: $(SOURCE) ## Show presentation statistics
	@echo "📊 Presentation Statistics:"
	@echo "  Lines: $$(wc -l < $(SOURCE))"
	@echo "  Words: $$(wc -w < $(SOURCE))"
	@echo "  Sections: $$(grep -c '^*' $(SOURCE))"
	@echo "  Subsections: $$(grep -c '^**' $(SOURCE))"

clean: ## Remove generated files
	@echo "🧹 Cleaning generated files..."
	@rm -f $(OUTPUTS) $(PRESENTATION).tex $(PRESENTATION).aux $(PRESENTATION).log

help: ## Show this help
	@echo "📋 Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

# Ensure required tools are available
check-deps: ## Check for required dependencies
	@echo "🔍 Checking dependencies..."
	@command -v emacs >/dev/null 2>&1 || { echo "❌ Emacs not found"; exit 1; }
	@echo "✅ Emacs found"
	@emacs --batch --eval "(require 'ox-html)" 2>/dev/null || echo "⚠️  org-html export not available"
	@emacs --batch --eval "(require 'ox-latex)" 2>/dev/null || echo "⚠️  org-latex export not available"
	@emacs --batch --eval "(require 'ox-reveal)" 2>/dev/null || echo "⚠️  org-reveal export not available (install ox-reveal package)"

# Development helpers
dev-watch: ## Watch for changes and rebuild HTML
	@echo "👁️  Watching for changes..."
	@while true; do \
		inotifywait -e modify $(SOURCE) 2>/dev/null && $(MAKE) html; \
	done

.DEFAULT_GOAL := help