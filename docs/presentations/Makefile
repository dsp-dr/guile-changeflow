# Makefile for Why ChangeFlow Presentation
# Builds PDF from org-mode source using Emacs

EMACS = emacs
ORG_FILE = presentation.org
PDF_FILE = why-changeflow-presentation.pdf
TEX_FILE = presentation.tex

.PHONY: all pdf clean setup-beamer

all: pdf

pdf: $(PDF_FILE)

$(PDF_FILE): $(ORG_FILE)
	@echo "Building PDF presentation from org-mode..."
	$(EMACS) --batch \
		--eval "(require 'ox-latex)" \
		--eval "(require 'ox-beamer)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--eval "(find-file \"$(ORG_FILE)\")" \
		--eval "(org-beamer-export-to-pdf)" \
		--kill
	@echo "PDF generated: $(PDF_FILE)"

setup-beamer:
	@echo "Installing required LaTeX packages for beamer..."
	@if command -v tlmgr >/dev/null 2>&1; then \
		tlmgr install beamer metropolis pgfopts; \
	elif command -v pkg >/dev/null 2>&1; then \
		pkg install texlive-full; \
	else \
		echo "Please install LaTeX/beamer manually for your system"; \
	fi

clean:
	rm -f $(PDF_FILE) $(TEX_FILE) *.aux *.log *.nav *.out *.snm *.toc *.vrb

preview: $(PDF_FILE)
	@if command -v xdg-open >/dev/null 2>&1; then \
		xdg-open $(PDF_FILE); \
	elif command -v open >/dev/null 2>&1; then \
		open $(PDF_FILE); \
	else \
		echo "Please open $(PDF_FILE) manually"; \
	fi

# Development targets
watch:
	@echo "Watching for changes to $(ORG_FILE)..."
	@while true; do \
		inotifywait -q -e modify $(ORG_FILE) 2>/dev/null || \
		sleep 2; \
		make pdf; \
	done

# Alternative export formats
html: $(ORG_FILE)
	$(EMACS) --batch \
		--eval "(require 'ox-html)" \
		--eval "(find-file \"$(ORG_FILE)\")" \
		--eval "(org-html-export-to-html)" \
		--kill

tex: $(TEX_FILE)

$(TEX_FILE): $(ORG_FILE)
	$(EMACS) --batch \
		--eval "(require 'ox-beamer)" \
		--eval "(find-file \"$(ORG_FILE)\")" \
		--eval "(org-beamer-export-to-latex)" \
		--kill

help:
	@echo "Available targets:"
	@echo "  pdf          - Build PDF presentation (default)"
	@echo "  html         - Export to HTML"
	@echo "  tex          - Export to LaTeX"
	@echo "  setup-beamer - Install required LaTeX packages"
	@echo "  preview      - Open PDF after building"
	@echo "  watch        - Continuously rebuild on file changes"
	@echo "  clean        - Remove generated files"
	@echo "  help         - Show this help message"