# Makefile for generating meta.pdf presentation from meta.org
# Uses Emacs org-mode export functionality

EMACS = emacs
ORG_FILE = meta.org
PDF_FILE = meta.pdf
TEX_FILE = meta.tex

# Default target
all: $(PDF_FILE)

# Generate PDF from org file via LaTeX
$(PDF_FILE): $(ORG_FILE)
	@echo "Generating PDF presentation from $(ORG_FILE)..."
	$(EMACS) --batch \
		--eval "(require 'ox-latex)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--eval "(setq org-latex-classes (cons '(\"article\" \"\\\\documentclass[11pt]{article}\" (\"\\\\section{%s}\" . \"\\\\section*{%s}\") (\"\\\\subsection{%s}\" . \"\\\\subsection*{%s}\") (\"\\\\subsubsection{%s}\" . \"\\\\subsubsection*{%s}\") (\"\\\\paragraph{%s}\" . \"\\\\paragraph*{%s}\") (\"\\\\subparagraph{%s}\" . \"\\\\subparagraph*{%s}\")) org-latex-classes))" \
		--visit $(ORG_FILE) \
		--eval "(org-latex-export-to-pdf)"
	@echo "PDF generated successfully: $(PDF_FILE)"

# Alternative: generate via pandoc if available
pdf-pandoc: $(ORG_FILE)
	@echo "Generating PDF using pandoc..."
	pandoc -f org -t latex -o $(PDF_FILE) $(ORG_FILE) \
		--pdf-engine=pdflatex \
		--variable geometry:margin=1in \
		--variable fontsize:11pt \
		--toc \
		--number-sections
	@echo "PDF generated successfully via pandoc: $(PDF_FILE)"

# Generate LaTeX intermediate file (for debugging)
tex: $(ORG_FILE)
	$(EMACS) --batch \
		--eval "(require 'ox-latex)" \
		--visit $(ORG_FILE) \
		--eval "(org-latex-export-to-latex)"

# Clean generated files
clean:
	rm -f $(PDF_FILE) $(TEX_FILE) *.aux *.log *.out *.toc

# Help target
help:
	@echo "Available targets:"
	@echo "  all        - Generate PDF from org file (default)"
	@echo "  pdf-pandoc - Generate PDF using pandoc instead of Emacs"
	@echo "  tex        - Generate LaTeX intermediate file"
	@echo "  clean      - Remove generated files"
	@echo "  help       - Show this help message"

# PDF target (alias for all)
pdf: all

# Phony targets
.PHONY: all pdf pdf-pandoc tex clean help

# Prerequisites check
check-deps:
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "Error: Emacs not found. Please install Emacs."; exit 1; }
	@command -v pdflatex >/dev/null 2>&1 || { echo "Error: pdflatex not found. Please install LaTeX distribution."; exit 1; }
	@echo "Dependencies satisfied."

# Quick build with dependency check
build: check-deps all

.DEFAULT_GOAL := all