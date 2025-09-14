# Presentations Makefile
# Builds presentation artifacts from org-mode sources

.PHONY: all clean executive meta

EXECUTIVE_DIR = executive-demo
META_DIR = meta-analysis

all: executive meta

executive:
	cd $(EXECUTIVE_DIR) && $(MAKE)

meta:
	cd $(META_DIR) && $(MAKE)

clean:
	cd $(EXECUTIVE_DIR) && $(MAKE) clean
	cd $(META_DIR) && $(MAKE) clean

install:
	@echo "Installing presentation dependencies..."
	@echo "Requires: pandoc, latex, beamer"

.DEFAULT_GOAL := all