# Experiments Directory Makefile
# This orchestrates all experiments

.PHONY: all test list clean help

EXPERIMENTS := $(wildcard [0-9][0-9][0-9]-*)

help:
	@echo "Guile ChangeFlow Experiments"
	@echo "============================"
	@echo ""
	@echo "Available targets:"
	@echo "  all   - Run all experiments"
	@echo "  test  - Test all experiments"
	@echo "  list  - List all experiments"
	@echo "  clean - Clean all experiments"
	@echo "  help  - Show this message"
	@echo ""
	@echo "Run specific experiment:"
	@echo "  gmake -C 000-core-tools test"

all: test

test:
	@echo "Running all experiments..."
	@for exp in $(EXPERIMENTS); do \
		echo ""; \
		echo "==> Testing $$exp"; \
		$(MAKE) -C $$exp test || exit 1; \
	done
	@echo ""
	@echo "All experiments completed successfully!"

list:
	@echo "Available experiments:"
	@for exp in $(EXPERIMENTS); do \
		echo "  - $$exp"; \
	done

clean:
	@echo "Cleaning all experiments..."
	@for exp in $(EXPERIMENTS); do \
		$(MAKE) -C $$exp clean; \
	done

# Validate experiment structure
validate:
	@echo "Validating experiment structure..."
	@for exp in $(EXPERIMENTS); do \
		if [ ! -f $$exp/README.org ]; then \
			echo "ERROR: $$exp missing README.org"; \
			exit 1; \
		fi; \
		if [ ! -f $$exp/Makefile ]; then \
			echo "ERROR: $$exp missing Makefile"; \
			exit 1; \
		fi; \
		if ls $$exp/*.scm $$exp/*.go $$exp/*.c $$exp/*.py 2>/dev/null | grep -q .; then \
			echo "ERROR: $$exp contains code artifacts"; \
			exit 1; \
		fi; \
	done
	@echo "All experiments valid!"

# Create new experiment
new:
	@if [ -z "$(NAME)" ]; then \
		echo "Usage: gmake new NAME=experiment-name"; \
		exit 1; \
	fi
	@NUM=$$(printf "%03d" $$(($$(ls -d [0-9][0-9][0-9]-* 2>/dev/null | wc -l) + 1))); \
	mkdir $$NUM-$(NAME); \
	echo "Created experiment: $$NUM-$(NAME)"