# Experiment 000: Core Tools Validation
# Tests for required development tools

.PHONY: all test clean help
.PHONY: test-guile test-build test-libs test-dev test-optional
.PHONY: report summary

# Test configuration
REQUIRED_GUILE_VERSION := 3.0
OUTPUT_DIR := /tmp/exp-000-$$$$

help:
	@echo "Core Tools Validation"
	@echo "===================="
	@echo ""
	@echo "Targets:"
	@echo "  test         - Run all tests"
	@echo "  test-guile   - Test Guile installation"
	@echo "  test-build   - Test build tools"
	@echo "  test-libs    - Test Guile libraries"
	@echo "  test-dev     - Test development tools"
	@echo "  test-optional - Test optional tools"
	@echo "  report       - Generate full report"
	@echo "  summary      - Show summary"
	@echo "  clean        - Clean test artifacts"

all: test

test: test-guile test-build test-libs test-dev test-optional summary

test-guile:
	@echo "==> Testing Guile Core Tools"
	@echo -n "guile: "
	@if command -v guile >/dev/null 2>&1; then \
		guile --version | head -n1; \
		guile -c "(display (string-append \"  effective-version: \" (effective-version)))(newline)"; \
		guile -c "(display (string-append \"  %host-type: \" %host-type))(newline)"; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "guild: "
	@if command -v guild >/dev/null 2>&1; then \
		echo "found"; \
		guild compile --version | head -n1 | sed 's/^/  /'; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "guile-config: "
	@if command -v guile-config >/dev/null 2>&1; then \
		echo "found"; \
		echo "  compile flags: $$(guile-config compile)"; \
		echo "  link flags: $$(guile-config link)"; \
	else \
		echo "NOT FOUND"; \
	fi

test-build:
	@echo ""
	@echo "==> Testing Build Tools"
	@echo -n "gmake: "
	@if command -v gmake >/dev/null 2>&1; then \
		gmake --version | head -n1; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "gcc: "
	@if command -v gcc >/dev/null 2>&1; then \
		gcc --version | head -n1; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "clang: "
	@if command -v clang >/dev/null 2>&1; then \
		clang --version | head -n1; \
	else \
		echo "not found (optional)"; \
	fi
	@echo -n "pkg-config: "
	@if command -v pkg-config >/dev/null 2>&1; then \
		pkg-config --version; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "autoconf: "
	@if command -v autoconf >/dev/null 2>&1; then \
		autoconf --version | head -n1; \
	else \
		echo "not found (optional)"; \
	fi
	@echo -n "automake: "
	@if command -v automake >/dev/null 2>&1; then \
		automake --version | head -n1; \
	else \
		echo "not found (optional)"; \
	fi

test-libs:
	@echo ""
	@echo "==> Testing Guile Libraries"
	@echo -n "SRFI-1 (lists): "
	@if guile -c "(use-modules (srfi srfi-1))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "SRFI-9 (records): "
	@if guile -c "(use-modules (srfi srfi-9))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "SRFI-18 (threads): "
	@if guile -c "(use-modules (srfi srfi-18))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "not available (optional)"; \
	fi
	@echo -n "SRFI-64 (testing): "
	@if guile -c "(use-modules (srfi srfi-64))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "ice-9 match: "
	@if guile -c "(use-modules (ice-9 match))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "ice-9 threads: "
	@if guile -c "(use-modules (ice-9 threads))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "web server: "
	@if guile -c "(use-modules (web server))(exit 0)" 2>/dev/null; then \
		echo "available"; \
	else \
		echo "NOT AVAILABLE"; \
	fi
	@echo -n "json: "
	@if guile -c "(use-modules (json))(exit 0)" 2>/dev/null; then \
		echo "available (guile-json)"; \
	else \
		echo "NOT AVAILABLE - need guile-json"; \
	fi

test-dev:
	@echo ""
	@echo "==> Testing Development Tools"
	@echo -n "git: "
	@if command -v git >/dev/null 2>&1; then \
		git --version; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "gh: "
	@if command -v gh >/dev/null 2>&1; then \
		gh --version | head -n1; \
	else \
		echo "NOT FOUND"; \
	fi
	@echo -n "texinfo: "
	@if command -v makeinfo >/dev/null 2>&1; then \
		makeinfo --version | head -n1; \
	else \
		echo "not found (optional)"; \
	fi
	@echo -n "sqlite3: "
	@if command -v sqlite3 >/dev/null 2>&1; then \
		sqlite3 --version; \
	else \
		echo "NOT FOUND"; \
	fi

test-optional:
	@echo ""
	@echo "==> Testing Optional Tools"
	@echo -n "redis-server: "
	@if command -v redis-server >/dev/null 2>&1; then \
		redis-server --version; \
	else \
		echo "not found"; \
	fi
	@echo -n "redis-cli: "
	@if command -v redis-cli >/dev/null 2>&1; then \
		redis-cli --version; \
	else \
		echo "not found"; \
	fi
	@echo -n "psql: "
	@if command -v psql >/dev/null 2>&1; then \
		psql --version; \
	else \
		echo "not found"; \
	fi
	@echo -n "docker: "
	@if command -v docker >/dev/null 2>&1; then \
		docker --version; \
	else \
		echo "not found"; \
	fi
	@echo -n "podman: "
	@if command -v podman >/dev/null 2>&1; then \
		podman --version; \
	else \
		echo "not found"; \
	fi

summary:
	@echo ""
	@echo "==> Summary"
	@echo "System: $$(uname -srm)"
	@echo ""
	@echo "Critical tools status:"
	@echo "  [Check README.org Results section for details]"

report:
	@echo "Generating detailed report..."
	@$(MAKE) test > report.txt 2>&1
	@echo "Report saved to report.txt"

clean:
	@rm -f report.txt
	@rm -rf /tmp/exp-000-*
	@echo "Cleaned test artifacts"

# Validation targets for CI
validate-required: test-guile test-build test-libs test-dev
	@echo ""
	@echo "Required tools validation complete"

# Installation helpers (FreeBSD specific)
install-missing:
	@echo "To install missing tools on FreeBSD:"
	@echo "  pkg install guile guile-json sqlite3"
	@echo "  pkg install gmake gcc pkgconf"
	@echo "  pkg install git gh"
	@echo ""
	@echo "Optional:"
	@echo "  pkg install redis postgresql15-client"
	@echo "  pkg install texinfo-install-info"