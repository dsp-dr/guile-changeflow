# REORGANIZATION PLAN - From Chaos to Architecture

## Current Chaos (Root Directory):
```
47 files in root including:
- test-core-models.scm
- demo-ready-check.sh  
- test-endpoints.sh
- Multiple STATUS files
- Multiple ORCHESTRATION files
- Random .org files everywhere
```

## Target Architecture:
```
.
├── src/                    # All Guile source code
├── test/                   # ALL test files
├── cloudflare/             # Cloudflare deployment only
├── docs/                   # ALL documentation
├── scripts/                # ALL shell scripts
├── config/                 # Configuration files
└── README.md               # Single entry point
```

## Files to Move:

### TO test/:
- test-core-models.scm → test/
- demo-ready-check.sh → test/
- test-endpoints.sh → test/

### TO scripts/:
- All .sh files in root
- tools/*.sh → scripts/
- tools/*.py → scripts/monitoring/

### TO docs/:
- All .org files from root
- All STATUS*.md files
- All ORCHESTRATION*.org files
- instructions/* → docs/instructions/
- presentations/* → docs/presentations/

### TO config/:
- .env.example
- Makefile

### DELETE:
- .tmp/ (or move to .gitignore)
- Empty feat/* branches
- integration branch