# 🎬 ITIL Change Management Demo Suite

## Overview

This directory contains everything needed to record, replay, and present ITIL change management demonstrations to stakeholders. The demos use pre-generated data for reproducible results and can be exported as GIFs for presentations.

## 📁 Directory Structure

```
docs/demos/
├── README.md              # This file
├── specs/                 # Demo specifications
│   └── STAKEHOLDER-DEMO-SPEC.md  # Detailed stakeholder demo guide
├── scripts/               # Demo automation scripts
│   ├── record-demo.sh     # Main recording interface
│   ├── run-scenario.sh    # Execute specific scenarios
│   └── generate-gif.sh    # Convert recordings to GIFs
├── recordings/            # Asciicinema recordings (.cast files)
└── gifs/                  # Generated GIF animations
```

## 🚀 Quick Start

### 1. Generate Demo Data
```bash
# Generate reproducible demo datasets
cd ../.. # Go to project root
gmake demo-data
```

This creates:
- `data/demo-change-requests.json` - 100 PRs
- `data/demo-quick.json` - 5 PRs for quick demos
- `data/demo-frontend-only.json` - 10 frontend PRs
- `data/demo-backend-focus.json` - 15 backend PRs
- `data/demo-infrastructure.json` - 8 IaC PRs

### 2. Record a Demo
```bash
cd docs/demos/scripts
chmod +x *.sh
./record-demo.sh
```

### 3. Generate GIFs
```bash
# Convert specific recording
./generate-gif.sh ../recordings/quick_20240115_143022.cast

# Convert all recordings
./generate-gif.sh
```

## 🎬 Available Demo Scenarios

### Quick Demo (5 PRs, ~2 minutes)
**Purpose**: Executive overview, quick pitch  
**Highlights**: Basic workflow, success metrics  
```bash
./run-scenario.sh quick 5
```

### Frontend Demo (10 PRs, ~5 minutes)
**Purpose**: UI team presentation  
**Highlights**: Rapid deployments, low risk  
```bash
./run-scenario.sh frontend 10
```

### Backend Demo (15 PRs, ~8 minutes)
**Purpose**: Engineering deep dive  
**Highlights**: API changes, database migrations  
```bash
./run-scenario.sh backend 15
```

### Infrastructure Demo (8 PRs, ~10 minutes)
**Purpose**: DevOps and platform teams  
**Highlights**: IaC, multi-region, compliance  
```bash
./run-scenario.sh infra 8
```

### Peak Load Demo (20 PRs, ~5 minutes)
**Purpose**: Stress testing, Black Friday scenario  
**Highlights**: Queue management, priority handling  
```bash
./run-scenario.sh peak-load 20
```

### Emergency Demo (1 PR, ~1 minute)
**Purpose**: Incident response demonstration  
**Highlights**: Bypass approvals, rapid deployment  
```bash
./run-scenario.sh emergency 3
```

## 📊 Demo Metrics

Each demo shows:
- **Real-time queue depth**
- **Success/failure rates**
- **Mean Time To Recovery (MTTR)**
- **Component-specific statistics**
- **ITIL compliance status**

## 🎨 GIF Generation

### Prerequisites
```bash
# Install agg (asciicinema GIF generator)
cargo install --git https://github.com/asciinema/agg

# Install gifsicle for optimization (optional)
apt-get install gifsicle
```

### GIF Settings
- **Resolution**: 1200x800 pixels
- **Frame rate**: 10 FPS
- **Colors**: 256 (optimized)
- **Font**: Monaco/Menlo 14pt
- **Theme**: Monokai

## 📤 Presentation Tips

### For Executives (2-3 minutes)
1. Start with `quick` scenario
2. Focus on success rates and ROI
3. Show metrics dashboard
4. End with compliance benefits

### For Technical Teams (10-15 minutes)
1. Start with component-specific demo
2. Show failure injection and recovery
3. Demonstrate rollback procedures
4. Deep dive into ITIL state machine

### For Compliance Officers (5-8 minutes)
1. Use `backend` scenario with migrations
2. Highlight audit trail generation
3. Show approval workflows
4. Demonstrate emergency procedures

## 🔄 Reproducible Results

All demos use **seed 42** for consistent results:
- Same PR titles and authors every time
- Predictable failure injection points
- Consistent deployment times
- Identical success rates

This ensures demos are:
- **Repeatable** for multiple audiences
- **Predictable** for live presentations
- **Comparable** across different runs

## 🎯 Recording Best Practices

### Terminal Setup
```bash
# Set terminal size
printf '\e[8;40;120t'

# Use dark theme
export TERM=xterm-256color

# Clear scrollback
clear && printf '\e[3J'
```

### Recording Checklist
- [ ] Generate fresh demo data
- [ ] Clear terminal history
- [ ] Set correct terminal size (120x40)
- [ ] Test audio (if narrating)
- [ ] Close unnecessary applications
- [ ] Disable notifications

### Editing Recordings
```bash
# Trim recording
asciinema play recording.cast --speed 2

# Cut sections (edit .cast file)
# Each line is: [timestamp, "o", "output"]
```

## 📊 Sample GIF Previews

### Quick Demo
![Quick Demo](gifs/quick_demo.gif)
*5 PRs processed in 2 minutes*

### Peak Load
![Peak Load](gifs/peak_load_demo.gif)
*20 high-priority PRs during Black Friday*

### Emergency Patch
![Emergency](gifs/emergency_demo.gif)
*Critical security patch deployment*

## 🔗 Integration Points

Demos can connect to:
- **GitHub API** - Live PR data
- **Slack** - Real-time notifications
- **PagerDuty** - Incident correlation
- **Datadog** - Metrics visualization
- **ServiceNow** - ITIL ticketing

## 📋 Stakeholder Materials

See [STAKEHOLDER-DEMO-SPEC.md](specs/STAKEHOLDER-DEMO-SPEC.md) for:
- Detailed scenario descriptions
- Talking points per audience
- Success criteria
- ROI calculations
- Compliance matrices

## 🎦 Advanced Recording

### Multi-window Recording
```bash
# Create tmux session
tmux new-session -s demo -d
tmux split-window -h
tmux send-keys -t demo:0.0 "watch -n 1 'cat metrics.txt'" Enter
tmux send-keys -t demo:0.1 "./run-scenario.sh quick" Enter

# Record tmux session
asciinema rec --command "tmux attach -t demo"
```

### Add Narration
```bash
# Record with audio
asciinema rec --stdin recording.cast

# Play with subtitles
asciinema play recording.cast --speed 1.5
```

## 🔍 Troubleshooting

### Common Issues

**Problem**: GIF file too large  
**Solution**: Use gifsicle optimization
```bash
gifsicle -O3 --colors 128 --lossy=80 -i demo.gif -o demo-opt.gif
```

**Problem**: Recording has long pauses  
**Solution**: Use idle time limit
```bash
asciinema rec --idle-time-limit 2 demo.cast
```

**Problem**: Colors look wrong in GIF  
**Solution**: Adjust terminal theme before recording
```bash
export TERM=xterm-256color
```

## 📞 Support

For issues or questions:
- Check [STAKEHOLDER-DEMO-SPEC.md](specs/STAKEHOLDER-DEMO-SPEC.md)
- Run `./record-demo.sh --help`
- See main project README

---

*Last Updated: January 2024*  
*Demo System Version: 1.0*