#!/bin/bash
# record-demo.sh - Automated demo recording script

set -e

# Configuration
DEMO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$DEMO_DIR/../../.." && pwd)"
RECORDINGS_DIR="$DEMO_DIR/../recordings"
GIFS_DIR="$DEMO_DIR/../gifs"
DATA_DIR="$PROJECT_ROOT/data"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Demo scenarios
DEMO_SCENARIOS=(
    "quick:5:Quick 5-PR Demo"
    "frontend:10:Frontend-focused Demo"
    "backend:15:Backend-focused Demo"
    "infra:8:Infrastructure Demo"
    "peak-load:20:Peak Load Scenario"
    "emergency:3:Emergency Patch Demo"
)

function print_header() {
    echo -e "${BLUE}"
    echo "╔════════════════════════════════════════════════════════╗"
    echo "║         ITIL Change Management Demo Recorder          ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

function check_dependencies() {
    echo -e "${YELLOW}🔍 Checking dependencies...${NC}"
    
    local deps_ok=true
    
    # Check for asciinema
    if ! command -v asciinema &> /dev/null; then
        echo -e "${RED}❌ asciinema not found${NC}"
        echo "   Install: pip install asciinema"
        deps_ok=false
    else
        echo -e "${GREEN}✅ asciinema found${NC}"
    fi
    
    # Check for tmux
    if ! command -v tmux &> /dev/null; then
        echo -e "${RED}❌ tmux not found${NC}"
        echo "   Install: apt-get install tmux"
        deps_ok=false
    else
        echo -e "${GREEN}✅ tmux found${NC}"
    fi
    
    # Check for guile
    if ! command -v guile &> /dev/null; then
        echo -e "${RED}❌ guile not found${NC}"
        echo "   Install: apt-get install guile-3.0"
        deps_ok=false
    else
        echo -e "${GREEN}✅ guile found${NC}"
    fi
    
    # Check for demo data
    if [ ! -f "$DATA_DIR/demo-change-requests.json" ]; then
        echo -e "${YELLOW}⚠️  Demo data not found, generating...${NC}"
        (cd "$PROJECT_ROOT" && gmake demo-data)
    else
        echo -e "${GREEN}✅ Demo data found${NC}"
    fi
    
    if [ "$deps_ok" = false ]; then
        echo -e "${RED}\nPlease install missing dependencies and try again.${NC}"
        exit 1
    fi
}

function setup_terminal() {
    echo -e "${YELLOW}🖥️  Setting up terminal...${NC}"
    
    # Set terminal size
    printf '\e[8;40;120t'
    
    # Clear screen
    clear
    
    # Set dark theme colors
    export TERM=xterm-256color
    
    echo -e "${GREEN}✅ Terminal configured (120x40)${NC}"
}

function list_scenarios() {
    echo -e "${BLUE}\n📋 Available Demo Scenarios:${NC}"
    echo "════════════════════════════════════════"
    
    local i=1
    for scenario in "${DEMO_SCENARIOS[@]}"; do
        IFS=':' read -r name prs description <<< "$scenario"
        printf "  %d. %-15s - %s (%d PRs)\n" $i "$name" "$description" $prs
        ((i++))
    done
    echo ""
}

function record_scenario() {
    local scenario_name=$1
    local scenario_prs=$2
    local scenario_desc=$3
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local output_file="$RECORDINGS_DIR/${scenario_name}_${timestamp}.cast"
    
    echo -e "${YELLOW}\n🎬 Recording: $scenario_desc${NC}"
    echo "Output: $output_file"
    echo ""
    
    # Create recordings directory if it doesn't exist
    mkdir -p "$RECORDINGS_DIR"
    
    # Start recording with asciinema
    asciinema rec \
        --title "ITIL Demo: $scenario_desc" \
        --idle-time-limit 2 \
        --command "$DEMO_DIR/run-scenario.sh $scenario_name $scenario_prs" \
        "$output_file"
    
    echo -e "${GREEN}\n✅ Recording saved: $output_file${NC}"
    
    # Ask if user wants to generate GIF
    echo -e "${YELLOW}\nGenerate GIF from recording? (y/n)${NC}"
    read -r response
    if [[ "$response" =~ ^[Yy]$ ]]; then
        generate_gif "$output_file" "$scenario_name" "$timestamp"
    fi
}

function generate_gif() {
    local cast_file=$1
    local scenario_name=$2
    local timestamp=$3
    local gif_file="$GIFS_DIR/${scenario_name}_${timestamp}.gif"
    
    echo -e "${YELLOW}🎨 Generating GIF...${NC}"
    
    # Create GIFs directory if it doesn't exist
    mkdir -p "$GIFS_DIR"
    
    # Check for agg (asciinema gif generator)
    if ! command -v agg &> /dev/null; then
        echo -e "${YELLOW}Installing agg...${NC}"
        cargo install --git https://github.com/asciinema/agg
    fi
    
    # Generate GIF
    agg "$cast_file" "$gif_file" \
        --font-family "Monaco,Menlo,'Courier New'" \
        --font-size 14 \
        --theme monokai \
        --speed 1.5
    
    # Optimize GIF
    if command -v gifsicle &> /dev/null; then
        echo -e "${YELLOW}Optimizing GIF...${NC}"
        gifsicle -O3 --colors 256 -i "$gif_file" -o "${gif_file}.opt"
        mv "${gif_file}.opt" "$gif_file"
    fi
    
    echo -e "${GREEN}✅ GIF saved: $gif_file${NC}"
    echo "   Size: $(du -h "$gif_file" | cut -f1)"
}

function record_all_scenarios() {
    echo -e "${YELLOW}\n🎬 Recording all scenarios...${NC}"
    
    for scenario in "${DEMO_SCENARIOS[@]}"; do
        IFS=':' read -r name prs description <<< "$scenario"
        record_scenario "$name" "$prs" "$description"
        
        echo -e "${YELLOW}\nWaiting 5 seconds before next recording...${NC}"
        sleep 5
    done
    
    echo -e "${GREEN}\n✅ All scenarios recorded!${NC}"
}

function main_menu() {
    while true; do
        echo -e "${BLUE}\n🎯 Demo Recording Menu${NC}"
        echo "════════════════════════"
        echo "1. Record specific scenario"
        echo "2. Record all scenarios"
        echo "3. Generate GIF from existing recording"
        echo "4. List recordings"
        echo "5. Exit"
        echo ""
        echo -n "Select option: "
        read -r option
        
        case $option in
            1)
                list_scenarios
                echo -n "Select scenario number: "
                read -r scenario_num
                
                if [ "$scenario_num" -ge 1 ] && [ "$scenario_num" -le ${#DEMO_SCENARIOS[@]} ]; then
                    scenario=${DEMO_SCENARIOS[$((scenario_num-1))]}
                    IFS=':' read -r name prs description <<< "$scenario"
                    record_scenario "$name" "$prs" "$description"
                else
                    echo -e "${RED}Invalid selection${NC}"
                fi
                ;;
            2)
                record_all_scenarios
                ;;
            3)
                echo -e "${BLUE}\nExisting recordings:${NC}"
                ls -la "$RECORDINGS_DIR"/*.cast 2>/dev/null || echo "No recordings found"
                echo -n "\nEnter recording filename: "
                read -r recording_file
                if [ -f "$RECORDINGS_DIR/$recording_file" ]; then
                    scenario_name=$(basename "$recording_file" .cast | cut -d'_' -f1)
                    timestamp=$(basename "$recording_file" .cast | cut -d'_' -f2-)
                    generate_gif "$RECORDINGS_DIR/$recording_file" "$scenario_name" "$timestamp"
                else
                    echo -e "${RED}File not found${NC}"
                fi
                ;;
            4)
                echo -e "${BLUE}\n📁 Recordings:${NC}"
                ls -lah "$RECORDINGS_DIR"/*.cast 2>/dev/null || echo "No recordings found"
                echo -e "${BLUE}\n🎨 GIFs:${NC}"
                ls -lah "$GIFS_DIR"/*.gif 2>/dev/null || echo "No GIFs found"
                ;;
            5)
                echo -e "${GREEN}Goodbye!${NC}"
                exit 0
                ;;
            *)
                echo -e "${RED}Invalid option${NC}"
                ;;
        esac
    done
}

# Main execution
print_header
check_dependencies
setup_terminal
main_menu