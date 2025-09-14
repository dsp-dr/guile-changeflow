#!/bin/bash
# generate-gif.sh - Convert asciicinema recordings to GIFs

set -e

RECORDING=${1:-}
OUTPUT=${2:-}

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

function usage() {
    echo "Usage: $0 <recording.cast> [output.gif]"
    echo ""
    echo "Convert asciicinema recording to animated GIF"
    echo ""
    echo "Options:"
    echo "  recording.cast - Input asciicinema recording file"
    echo "  output.gif     - Output GIF file (optional)"
    echo ""
    echo "Example:"
    echo "  $0 demo.cast demo.gif"
    exit 1
}

function check_dependencies() {
    local deps_ok=true
    
    # Check for agg
    if ! command -v agg &> /dev/null; then
        echo -e "${YELLOW}agg not found. Installing...${NC}"
        
        # Try to install agg
        if command -v cargo &> /dev/null; then
            cargo install --git https://github.com/asciinema/agg
        else
            echo -e "${RED}Cargo not found. Please install Rust first.${NC}"
            echo "Visit: https://rustup.rs/"
            deps_ok=false
        fi
    fi
    
    # Check for gifsicle (optional, for optimization)
    if ! command -v gifsicle &> /dev/null; then
        echo -e "${YELLOW}gifsicle not found (optional, for optimization)${NC}"
        echo "Install: apt-get install gifsicle"
    fi
    
    if [ "$deps_ok" = false ]; then
        exit 1
    fi
}

function generate_gif() {
    local input_file=$1
    local output_file=$2
    
    echo -e "${YELLOW}🎨 Converting $input_file to GIF...${NC}"
    
    # Generate GIF with agg
    agg "$input_file" "$output_file" \
        --font-family "Monaco,Menlo,'Courier New',monospace" \
        --font-size 14 \
        --theme monokai \
        --speed 1.5 \
        --cols 120 \
        --rows 40
    
    echo -e "${GREEN}✅ GIF generated: $output_file${NC}"
    echo "   Size: $(du -h "$output_file" | cut -f1)"
    
    # Optimize if gifsicle is available
    if command -v gifsicle &> /dev/null; then
        echo -e "${YELLOW}Optimizing GIF...${NC}"
        
        local original_size=$(stat -f%z "$output_file" 2>/dev/null || stat -c%s "$output_file")
        
        gifsicle -O3 \
            --colors 256 \
            --resize-width 1200 \
            --lossy=30 \
            -i "$output_file" \
            -o "${output_file}.opt"
        
        mv "${output_file}.opt" "$output_file"
        
        local new_size=$(stat -f%z "$output_file" 2>/dev/null || stat -c%s "$output_file")
        local reduction=$((100 - (new_size * 100 / original_size)))
        
        echo -e "${GREEN}✅ Optimized: Reduced by ${reduction}%${NC}"
        echo "   Final size: $(du -h "$output_file" | cut -f1)"
    fi
}

function batch_convert() {
    echo -e "${YELLOW}🎨 Batch converting all recordings...${NC}"
    
    local recordings_dir="../recordings"
    local gifs_dir="../gifs"
    
    mkdir -p "$gifs_dir"
    
    for recording in "$recordings_dir"/*.cast; do
        if [ -f "$recording" ]; then
            local basename=$(basename "$recording" .cast)
            local gif_file="$gifs_dir/${basename}.gif"
            
            if [ -f "$gif_file" ]; then
                echo -e "${YELLOW}Skipping $basename (already exists)${NC}"
            else
                generate_gif "$recording" "$gif_file"
            fi
        fi
    done
    
    echo -e "${GREEN}\n✅ Batch conversion complete!${NC}"
}

# Main execution
if [ -z "$RECORDING" ]; then
    echo -e "${YELLOW}No recording specified. Running batch conversion...${NC}"
    check_dependencies
    batch_convert
else
    if [ ! -f "$RECORDING" ]; then
        echo -e "${RED}Error: Recording file not found: $RECORDING${NC}"
        usage
    fi
    
    if [ -z "$OUTPUT" ]; then
        OUTPUT="${RECORDING%.cast}.gif"
    fi
    
    check_dependencies
    generate_gif "$RECORDING" "$OUTPUT"
fi