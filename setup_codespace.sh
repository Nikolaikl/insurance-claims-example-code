#!/bin/bash

# Setup script for COBOL Claims Program development environment
# Designed for GitHub Codespaces

echo "Setting up COBOL development environment..."

# Install GnuCOBOL
echo "Installing GnuCOBOL..."
sudo apt-get update
sudo apt-get install -y gnucobol

# Verify installation
echo "GnuCOBOL version:"
cobc --version

# Create bin directory if it doesn't exist
echo "Creating bin directory..."
mkdir -p bin

# Make scripts executable
echo "Setting permissions..."
chmod +x bin/*

# Verify directory structure
echo "Project structure:"
tree

# Print setup completion message
echo ""
echo "Setup complete! You can now:"
echo "1. Build all programs: make all"
echo "2. Run the full sequence: make run"
echo "3. Clean up: make clean"
echo ""
echo "For more commands, see the Makefile or README.md"
