# README - Claims Program

## Task Overview

possible tasks include

### Debug Running Programm - does not finish

### Develop more features 

### Refactor to Java

### Refactor to Python

### Work on CI/CD Pipeline

### Add Testing frameworks


## Development Setup

To set up your development environment in a GitHub Codespace:

```bash
# Run the setup script
./setup_codespace.sh
```

## Building and Running

Use the Makefile for all build tasks:

```bash
# Build all programs (default)
make

# Build setup programs
make setup

# Build main program
make pensclm

# Run full processing pipeline
make run

# Clean built binaries
make clean

# Set up development environment
make setup-env
```

The Makefile handles:
- Automatic dependency tracking
- Output file organization in bin/
- Parallel builds where possible
- Clean rebuilds
