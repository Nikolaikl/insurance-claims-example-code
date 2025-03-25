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

```bash
# For GnuCOBOL:
cobc -x -o polsetup polsetup.cbl
cobc -x -o clmsetup clmsetup.cbl
cobc -x -o indsetup indsetup.cbl
cobc -x -o geosetup geosetup.cbl

# with optimisation
cobc -O -x -o polsetup polsetup.cbl
cobc -O -x -o clmsetup clmsetup.cbl
cobc -O -x -o indsetup indsetup.cbl
cobc -O -x -o geosetup geosetup.cbl

# Run them in sequence:
./polsetup
./clmsetup
./indsetup
./geosetup

```

Compile and Run the Main Program:

```bash
cobc -x -o pensclm pensclm.cbl
./pensclm

```
