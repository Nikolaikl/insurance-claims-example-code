# README - Claims Program

## Task Overview

The main task is to implement a disability bonus feature in the pension calculation system. This involves:

1. Adding new data fields to handle disability status and bonus calculations
2. Modifying the calculation logic to include disability bonuses
3. Updating the output display to show disability bonus information
4. Ensuring proper validation and error handling for disability-related inputs

### Implementation Details

- Add new WS-DISABILITY-STATUS field to track disability status (Y/N)
- Create WS-DISABILITY-BONUS field for bonus calculations
- Modify pension calculation logic to include bonus when applicable
- Update output display to show bonus information
- Add validation for disability status input

### COBOL Peculiarities

- COBOL uses fixed-format source code (columns 7-72 for code)
- Use 88-level condition names for status fields
- Numeric fields require proper PIC clauses
- String literals must be properly quoted and aligned
- Use PERFORM for loops and conditional logic

### Testing Requirements

1. Test cases for:
   - Non-disabled individuals (no bonus)
   - Disabled individuals (with bonus)
   - Edge cases (minimum/maximum values)
   - Invalid input handling

2. Test data scenarios:
   - Various disability levels
   - Different pension amounts
   - Boundary conditions

### Coding Standards

- Follow existing code style and conventions
- Use meaningful data names
- Add comments for complex logic
- Maintain proper alignment and spacing
- Use condition names for status fields
- Validate all user inputs

### Documentation Requirements

- Update program documentation header
- Add comments for new logic
- Document test cases
- Update user documentation
- Add change log entry

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
