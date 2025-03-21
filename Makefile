.PHONY: all setup pensclm run clean setup-env

# Add setup-env target
setup-env:
	@echo "Setting up development environment..."
	@chmod +x setup_codespace.sh
	@./setup_codespace.sh
# Makefile for COBOL Claims Program
# Targets:
#   all       - Build all programs (default)
#   setup     - Build setup programs
#   pensclm   - Build main program
#   clean     - Remove compiled binaries
#   run       - Run setup programs then main program

# Compiler and flags
COBC = cobc
COBCFLAGS = -x -O
SRC_DIR = src
BIN_DIR = bin

# Source files
SETUP_SRCS = $(SRC_DIR)/polsetup.cbl $(SRC_DIR)/clmsetup.cbl \
             $(SRC_DIR)/indsetup.cbl $(SRC_DIR)/geosetup.cbl
MAIN_SRC = $(SRC_DIR)/pensclm.cbl

# Executables
SETUP_BINS = $(BIN_DIR)/polsetup $(BIN_DIR)/clmsetup \
             $(BIN_DIR)/indsetup $(BIN_DIR)/geosetup
MAIN_BIN = $(BIN_DIR)/pensclm

# Create bin directory if it doesn't exist
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Default target
all: $(BIN_DIR) setup pensclm

# Build setup programs
setup: $(SETUP_BINS)

# Build main program
pensclm: $(MAIN_BIN)

# Individual build rules
$(BIN_DIR)/polsetup: $(SRC_DIR)/polsetup.cbl
	$(COBC) $(COBCFLAGS) -o $@ $<

$(BIN_DIR)/clmsetup: $(SRC_DIR)/clmsetup.cbl
	$(COBC) $(COBCFLAGS) -o $@ $<

$(BIN_DIR)/indsetup: $(SRC_DIR)/indsetup.cbl
	$(COBC) $(COBCFLAGS) -o $@ $<

$(BIN_DIR)/geosetup: $(SRC_DIR)/geosetup.cbl
	$(COBC) $(COBCFLAGS) -o $@ $<

$(BIN_DIR)/pensclm: $(MAIN_SRC) $(SETUP_SRCS)
	$(COBC) $(COBCFLAGS) -o $@ $(MAIN_SRC)

# Run all programs in sequence
run: all
	@echo "Running setup programs..."
	@for prog in $(SETUP_BINS); do \
		echo "Running $$prog..."; \
		$$prog; \
	done
	@echo "Running main program with INPUT.txt..."
	@$(MAIN_BIN) INPUT.txt OUTPUT.txt

# Clean up
clean:
	rm -f $(SETUP_BINS) $(MAIN_BIN)
