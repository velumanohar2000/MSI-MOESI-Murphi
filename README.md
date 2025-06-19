
# README

This repository contains two versions of the MSI protocol model:

- **msi**: The original MSI protocol implementation.
- **msi_opt**: The optimized version of the original MSI model with performance enhancements and improved state handling.

## Building and Running the Models

### Original MSI Version

To generate, build, and run the original MSI model, execute the following commands:

1. **Generate the model file:**

   ```bash
   ./mu msi.m
   ```

2. **Build the executable:**

   ```bash
   make msi
   ```

3. **Run the simulation:**

   ```bash
   ./msi -tv -m 1024 > msi.out
   ```

   This command runs the simulation with test-verbose mode (`-tv`) using 1024 MB of memory (`-m 1024`), and outputs the results to `msi.out`.

### Optimized MSI Version (msi_opt)

The optimized version, `msi_opt`, incorporates enhancements over the original MSI version. To generate, build, and run the optimized version, use:

1. **Generate the model file:**

   ```bash
   ./mu msi_opt.m
   ```

2. **Build the optimized executable:**

   ```bash
   make msi_opt
   ```

3. **Run the simulation:**

   ```bash
   ./msi_opt -tv -m 1024 > msi_opt.out
   ```

   This command also uses test-verbose mode with 1024 MB of memory, and writes the simulation output to `msi_opt.out`.

## Overview

- **msi**: The original MSI protocol model.
- **msi_opt**: The optimized version of the MSI model with improved performance and refined state transitions.
- **Simulation Options**:
  - `-tv`: Enables test-verbose mode.
  - `-m 1024`: Allocates 1024 MB of memory for the simulation.
- **Output Files**:
  - `msi.out`: Output file for the original MSI simulation.
  - `msi_opt.out`: Output file for the optimized MSI simulation.

## Requirements

- Bash (bash-4.2 or later)
- A build environment with `make` installed.
- The model-checking tool `mu` must be accessible in your system path.

## Usage
Change into the project directory and run the appropriate commands as described above. Review the output files (`msi.out` and `msi_opt.out`) to compare the performance and verification results between the original MSI model and its optimized version.
