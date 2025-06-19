
# README for MSI and MOESI Protocol (msi_opt)

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

### Optimized MSI Version MOESI Protocol (msi_opt)
Building on the successful MSI model, I implemented the MOESI protocol which incorporates two additional stable states:
* `Exclusive (E):` Indicates that a processor has the only valid copy of the block in a clean (unmodified) state.
* `Owned (O):` Designates one cache as the owner for servicing shared requests while other caches maintain a valid copy. This state minimizes full directory lookups and reduces invalidation broadcasts.
  
In addition, multiple transient states are introduced on both the home node and processor sides. For example, states like `H_IE`, `H_MOD`, `H_OMA`, and others manage the handshaking and message delays that occur during transitions between stable states. Similarly, on the processor side, transient states such as `P_MIA`, `P_OMAC`, `P_OMA`, and others help ensure smooth upgrades from a shared to a modified state and a proper response to outstanding invalidation or acknowledgment messages.
### Optimizations Over MSI:
* The enhancements provided by the MOESI protocol lead to several key optimizations:
Reduced Invalidation Traffic: By using the Exclusive state, the protocol avoids unnecessary invalidation messages when only one cache holds the data.
* Efficient Handling of Dirty Data: The Owned state allows for a dirty block to be shared with other processors. Which lowers the coherence traffic. 
* Improved Transition Handling: The new transient states allow for smoother and more robust state transitions, especially under conditions with overlapping requests or message reordering—thus eliminating potential race conditions and reducing the overall communication cost compared to the baseline MSI protocol.

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
