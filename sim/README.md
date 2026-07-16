# Zircon-VLIW Verilator runner

This directory contains the processor-owned Verilator entry point. It drives
the generated eight-slot `CPU.sv`, connects the two LSU ports to the unmodified
`Zircon-SimModels` DDR model, and optionally performs commit-time GPR/FPR
differential testing against `Zircon-SimModels::RVCPU`.

The processor RTL owns the cycle, effective-instruction, and committed-packet
counters. `main.cpp` only reads the three debug outputs and formats:

```text
effective_ipc = perfEffectiveInstructions / perfCycles
avg_effective_insts_per_packet = perfEffectiveInstructions / perfExecutedPackets
```

Run from the processor repository root:

```bash
make run IMG=/path/to/program.bin
```

Enable differential testing and override the default cycle/memory limits:

```bash
ZIRCON_DIFFTEST=1 make run IMG=/path/to/program.bin ARGS="10000000 64"
```

The default `SIMMODELS_DIR` expects `Zircon-SimModels` beside the processor
repository. Override it when using another layout:

```bash
make run IMG=/path/to/program.bin SIMMODELS_DIR=/path/to/Zircon-SimModels
```
