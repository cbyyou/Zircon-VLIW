# Zircon-VLIW

VLIW version for Zircon.

## Verilator simulation

The eight-slot Verilator runner and IPC report code live in `sim/`. Generate
the processor Verilog, run a raw RV-Software image, and print the processor's
hardware performance counters with:

```bash
make run IMG=/path/to/program.bin
```

The processor counts cycles, effective committed instructions, and committed
VLIW packets in `PerformanceMonitor.scala`. Integer NOP, the slot-0 filler
`feq.s zero, ft0, ft0`, and the halt marker are excluded from the effective
instruction count. ZirconSim is not required by this simulation path.
