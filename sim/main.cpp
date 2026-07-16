#include "VCPU.h"
#include "verilated.h"
#include "zircon/DDR.hpp"
#include "zircon/RVCPU.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <deque>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace {

constexpr uint32_t kMemoryBase = 0x80000000u;
constexpr uint32_t kUartAddress = 0xa00003f8u;
constexpr uint32_t kTestWriteCounterAddress = 0xa0000400u;
constexpr uint32_t kHaltInstruction = 0x80000000u;
constexpr uint64_t kDefaultMaxCycles = 1000000;
constexpr std::size_t kDefaultMemoryMiB = 16;

struct Commit {
    uint64_t cycle;
    uint32_t pc;
    uint32_t inst;
};

struct RegisterWrite {
    bool valid = false;
    uint64_t cycle = 0;
    std::size_t slot = 0;
    uint32_t pc = 0;
    uint32_t inst = 0;
    uint32_t wbData = 0;
    uint32_t expected = 0;
};

void printPerformanceMetrics(
    uint64_t cycles,
    uint64_t effectiveInstructions,
    uint64_t executedPackets) {
    const double effectiveIpc = cycles == 0
        ? 0.0
        : static_cast<double>(effectiveInstructions) / static_cast<double>(cycles);
    const double averageEffectiveInstructions = executedPackets == 0
        ? 0.0
        : static_cast<double>(effectiveInstructions) / static_cast<double>(executedPackets);

    std::cout << "effective_instructions=" << effectiveInstructions
              << " executed_packets=" << executedPackets
              << " effective_ipc=" << std::fixed << std::setprecision(6) << effectiveIpc
              << " avg_effective_insts_per_packet=" << averageEffectiveInstructions
              << std::defaultfloat << "\n";
}

std::vector<uint8_t> readFile(const std::string& path) {
    std::ifstream input(path, std::ios::binary | std::ios::ate);
    if (!input) {
        throw std::runtime_error("failed to open image: " + path);
    }

    const std::streamoff size = input.tellg();
    if (size <= 0) {
        throw std::runtime_error("image is empty: " + path);
    }
    input.seekg(0, std::ios::beg);

    std::vector<uint8_t> data(static_cast<std::size_t>(size));
    input.read(reinterpret_cast<char*>(data.data()), size);
    if (!input) {
        throw std::runtime_error("failed to read image: " + path);
    }
    return data;
}

class MemoryMap {
public:
    explicit MemoryMap(std::size_t sizeBytes)
        : ddr_(makeConfig(sizeBytes)) {
    }

    void load(const std::vector<uint8_t>& image) {
        if (image.size() > ddr_.sizeBytes()) {
            throw std::runtime_error("image does not fit in configured memory");
        }
        std::copy(image.begin(), image.end(), ddr_.data().begin());
    }

    uint32_t read(uint32_t address, uint32_t size) const {
        if (isMemory(address, size)) {
            return ddr_.read(address - kMemoryBase, size);
        }
        if (address == kTestWriteCounterAddress && size == 4) {
            return testWriteCount_;
        }
        if (address >= 0xa0000000u) {
            return 0;
        }
        std::ostringstream message;
        message << "read outside RV-Software memory map: addr=0x" << std::hex
                << address << " size=" << std::dec << size;
        throw std::out_of_range(message.str());
    }

    void write(uint32_t address, uint32_t value, uint32_t size) {
        if (isMemory(address, size)) {
            ddr_.write(address - kMemoryBase, value, size);
            return;
        }
        if (address == kUartAddress) {
            std::cout.put(static_cast<char>(value & 0xffu));
            std::cout.flush();
            return;
        }
        if (address == kTestWriteCounterAddress && size == 4) {
            ++testWriteCount_;
            return;
        }
        if (address >= 0xa0000000u) {
            return;
        }
        std::ostringstream message;
        message << "write outside RV-Software memory map: addr=0x" << std::hex
                << address << " size=" << std::dec << size;
        throw std::out_of_range(message.str());
    }

private:
    static zircon::DDR::Config makeConfig(std::size_t sizeBytes) {
        zircon::DDR::Config config;
        config.sizeBytes = sizeBytes;
        config.nativeDataBits = 32;
        config.axiDataBits = 32;
        return config;
    }

    bool isMemory(uint32_t address, uint32_t size) const {
        if (address < kMemoryBase) {
            return false;
        }
        const uint64_t offset = static_cast<uint64_t>(address - kMemoryBase);
        return offset + size <= ddr_.sizeBytes();
    }

    zircon::DDR ddr_;
    uint32_t testWriteCount_ = 0;
};

uint32_t loadSize(uint8_t op) {
    switch (op) {
    case 0x10: // LB
    case 0x14: // LBU
        return 1;
    case 0x11: // LH
    case 0x15: // LHU
        return 2;
    case 0x12: // LW
    case 0x54: // FLW
        return 4;
    default:
        return 0;
    }
}

uint32_t storeSize(uint8_t op) {
    switch (op) {
    case 0x20: // SB
        return 1;
    case 0x21: // SH
        return 2;
    case 0x22: // SW
    case 0x62: // FSW
        return 4;
    default:
        return 0;
    }
}

void driveInstructionMemory(VCPU& cpu, const MemoryMap& memory) {
    const uint32_t pc = cpu.io_imem_pc;
    std::array<uint32_t*, 8> inputs = {
        &cpu.io_imem_insts_0, &cpu.io_imem_insts_1,
        &cpu.io_imem_insts_2, &cpu.io_imem_insts_3,
        &cpu.io_imem_insts_4, &cpu.io_imem_insts_5,
        &cpu.io_imem_insts_6, &cpu.io_imem_insts_7,
    };
    for (std::size_t slot = 0; slot < inputs.size(); ++slot) {
        *inputs[slot] = memory.read(pc + static_cast<uint32_t>(slot * 4), 4);
    }
}

void driveLoadData(VCPU& cpu, const MemoryMap& memory) {
    const uint32_t size0 = loadSize(cpu.io_dmem_lsu0_op);
    const uint32_t size1 = loadSize(cpu.io_dmem_lsu1_op);
    cpu.io_dmem_lsu0_rdata = size0 ? memory.read(cpu.io_dmem_lsu0_addr, size0) : 0;
    cpu.io_dmem_lsu1_rdata = size1 ? memory.read(cpu.io_dmem_lsu1_addr, size1) : 0;
}

void commitStores(VCPU& cpu, MemoryMap& memory) {
    const uint32_t size0 = storeSize(cpu.io_dmem_lsu0_op);
    const uint32_t size1 = storeSize(cpu.io_dmem_lsu1_op);
    if (cpu.io_dmem_lsu0_valid && size0) {
        memory.write(cpu.io_dmem_lsu0_addr, cpu.io_dmem_lsu0_wdata, size0);
    }
    if (cpu.io_dmem_lsu1_valid && size1) {
        memory.write(cpu.io_dmem_lsu1_addr, cpu.io_dmem_lsu1_wdata, size1);
    }
}

void settle(VCPU& cpu, const MemoryMap& memory) {
    cpu.eval();
    driveInstructionMemory(cpu, memory);
    driveLoadData(cpu, memory);
    cpu.eval();
}

void driveResetInputs(VCPU& cpu) {
    cpu.io_imem_insts_0 = 0;
    cpu.io_imem_insts_1 = 0;
    cpu.io_imem_insts_2 = 0;
    cpu.io_imem_insts_3 = 0;
    cpu.io_imem_insts_4 = 0;
    cpu.io_imem_insts_5 = 0;
    cpu.io_imem_insts_6 = 0;
    cpu.io_imem_insts_7 = 0;
    cpu.io_dmem_lsu0_rdata = 0;
    cpu.io_dmem_lsu1_rdata = 0;
}

std::array<uint8_t, 8> wbValid(const VCPU& cpu) {
    return {cpu.io_debug_wbValid_0, cpu.io_debug_wbValid_1,
            cpu.io_debug_wbValid_2, cpu.io_debug_wbValid_3,
            cpu.io_debug_wbValid_4, cpu.io_debug_wbValid_5,
            cpu.io_debug_wbValid_6, cpu.io_debug_wbValid_7};
}

std::array<uint32_t, 8> wbPc(const VCPU& cpu) {
    return {cpu.io_debug_wbPC_0, cpu.io_debug_wbPC_1,
            cpu.io_debug_wbPC_2, cpu.io_debug_wbPC_3,
            cpu.io_debug_wbPC_4, cpu.io_debug_wbPC_5,
            cpu.io_debug_wbPC_6, cpu.io_debug_wbPC_7};
}

std::array<uint32_t, 8> wbInst(const VCPU& cpu) {
    return {cpu.io_debug_wbInst_0, cpu.io_debug_wbInst_1,
            cpu.io_debug_wbInst_2, cpu.io_debug_wbInst_3,
            cpu.io_debug_wbInst_4, cpu.io_debug_wbInst_5,
            cpu.io_debug_wbInst_6, cpu.io_debug_wbInst_7};
}

std::array<uint8_t, 8> wbRd(const VCPU& cpu) {
    return {cpu.io_debug_wbRd_0, cpu.io_debug_wbRd_1,
            cpu.io_debug_wbRd_2, cpu.io_debug_wbRd_3,
            cpu.io_debug_wbRd_4, cpu.io_debug_wbRd_5,
            cpu.io_debug_wbRd_6, cpu.io_debug_wbRd_7};
}

std::array<uint32_t, 8> wbData(const VCPU& cpu) {
    return {cpu.io_debug_wbData_0, cpu.io_debug_wbData_1,
            cpu.io_debug_wbData_2, cpu.io_debug_wbData_3,
            cpu.io_debug_wbData_4, cpu.io_debug_wbData_5,
            cpu.io_debug_wbData_6, cpu.io_debug_wbData_7};
}

std::array<uint32_t, 32> gprState(const VCPU& cpu) {
    return {
        cpu.io_debug_gpr_0, cpu.io_debug_gpr_1,
        cpu.io_debug_gpr_2, cpu.io_debug_gpr_3,
        cpu.io_debug_gpr_4, cpu.io_debug_gpr_5,
        cpu.io_debug_gpr_6, cpu.io_debug_gpr_7,
        cpu.io_debug_gpr_8, cpu.io_debug_gpr_9,
        cpu.io_debug_gpr_10, cpu.io_debug_gpr_11,
        cpu.io_debug_gpr_12, cpu.io_debug_gpr_13,
        cpu.io_debug_gpr_14, cpu.io_debug_gpr_15,
        cpu.io_debug_gpr_16, cpu.io_debug_gpr_17,
        cpu.io_debug_gpr_18, cpu.io_debug_gpr_19,
        cpu.io_debug_gpr_20, cpu.io_debug_gpr_21,
        cpu.io_debug_gpr_22, cpu.io_debug_gpr_23,
        cpu.io_debug_gpr_24, cpu.io_debug_gpr_25,
        cpu.io_debug_gpr_26, cpu.io_debug_gpr_27,
        cpu.io_debug_gpr_28, cpu.io_debug_gpr_29,
        cpu.io_debug_gpr_30, cpu.io_debug_gpr_31,
    };
}

std::array<uint32_t, 32> fprState(const VCPU& cpu) {
    return {
        cpu.io_debug_fpr_0, cpu.io_debug_fpr_1,
        cpu.io_debug_fpr_2, cpu.io_debug_fpr_3,
        cpu.io_debug_fpr_4, cpu.io_debug_fpr_5,
        cpu.io_debug_fpr_6, cpu.io_debug_fpr_7,
        cpu.io_debug_fpr_8, cpu.io_debug_fpr_9,
        cpu.io_debug_fpr_10, cpu.io_debug_fpr_11,
        cpu.io_debug_fpr_12, cpu.io_debug_fpr_13,
        cpu.io_debug_fpr_14, cpu.io_debug_fpr_15,
        cpu.io_debug_fpr_16, cpu.io_debug_fpr_17,
        cpu.io_debug_fpr_18, cpu.io_debug_fpr_19,
        cpu.io_debug_fpr_20, cpu.io_debug_fpr_21,
        cpu.io_debug_fpr_22, cpu.io_debug_fpr_23,
        cpu.io_debug_fpr_24, cpu.io_debug_fpr_25,
        cpu.io_debug_fpr_26, cpu.io_debug_fpr_27,
        cpu.io_debug_fpr_28, cpu.io_debug_fpr_29,
        cpu.io_debug_fpr_30, cpu.io_debug_fpr_31,
    };
}

zircon::RVCPU stepReference(const zircon::RVCPU& snapshot, MemoryMap& memory,
                            uint32_t pc, uint32_t inst) {
    zircon::RVCPU lane = snapshot;
    lane.setPC(pc);
    const zircon::StepResult result = lane.step(inst);
    if (result.status != zircon::StepStatus::NeedMemory) {
        return lane;
    }

    const zircon::MemRequest& request = result.memReq;
    if (request.op == zircon::MemOp::Load) {
        lane.finishMemory(memory.read(request.addr, request.size));
    } else {
        // The DUT has already applied the architectural store to shared memory.
        lane.finishMemory();
    }
    return lane;
}

bool compareCommittedRegisters(
    const VCPU& cpu,
    const std::array<RegisterWrite, 64>& writes) {
    const auto gprs = gprState(cpu);
    const auto fprs = fprState(cpu);

    for (std::size_t encodedRd = 1; encodedRd < writes.size(); ++encodedRd) {
        const RegisterWrite& write = writes[encodedRd];
        if (!write.valid) {
            continue;
        }

        const bool isFpr = (encodedRd & 0x20u) != 0;
        const std::size_t index = encodedRd & 0x1fu;
        const uint32_t actual = isFpr ? fprs[index] : gprs[index];
        if (actual == write.expected) {
            continue;
        }

        std::cerr << "difftest " << (isFpr ? "FPR" : "GPR")
                  << " mismatch: cycle=" << std::dec << write.cycle
                  << " slot=" << write.slot
                  << " pc=0x" << std::hex << write.pc
                  << " inst=0x" << write.inst
                  << " reg=" << (isFpr ? 'f' : 'x') << std::dec << index
                  << " dut=0x" << std::hex << actual
                  << " ref=0x" << write.expected
                  << " wb=0x" << write.wbData << std::dec << "\n";
        return false;
    }
    return true;
}

void printRecentCommits(const std::deque<Commit>& commits) {
    std::cerr << "recent commits:\n";
    for (const Commit& commit : commits) {
        std::cerr << "  cycle " << std::dec << std::setw(8) << commit.cycle
                  << " pc=0x" << std::hex << std::setw(8) << std::setfill('0')
                  << commit.pc << " inst=0x" << std::setw(8) << commit.inst
                  << std::setfill(' ') << "\n";
    }
}

uint32_t hashMemory(const MemoryMap& memory, uint32_t address, std::size_t size) {
    uint32_t hash = 2166136261u;
    for (std::size_t offset = 0; offset < size; ++offset) {
        hash ^= memory.read(address + static_cast<uint32_t>(offset), 1) & 0xffu;
        hash *= 16777619u;
    }
    return hash;
}

} // namespace

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);
    if (argc < 2 || argc > 4) {
        std::cerr << "usage: " << argv[0]
                  << " <program.bin> [max-cycles] [memory-mib]\n";
        return 2;
    }

    try {
        const std::string imagePath = argv[1];
        const uint64_t maxCycles = argc >= 3 ? std::stoull(argv[2]) : kDefaultMaxCycles;
        const std::size_t memoryMiB = argc >= 4 ? std::stoull(argv[3]) : kDefaultMemoryMiB;
        const std::size_t memoryBytes = memoryMiB * 1024 * 1024;
        const bool traceFp = std::getenv("ZIRCON_TRACE_FP") != nullptr;
        const bool traceWb = std::getenv("ZIRCON_TRACE_WB") != nullptr;
        const bool difftest = std::getenv("ZIRCON_DIFFTEST") != nullptr;

        const std::vector<uint8_t> image = readFile(imagePath);
        MemoryMap memory(memoryBytes);
        memory.load(image);
        zircon::RVCPU reference(kMemoryBase);

        VCPU cpu;
        cpu.clock = 0;
        cpu.reset = 1;
        driveResetInputs(cpu);
        for (int cycle = 0; cycle < 5; ++cycle) {
            cpu.eval();
            cpu.clock = 1;
            cpu.eval();
            cpu.clock = 0;
            cpu.eval();
        }
        cpu.reset = 0;

        uint64_t retired = 0;
        std::deque<Commit> recent;
        for (uint64_t cycle = 1; cycle <= maxCycles; ++cycle) {
            cpu.clock = 0;
            settle(cpu, memory);
            commitStores(cpu, memory);

            // Sample the WB package that will write the register file on this edge.
            const auto valid = wbValid(cpu);
            const auto pcs = wbPc(cpu);
            const auto insts = wbInst(cpu);
            const auto rds = wbRd(cpu);
            const auto data = wbData(cpu);
            const zircon::RVCPU referenceSnapshot = reference;
            const uint32_t referencePacketPc = reference.getPC();

            cpu.clock = 1;
            cpu.eval();

            std::array<RegisterWrite, 64> writes{};
            std::vector<std::pair<uint32_t, uint32_t>> csrWrites;
            uint32_t nextReferencePc = referencePacketPc + 32;
            bool referenceAdvanced = false;
            bool haltSeen = false;
            for (std::size_t slot = 0; slot < valid.size(); ++slot) {
                if (!valid[slot]) {
                    continue;
                }
                ++retired;
                recent.push_back(Commit{cycle, pcs[slot], insts[slot]});
                if (recent.size() > 24) {
                    recent.pop_front();
                }
                const uint32_t opcode = insts[slot] & 0x7fu;
                if (traceWb && rds[slot] != 0) {
                    std::cerr << "wb-trace cycle=" << std::dec << cycle
                              << " slot=" << slot
                              << " pc=0x" << std::hex << pcs[slot]
                              << " inst=0x" << insts[slot]
                              << " rd=0x" << static_cast<unsigned>(rds[slot])
                              << " data=0x" << data[slot]
                              << std::dec << "\n";
                }
                if (traceFp && pcs[slot] >= 0x80000a80u && pcs[slot] <= 0x80000b5cu &&
                    (opcode == 0x07u || opcode == 0x27u || opcode == 0x53u)) {
                    std::cerr << "fp-trace cycle=" << std::dec << cycle
                              << " pc=0x" << std::hex << pcs[slot]
                              << " inst=0x" << insts[slot]
                              << " rd=0x" << static_cast<unsigned>(rds[slot])
                              << " wb=0x" << data[slot]
                              << " f14=0x" << cpu.io_debug_fpr_14
                              << " f15=0x" << cpu.io_debug_fpr_15
                              << std::dec << "\n";
                }
                if (insts[slot] == kHaltInstruction) {
                    haltSeen = true;
                    break;
                }
                if (difftest) {
                    const uint32_t expectedPc = referencePacketPc + static_cast<uint32_t>(slot * 4);
                    if (pcs[slot] != expectedPc) {
                        std::ostringstream message;
                        message << "difftest PC mismatch: ref=0x" << std::hex
                                << expectedPc << " dut=0x" << pcs[slot]
                                << " inst=0x" << insts[slot];
                        throw std::runtime_error(message.str());
                    }

                    const zircon::RVCPU lane = stepReference(
                        referenceSnapshot, memory, pcs[slot], insts[slot]);
                    referenceAdvanced = true;
                    nextReferencePc = lane.getPC();
                    if (rds[slot] != 0) {
                        const bool isFpr = (rds[slot] & 0x20u) != 0;
                        const uint32_t index = rds[slot] & 0x1fu;
                        const uint32_t expected = isFpr
                            ? lane.getFPRBits(index)
                            : lane.getGPR(index);
                        writes[rds[slot]] = RegisterWrite{
                            true, cycle, slot, pcs[slot], insts[slot], data[slot], expected
                        };
                    }
                    if ((insts[slot] & 0x7fu) == 0x73u) {
                        const uint32_t csr = insts[slot] >> 20;
                        csrWrites.emplace_back(csr, lane.getCSR(csr));
                    }
                }
            }

            // Let the register-file debug ports settle after the commit edge.
            cpu.clock = 0;
            cpu.eval();

            if (difftest && referenceAdvanced) {
                for (std::size_t encodedRd = 1; encodedRd < writes.size(); ++encodedRd) {
                    const RegisterWrite& write = writes[encodedRd];
                    if (!write.valid) {
                        continue;
                    }
                    const uint32_t index = static_cast<uint32_t>(encodedRd & 0x1fu);
                    if ((encodedRd & 0x20u) != 0) {
                        reference.setFPRBits(index, write.expected);
                    } else {
                        reference.setGPR(index, write.expected);
                    }
                }
                for (const auto& [csr, value] : csrWrites) {
                    reference.setCSR(csr, value);
                }
                reference.setPC(nextReferencePc);
            }

            if (difftest && !compareCommittedRegisters(cpu, writes)) {
                printRecentCommits(recent);
                cpu.final();
                return 4;
            }

            if (haltSeen) {
                const uint32_t exitCode = cpu.io_debug_gpr_10;
                const char* hashAddress = std::getenv("ZIRCON_HASH_ADDR");
                const char* hashSize = std::getenv("ZIRCON_HASH_SIZE");
                if (hashAddress != nullptr && hashSize != nullptr) {
                    const uint32_t address = static_cast<uint32_t>(std::stoul(hashAddress, nullptr, 0));
                    const std::size_t size = std::stoull(hashSize, nullptr, 0);
                    std::cout << "memory-hash addr=0x" << std::hex << address
                              << " size=0x" << size
                              << " fnv1a=0x" << hashMemory(memory, address, size)
                              << std::dec << "\n";
                }
                std::cout << "\ncycles=" << cycle << " retired=" << retired
                          << " a0=" << exitCode << "\n";
                printPerformanceMetrics(
                    cpu.io_debug_perfCycles,
                    cpu.io_debug_perfEffectiveInstructions,
                    cpu.io_debug_perfExecutedPackets);
                cpu.final();
                return exitCode == 0 ? 0 : 1;
            }

        }

        std::cerr << "simulation timed out after " << maxCycles
                  << " cycles, retired=" << retired
                  << " fetch_pc=0x" << std::hex << cpu.io_imem_pc
                  << " a0=0x" << cpu.io_debug_gpr_10 << std::dec << "\n";
        printPerformanceMetrics(
            cpu.io_debug_perfCycles,
            cpu.io_debug_perfEffectiveInstructions,
            cpu.io_debug_perfExecutedPackets);
        printRecentCommits(recent);
        cpu.final();
        return 3;
    } catch (const std::exception& error) {
        std::cerr << "simulation error: " << error.what() << "\n";
        return 2;
    }
}
