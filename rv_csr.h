#pragma once

#define CSR_RW_BITS(csr) (((csr) >> 10) & 0b11)
#define CSR_PR_BITS(csr) (((csr) >> 8) & 0b11)
#define CSR_IDX(csr) ((csr) & 0xFF)

#define CSR_U_PRIV 0b00
#define CSR_S_PRIV 0b01
#define CSR_M_PRIV 0b11

#define CSR_READ_ONLY 0b11

#define CSR_COUNT 4096

#define CSR_Type u32

//CSR X-Lists

#define CSR_LIST_M_InfoRegisters \
CSR_DEF(0xF11, mvendorid), \
CSR_DEF(0xF12, marchid), \
CSR_DEF(0xF13, mimpid), \
CSR_DEF(0xF14, mhartid), \
CSR_DEF(0xF15, mconfigptr), \

#define CSR_LIST_M_TrapSetup \
CSR_DEF(0x300, mstatus), \
CSR_DEF(0x301, misa), \
CSR_DEF(0x302, medeleg), \
CSR_DEF(0x303, mideleg), \
CSR_DEF(0x304, mie), \
CSR_DEF(0x305, mtvec), \
CSR_DEF(0x306, mcounteren), \
CSR_DEF(0x310, mstatush), \
CSR_DEF(0x312, medelegh), \

#define CSR_LIST_M_TrapHandling \
CSR_DEF(0x340, mscratch),  \
CSR_DEF(0x341, mepc),  \
CSR_DEF(0x342, mcause),  \
CSR_DEF(0x343, mtval),  \
CSR_DEF(0x344, mip),  \
CSR_DEF(0x34A, mtinst),  \
CSR_DEF(0x34B, mtval2),  \

#define CSR_LIST_M_Configuration \
CSR_DEF(0x30A, menvcfg), \
CSR_DEF(0x31A, menvcfgh), \
CSR_DEF(0x747, mseccfg), \
CSR_DEF(0x757, mseccfgh), \

#define CSR_LIST_M_MemoryProtection \
CSR_DEF(0x3A0, pmpcfg0), \
CSR_DEF(0x3A1, pmpcfg1), \
CSR_DEF(0x3A2, pmpcfg2), \
CSR_DEF(0x3A3, pmpcfg3), \
CSR_DEF(0x3A4, pmpcfg4), \
CSR_DEF(0x3A5, pmpcfg5), \
CSR_DEF(0x3A6, pmpcfg6), \
CSR_DEF(0x3A7, pmpcfg7), \
CSR_DEF(0x3A8, pmpcfg8), \
CSR_DEF(0x3A9, pmpcfg9), \
CSR_DEF(0x3AA, pmpcfg10), \
CSR_DEF(0x3AB, pmpcfg11), \
CSR_DEF(0x3AC, pmpcfg12), \
CSR_DEF(0x3AD, pmpcfg13), \
CSR_DEF(0x3AE, pmpcfg14), \
CSR_DEF(0x3AF, pmpcfg15), \
CSR_DEF(0x3B0, pmpaddr0), \
CSR_DEF(0x3B1, pmpaddr1), \
CSR_DEF(0x3B2, pmpaddr2), \
CSR_DEF(0x3B3, pmpaddr3), \
CSR_DEF(0x3B4, pmpaddr4), \
CSR_DEF(0x3B5, pmpaddr5), \
CSR_DEF(0x3B6, pmpaddr6), \
CSR_DEF(0x3B7, pmpaddr7), \
CSR_DEF(0x3B8, pmpaddr8), \
CSR_DEF(0x3B9, pmpaddr9), \
CSR_DEF(0x3BA, pmpaddr10), \
CSR_DEF(0x3BB, pmpaddr11), \
CSR_DEF(0x3BC, pmpaddr12), \
CSR_DEF(0x3BD, pmpaddr13), \
CSR_DEF(0x3BE, pmpaddr14), \
CSR_DEF(0x3BF, pmpaddr15), \
CSR_DEF(0x3C0, pmpaddr16), \
CSR_DEF(0x3C1, pmpaddr17), \
CSR_DEF(0x3C2, pmpaddr18), \
CSR_DEF(0x3C3, pmpaddr19), \
CSR_DEF(0x3C4, pmpaddr20), \
CSR_DEF(0x3C5, pmpaddr21), \
CSR_DEF(0x3C6, pmpaddr22), \
CSR_DEF(0x3C7, pmpaddr23), \
CSR_DEF(0x3C8, pmpaddr24), \
CSR_DEF(0x3C9, pmpaddr25), \
CSR_DEF(0x3CA, pmpaddr26), \
CSR_DEF(0x3CB, pmpaddr27), \
CSR_DEF(0x3CC, pmpaddr28), \
CSR_DEF(0x3CD, pmpaddr29), \
CSR_DEF(0x3CE, pmpaddr30), \
CSR_DEF(0x3CF, pmpaddr31), \
CSR_DEF(0x3D0, pmpaddr32), \
CSR_DEF(0x3D1, pmpaddr33), \
CSR_DEF(0x3D2, pmpaddr34), \
CSR_DEF(0x3D3, pmpaddr35), \
CSR_DEF(0x3D4, pmpaddr36), \
CSR_DEF(0x3D5, pmpaddr37), \
CSR_DEF(0x3D6, pmpaddr38), \
CSR_DEF(0x3D7, pmpaddr39), \
CSR_DEF(0x3D8, pmpaddr40), \
CSR_DEF(0x3D9, pmpaddr41), \
CSR_DEF(0x3DA, pmpaddr42), \
CSR_DEF(0x3DB, pmpaddr43), \
CSR_DEF(0x3DC, pmpaddr44), \
CSR_DEF(0x3DD, pmpaddr45), \
CSR_DEF(0x3DE, pmpaddr46), \
CSR_DEF(0x3DF, pmpaddr47), \
CSR_DEF(0x3E0, pmpaddr48), \
CSR_DEF(0x3E1, pmpaddr49), \
CSR_DEF(0x3E2, pmpaddr50), \
CSR_DEF(0x3E3, pmpaddr51), \
CSR_DEF(0x3E4, pmpaddr52), \
CSR_DEF(0x3E5, pmpaddr53), \
CSR_DEF(0x3E6, pmpaddr54), \
CSR_DEF(0x3E7, pmpaddr55), \
CSR_DEF(0x3E8, pmpaddr56), \
CSR_DEF(0x3E9, pmpaddr57), \
CSR_DEF(0x3EA, pmpaddr58), \
CSR_DEF(0x3EB, pmpaddr59), \
CSR_DEF(0x3EC, pmpaddr60), \
CSR_DEF(0x3ED, pmpaddr61), \
CSR_DEF(0x3EE, pmpaddr62), \
CSR_DEF(0x3EF, pmpaddr63), \

#define CSR_LIST_M_StateEnable \
CSR_DEF(0x30C, mstateen0), \
CSR_DEF(0x30D, mstateen1), \
CSR_DEF(0x30E, mstateen2), \
CSR_DEF(0x30F, mstateen3), \
CSR_DEF(0x31C, mstateen0h), \
CSR_DEF(0x31D, mstateen1h), \
CSR_DEF(0x31E, mstateen2h), \
CSR_DEF(0x31F, mstateen3h), \

#define CSR_LIST_M_NonMaskableInterruptHandling \
CSR_DEF(0x740, mnscratch), \
CSR_DEF(0x741, mnepc), \
CSR_DEF(0x742, mncause), \
CSR_DEF(0x744, mnstatus), \

#define CSR_LIST_M_CounterTimers \
CSR_DEF(0xB00, mcycle), \
CSR_DEF(0xB02, minstret), \
CSR_DEF(0xB03, mhpmcounter3), \
CSR_DEF(0xB04, mhpmcounter4), \
CSR_DEF(0xB05, mhpmcounter5), \
CSR_DEF(0xB06, mhpmcounter6), \
CSR_DEF(0xB07, mhpmcounter7), \
CSR_DEF(0xB08, mhpmcounter8), \
CSR_DEF(0xB09, mhpmcounter9), \
CSR_DEF(0xB0A, mhpmcounter10), \
CSR_DEF(0xB0B, mhpmcounter11), \
CSR_DEF(0xB0C, mhpmcounter12), \
CSR_DEF(0xB0D, mhpmcounter13), \
CSR_DEF(0xB0E, mhpmcounter14), \
CSR_DEF(0xB0F, mhpmcounter15), \
CSR_DEF(0xB10, mhpmcounter16), \
CSR_DEF(0xB11, mhpmcounter17), \
CSR_DEF(0xB12, mhpmcounter18), \
CSR_DEF(0xB13, mhpmcounter19), \
CSR_DEF(0xB14, mhpmcounter20), \
CSR_DEF(0xB15, mhpmcounter21), \
CSR_DEF(0xB16, mhpmcounter22), \
CSR_DEF(0xB17, mhpmcounter23), \
CSR_DEF(0xB18, mhpmcounter24), \
CSR_DEF(0xB19, mhpmcounter25), \
CSR_DEF(0xB1A, mhpmcounter26), \
CSR_DEF(0xB1B, mhpmcounter27), \
CSR_DEF(0xB1C, mhpmcounter28), \
CSR_DEF(0xB1D, mhpmcounter29), \
CSR_DEF(0xB1E, mhpmcounter30), \
CSR_DEF(0xB1F, mhpmcounter31), \
\
CSR_DEF(0xB80, mcycleh), \
CSR_DEF(0xB82, minstreth), \
CSR_DEF(0xB83, mhpmcounter3h), \
CSR_DEF(0xB84, mhpmcounter4h), \
CSR_DEF(0xB85, mhpmcounter5h), \
CSR_DEF(0xB86, mhpmcounter6h), \
CSR_DEF(0xB87, mhpmcounter7h), \
CSR_DEF(0xB88, mhpmcounter8h), \
CSR_DEF(0xB89, mhpmcounter9h), \
CSR_DEF(0xB8A, mhpmcounter10h), \
CSR_DEF(0xB8B, mhpmcounter11h), \
CSR_DEF(0xB8C, mhpmcounter12h), \
CSR_DEF(0xB8D, mhpmcounter13h), \
CSR_DEF(0xB8E, mhpmcounter14h), \
CSR_DEF(0xB8F, mhpmcounter15h), \
CSR_DEF(0xB90, mhpmcounter16h), \
CSR_DEF(0xB91, mhpmcounter17h), \
CSR_DEF(0xB92, mhpmcounter18h), \
CSR_DEF(0xB93, mhpmcounter19h), \
CSR_DEF(0xB94, mhpmcounter20h), \
CSR_DEF(0xB95, mhpmcounter21h), \
CSR_DEF(0xB96, mhpmcounter22h), \
CSR_DEF(0xB97, mhpmcounter23h), \
CSR_DEF(0xB98, mhpmcounter24h), \
CSR_DEF(0xB99, mhpmcounter25h), \
CSR_DEF(0xB9A, mhpmcounter26h), \
CSR_DEF(0xB9B, mhpmcounter27h), \
CSR_DEF(0xB9C, mhpmcounter28h), \
CSR_DEF(0xB9D, mhpmcounter29h), \
CSR_DEF(0xB9E, mhpmcounter30h), \
CSR_DEF(0xB9F, mhpmcounter31h), \

#define CSR_LIST_M_CounterSetup \
CSR_DEF(0x320, mhpmevent0), \
CSR_DEF(0x321, mhpmevent1), \
CSR_DEF(0x322, mhpmevent2), \
CSR_DEF(0x323, mhpmevent3), \
CSR_DEF(0x324, mhpmevent4), \
CSR_DEF(0x325, mhpmevent5), \
CSR_DEF(0x326, mhpmevent6), \
CSR_DEF(0x327, mhpmevent7), \
CSR_DEF(0x328, mhpmevent8), \
CSR_DEF(0x329, mhpmevent9), \
CSR_DEF(0x32A, mhpmevent10), \
CSR_DEF(0x32B, mhpmevent11), \
CSR_DEF(0x32C, mhpmevent12), \
CSR_DEF(0x32D, mhpmevent13), \
CSR_DEF(0x32E, mhpmevent14), \
CSR_DEF(0x32F, mhpmevent15), \
CSR_DEF(0x330, mhpmevent16), \
CSR_DEF(0x331, mhpmevent17), \
CSR_DEF(0x332, mhpmevent18), \
CSR_DEF(0x333, mhpmevent19), \
CSR_DEF(0x334, mhpmevent20), \
CSR_DEF(0x335, mhpmevent21), \
CSR_DEF(0x336, mhpmevent22), \
CSR_DEF(0x337, mhpmevent23), \
CSR_DEF(0x338, mhpmevent24), \
CSR_DEF(0x339, mhpmevent25), \
CSR_DEF(0x33A, mhpmevent26), \
CSR_DEF(0x33B, mhpmevent27), \
CSR_DEF(0x33C, mhpmevent28), \
CSR_DEF(0x33D, mhpmevent29), \
CSR_DEF(0x33E, mhpmevent30), \
CSR_DEF(0x33F, mhpmevent31), \
\
CSR_DEF(0x723, mhpmevent3h), \
CSR_DEF(0x724, mhpmevent4h), \
CSR_DEF(0x725, mhpmevent5h), \
CSR_DEF(0x726, mhpmevent6h), \
CSR_DEF(0x727, mhpmevent7h), \
CSR_DEF(0x728, mhpmevent8h), \
CSR_DEF(0x729, mhpmevent9h), \
CSR_DEF(0x72a, mhpmevent10h), \
CSR_DEF(0x72b, mhpmevent11h), \
CSR_DEF(0x72c, mhpmevent12h), \
CSR_DEF(0x72d, mhpmevent13h), \
CSR_DEF(0x72e, mhpmevent14h), \
CSR_DEF(0x72f, mhpmevent15h), \
CSR_DEF(0x730, mhpmevent16h), \
CSR_DEF(0x731, mhpmevent17h), \
CSR_DEF(0x732, mhpmevent18h), \
CSR_DEF(0x733, mhpmevent19h), \
CSR_DEF(0x734, mhpmevent20h), \
CSR_DEF(0x735, mhpmevent21h), \
CSR_DEF(0x736, mhpmevent22h), \
CSR_DEF(0x737, mhpmevent23h), \
CSR_DEF(0x738, mhpmevent24h), \
CSR_DEF(0x739, mhpmevent25h), \
CSR_DEF(0x73a, mhpmevent26h), \
CSR_DEF(0x73b, mhpmevent27h), \
CSR_DEF(0x73c, mhpmevent28h), \
CSR_DEF(0x73d, mhpmevent29h), \
CSR_DEF(0x73e, mhpmevent30h), \
CSR_DEF(0x73f, mhpmevent31h), \

#define CSR_LIST_M_DebugTrace \
CSR_DEF(0x7A0, tselect), \
CSR_DEF(0x7A1, tdata1), \
CSR_DEF(0x7A2, tdata2), \
CSR_DEF(0x7A3, tdata3), \
CSR_DEF(0x7A8, mcontext), \

#define CSR_LIST_M_All \
CSR_LIST_M_InfoRegisters \
CSR_LIST_M_TrapSetup \
CSR_LIST_M_TrapHandling \
CSR_LIST_M_Configuration \
CSR_LIST_M_MemoryProtection \
CSR_LIST_M_StateEnable \
CSR_LIST_M_NonMaskableInterruptHandling \
CSR_LIST_M_CounterTimers \
CSR_LIST_M_CounterSetup \
CSR_LIST_M_DebugTrace

#define CSR_LIST_TEST \
CSR_DEF(0x01, mtest1), \
CSR_DEF(0x02, mtest2), 

// CSR X-List Instances

#define CSR_DEF(address, name) [address] = #name
static const char* csr_str[CSR_COUNT] = {
CSR_LIST_M_All
};
#undef CSR_DEF

#define CSR_DEF(address, name) CSR_##name = address
typedef enum {
CSR_LIST_M_All
} CSR_Name;
#undef CSR_DEF