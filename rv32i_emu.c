#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include "common.h"

#define XLEN 32
#define IALIGN 32
#define PAGE_SIZE 4096

#define BIT(at) (1 << (at))
#define SIGN_EXTEND(x, sign_at) ((x) | ( (~(BIT(sign_at)-1)) & (-((BIT(sign_at) & (x)) > 0))))
#define SAR(x, n) (((int)(x)) >> ((int)(n)))

////
//  Register File
//
typedef union {
    u32 regs[32];
    struct
    {
        u32 x0;  u32 x1;  u32 x2;  u32 x3;  u32 x4;  u32 x5;  u32 x6;  u32 x7;  u32 x8;  u32 x9;  u32 x10; u32 x11; u32 x12; u32 x13; u32 x14; u32 x15;
        u32 x16; u32 x17; u32 x18; u32 x19; u32 x20; u32 x21; u32 x22; u32 x23; u32 x24; u32 x25; u32 x26; u32 x27; u32 x28; u32 x29; u32 x30; u32 x31;
    };
    struct
    {
        u32 zero;                   // Zero Register
        u32 ra;                     // Return Address (Caller Saved)
        u32 sp;                     // Stack Pointer (Callee Saved)
        u32 gp;                     // Global Pointer 
        u32 tp;                     // Thread Pointer
        u32 t0;                     // Temporary/Alternate Link Register (Caller Saved)
        u32 t1, t2;                 // Temporaries (Caller Saved)
        union { u32 s0; u32 fp; };  // (Callee) Saved register/ Frame pointer
        u32 s1;                     // (Callee) Saved register
        u32 a0, a1;                 // Function arguments / Return values (Caller Saved)
        u32 a2, a3, a4; 
        u32 a5, a6, a7;             // Function arguments (Caller Saved)
        u32 s2, s3, s4, s5, s6;   
        u32 s7, s8, s9, s10, s11;   // (Callee) Saved registers
        u32 t3, t4, t5, t6;         // Temporaries (Caller Saved)
    };
} Registers;

STATIC_ASSERT(sizeof(Registers) == sizeof(u32) * 32);

#define RegRead(reg_bank, reg) ((offsetof(Registers, reg) == 0) ? ((u32)0) : ((reg_bank).reg))
#define RegWrite(reg_bank, reg, val) if (offsetof(Registers, reg) != 0) { (reg_bank).reg = (val); }

#define RegReadI(reg_bank, idx) (((idx) == 0) ? ((u32)0) : ((reg_bank).regs[idx]))
#define RegWriteI(reg_bank, idx, val) if (idx != 0) { (reg_bank).regs[idx] = (val); }

const char* _regaddr_to_num_str[] = {
    "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", "x11", "x12", "x13", "x14", "x15",
    "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31",
};

const char* _regaddr_to_str[] = {
    "zero",                   
    "ra",                     
    "sp",                     
    "gp",                     
    "tp",                     
    "t0",                     
    "t1", "t2",                 
    "s0", 
    "s1",                     
    "a0", "a1",                 
    "a2", "a3", "a4", 
    "a5", "a6", "a7",             
    "s2", "s3", "s4", "s5", "s6",   
    "s7", "s8", "s9", "s10", "s11",   
    "t3", "t4", "t5", "t6",         
};

#define REGADDR_TO_STR(reg) (((reg) >= ARRAY_COUNT(_regaddr_to_str)) ? "UNKNOWN" : _regaddr_to_str[reg])
#define REGADDR_TO_NUM_STR(reg) (((reg) >= ARRAY_COUNT(_regaddr_to_num_str)) ? "UNKNOWN" : _regaddr_to_num_str[reg])

////
//  Instructions
//

#define OPCODE_MASK 0b1111111
#define DEC_OPCODE(inst) ((inst) & OPCODE_MASK)

#define OPC_LOAD        0b0000011
#define OPC_STORE       0b0100011
#define OPC_MADD        0b1000011
#define OPC_BRANCH      0b1100011
#define OPC_LOAD_FP     0b0000111
#define OPC_STORE_FP    0b0100111
#define OPC_MSUB        0b1000111
#define OPC_JALR        0b1100111
#define OPC_NMSUB       0b1001011
#define OPC_MISC_MEM    0b0001111
#define OPC_AMO         0b0101111
#define OPC_NMADD       0b1001111
#define OPC_JAL         0b1101111
#define OPC_OP_IMM      0b0010011
#define OPC_OP          0b0110011
#define OPC_OP_FP       0b1010011
#define OPC_SYSTEM      0b1110011
#define OPC_AUIPC       0b0010111
#define OPC_LUI         0b0110111
#define OPC_OP_IMM_32   0b0011011
#define OPC_OP_32       0b0111011

#define OP_LUI      0b0110111
#define OP_AUIPC    0b0010111
#define OP_JAL      0b1101111
#define OP_JALR     0b1100111
#define OP_BEQ      0b1100011
#define OP_BNE      0b1100011
#define OP_BLT      0b1100011
#define OP_BGE      0b1100011
#define OP_BLTU     0b1100011
#define OP_BGEU     0b1100011
#define OP_LB       0b0000011
#define OP_LH       0b0000011
#define OP_LW       0b0000011
#define OP_LBU      0b0000011
#define OP_LHU      0b0000011
#define OP_SB       0b0100011
#define OP_SH       0b0100011
#define OP_SW       0b0100011
#define OP_ADDI     0b0010011
#define OP_SLTI     0b0010011
#define OP_SLTIU    0b0010011
#define OP_XORI     0b0010011
#define OP_ORI      0b0010011
#define OP_ANDI     0b0010011
#define OP_SLLI     0b0010011
#define OP_SRLI     0b0010011
#define OP_SRAI     0b0010011
#define OP_ADD      0b0110011
#define OP_SUB      0b0110011
#define OP_SLL      0b0110011
#define OP_SLT      0b0110011
#define OP_SLTU     0b0110011
#define OP_XOR      0b0110011
#define OP_SRL      0b0110011
#define OP_SRA      0b0110011
#define OP_OR       0b0110011
#define OP_AND      0b0110011
#define OP_FENCE    0b0001111
#define OP_ECALL    0b1110011
#define OP_EBREAK   0b1110011

const char* _opcode_to_str[] = { //TODO: Invalid, repeated opcodes, not enough info to get op type
    [OPC_LOAD] = "LOAD",
    [OPC_STORE] = "STORE",
    [OPC_MADD] = "MADD",
    [OPC_BRANCH] = "BRANCH",
    [OPC_LOAD_FP] = "LOAD_FP",
    [OPC_STORE_FP] = "STORE_FP",
    [OPC_MSUB] = "MSUB",
    [OPC_NMSUB] = "NMSUB",
    [OPC_MISC_MEM] = "MISC_MEM",
    [OPC_AMO] = "AMO",
    [OPC_NMADD] = "NMADD",
    [OPC_JAL] = "JAL",
    [OPC_JALR] = "JALR",
    [OPC_OP_IMM] = "OP_IMM",
    [OPC_OP] = "OP",
    [OPC_OP_FP] = "OP_FP",
    [OPC_SYSTEM] = "SYSTEM",
    [OPC_AUIPC] = "AUIPC",
    [OPC_LUI] = "LUI",
    [OPC_OP_IMM_32] = "OP_IMM_32",
    [OPC_OP_32] = "OP_32",
};
#define OPCODE_TO_STR(opcode) (((opcode) >= ARRAY_COUNT(_opcode_to_str) || _opcode_to_str[opcode] == NULL) ? "UNKNOWN" : _opcode_to_str[opcode])

#define RD_MASK     0b00000000000000000000111110000000
#define RD_SHFT 7
#define DEC_RD(inst) (((inst) & RD_MASK) >> RD_SHFT)

#define RS1_MASK    0b00000000000011111000000000000000
#define RS1_SHFT 15
#define DEC_RS1(inst) (((inst) & RS1_MASK) >> RS1_SHFT)

#define RS2_MASK    0b00000001111100000000000000000000
#define RS2_SHFT 20
#define DEC_RS2(inst) (((inst) & RS2_MASK) >> RS2_SHFT)
#define DEC_SHAMT(inst) DEC_RS2(inst)

// Funct7
#define FUNC7_SLLI  0b0000000
#define FUNC7_SRLI  0b0000000
#define FUNC7_SRAI  0b0100000
#define FUNC7_ADD   0b0000000
#define FUNC7_SUB   0b0100000
#define FUNC7_SLL   0b0000000
#define FUNC7_SLT   0b0000000
#define FUNC7_SLTU  0b0000000
#define FUNC7_XOR   0b0000000
#define FUNC7_SRL   0b0000000
#define FUNC7_SRA   0b0100000
#define FUNC7_OR    0b0000000
#define FUNC7_AND   0b0000000

#define FUNCT7_MASK 0b11111110000000000000000000000000
#define FUNCT7_SHFT 25
#define DEC_FUNCT7(inst) (((inst) & FUNCT7_MASK) >> FUNCT7_SHFT)

// U-Imm
#define UIMM_MASK   0b11111111111111111111000000000000
#define UIMM_SHFT 12
#define DEC_UIMM(inst) (((inst) & UIMM_MASK) >> UIMM_SHFT)
#define DEC_UIMM_VALUE(inst) ((inst) & UIMM_MASK)

// I-Imm
#define IIMM_MASK   0b11111111111100000000000000000000
#define IIMM_SHFT 20
#define DEC_IIMM(inst) (((inst) & IIMM_MASK) >> IIMM_SHFT)
//#define DEC_IIMM_VALUE(inst) SIGN_EXTEND(DEC_IIMM(inst), 11)
#define DEC_IIMM_VALUE(inst) ((i32)SAR(inst, IIMM_SHFT))

// S-Imm
#define SIMM_MASK FUNCT7_MASK
#define SIMM_SHFT 25
#define DEC_SIMM_VALUE(inst) ((i32)(SAR((inst) & FUNCT7_MASK, 20) | (((inst) & RD_MASK) >> 7)))
#define S_IMM_TO_HI(imm) (((imm) >> 5) & 0b1111111)
#define S_IMM_TO_LO(imm) ((imm) & 0b11111)

// B-Imm
#define DEC_BIMM(inst)         ((((inst) >> 19)   & 0b1000000000000) | \
                                (((inst) << 4)    & 0b0100000000000) | \
                                (((inst) >> 20)   & 0b0011111100000) | \
                                (((inst) >> 7)    & 0b0000000011110))
#define DEC_BIMM_VALUE(inst) ((i32)SIGN_EXTEND(DEC_BIMM(inst), 12))
#define B_IMM_TO_HI(imm) ((((imm) >> 6) & 0b1000000) | (((imm) >> 5) & 0b111111))
#define B_IMM_TO_LO(imm) (((imm) & 0b11110) | (((imm) >> 11) & 1) )

// J-Imm
#define DEC_JIMM(inst)         ((((inst) >> 11)   & 0b100000000000000000000) | \
                                (((inst))         & 0b011111111000000000000) | \
                                (((inst) >> 9)    & 0b000000000100000000000) | \
                                (((inst) >> 20)   & 0b000000000011111111110))
#define DEC_JIMM_VALUE(inst) ((i32)SIGN_EXTEND(DEC_JIMM(inst), 20))

// ALU Op
#define ALUOP_JALR    0b000
#define ALUOP_BEQ     0b000
#define ALUOP_BNE     0b001
#define ALUOP_BLT     0b100
#define ALUOP_BGE     0b101
#define ALUOP_BLTU    0b110
#define ALUOP_BGEU    0b111
#define ALUOP_LB      0b000
#define ALUOP_LH      0b001
#define ALUOP_LW      0b010
#define ALUOP_LBU     0b100
#define ALUOP_LHU     0b101
#define ALUOP_SB      0b000
#define ALUOP_SH      0b001
#define ALUOP_SW      0b010
#define ALUOP_ADDI    0b000
#define ALUOP_SLTI    0b010
#define ALUOP_SLTIU   0b011
#define ALUOP_XORI    0b100
#define ALUOP_ORI     0b110
#define ALUOP_ANDI    0b111
#define ALUOP_SLLI    0b001
#define ALUOP_SRLI    0b101
#define ALUOP_SRAI    0b101
#define ALUOP_ADD     0b000
#define ALUOP_SUB     0b000
#define ALUOP_SLL     0b001
#define ALUOP_SLT     0b010
#define ALUOP_SLTU    0b011
#define ALUOP_XOR     0b100
#define ALUOP_SRL     0b101
#define ALUOP_SRA     0b101
#define ALUOP_OR      0b110
#define ALUOP_AND     0b111
#define ALUOP_FENCE   0b000
#define ALUOP_ECALL   0b000
#define ALUOP_EBREAK  0b000

#define ALUOP_MASK  0b0000000000000000111000000000000
#define ALUOP_SHFT 12
#define DEC_ALUOP(inst) (((inst) & ALUOP_MASK) >> ALUOP_SHFT)

// Macro Assembler
#define ENC_R_TYPE(rd, rs1, rs2, func7, func3, opcode) (((func7) << FUNCT7_SHFT) | ((rs2) << RS2_SHFT) | ((rs1) << RS1_SHFT) | ((func3) << ALUOP_SHFT) | ((rd) << RD_SHFT) | (opcode))
#define ENC_I_TYPE(rd, rs1, imm, func3, opcode) (((imm) << IIMM_SHFT) | ((rs1) << RS1_SHFT) | ((func3) << ALUOP_SHFT) | ((rd) << RD_SHFT) | (opcode))
#define ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, func3, opcode) (((imm_hi) << SIMM_SHFT) | ((rs2) << RS2_SHFT) | ((rs1) << RS1_SHFT) | ((func3) << ALUOP_SHFT) | ((imm_lo) << RD_SHFT) | (opcode))
#define ENC_U_TYPE(rd, imm, opcode) (((imm) << UIMM_SHFT) | ((rd) << RD_SHFT) | (opcode))
#define ENC_SH_TYPE(rd, rs1, shamt, func7, func3, opcode) ENC_R_TYPE(rd, rs1, shamt, func7, func3, opcode)

#define CHECK_J_IMM(imm) assert(((imm) & 1) == 0 && "Jump immediate must be aligned to 2 bytes!")

#define LUI(rd, imm)   ENC_U_TYPE(rd, imm, OP_LUI)
#define AUIPC(rd, imm) ENC_U_TYPE(rd, imm, OP_AUIPC)

#define _JAL(rd, imm)      ( (((imm) << 11) & 0x80000000) | (((imm) & 0b11111111110) << 20) | (((imm) & 0b100000000000) << 9) | ((imm) & 0b11111111000000000000) | ((rd) << RD_SHFT) | (OP_JAL))
#define JAL(rd, imm) (CHECK_J_IMM(imm), _JAL(rd, imm))
#define JALR(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_JALR, OP_JALR)

#define _BEQ(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BEQ, OP_BEQ)
#define _BNE(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BNE, OP_BNE)
#define _BLT(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BLT, OP_BLT)
#define _BGE(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BGE, OP_BGE)
#define _BLTU(rs1, rs2, imm_hi, imm_lo) ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BLTU, OP_BLTU)
#define _BGEU(rs1, rs2, imm_hi, imm_lo) ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_BGEU, OP_BGEU)

#define BEQ(rs1, rs2, imm)  (CHECK_J_IMM(imm), _BEQ(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))
#define BNE(rs1, rs2, imm)  (CHECK_J_IMM(imm), _BNE(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))
#define BLT(rs1, rs2, imm)  (CHECK_J_IMM(imm), _BLT(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))
#define BGE(rs1, rs2, imm)  (CHECK_J_IMM(imm), _BGE(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))
#define BLTU(rs1, rs2, imm) (CHECK_J_IMM(imm), _BLTU(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))
#define BGEU(rs1, rs2, imm) (CHECK_J_IMM(imm), _BGEU(rs1, rs2, B_IMM_TO_HI(imm), B_IMM_TO_LO(imm)))

#define LB(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_LB, OP_LB)
#define LH(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_LH, OP_LH)
#define LW(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_LW, OP_LW)
#define LBU(rd, rs1, imm) ENC_I_TYPE(rd, rs1, imm, ALUOP_LBU, OP_LBU)
#define LHU(rd, rs1, imm) ENC_I_TYPE(rd, rs1, imm, ALUOP_LHU, OP_LHU)

#define _SB(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_SB, OP_SB)
#define _SH(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_SH, OP_SH)
#define _SW(rs1, rs2, imm_hi, imm_lo)  ENC_S_TYPE(rs1, rs2, imm_hi, imm_lo, ALUOP_SW, OP_SW)

#define SB(rs1, rs2, imm) _SB(rs1, rs2, S_IMM_TO_HI(imm), S_IMM_TO_LO(imm))
#define SH(rs1, rs2, imm) _SH(rs1, rs2, S_IMM_TO_HI(imm), S_IMM_TO_LO(imm))
#define SW(rs1, rs2, imm) _SW(rs1, rs2, S_IMM_TO_HI(imm), S_IMM_TO_LO(imm))

#define ADDI(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_ADDI, OP_ADDI)
#define SLTI(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_SLTI, OP_SLTI)
#define SLTIU(rd, rs1, imm) ENC_I_TYPE(rd, rs1, imm, ALUOP_SLTIU, OP_SLTIU)
#define XORI(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_XORI, OP_XORI)
#define ORI(rd, rs1, imm)   ENC_I_TYPE(rd, rs1, imm, ALUOP_ORI, OP_ORI)
#define ANDI(rd, rs1, imm)  ENC_I_TYPE(rd, rs1, imm, ALUOP_ANDI, OP_ANDI)

#define SLLI(rd, rs1, shamt) ENC_SH_TYPE(rd, rs1, shamt, FUNC7_SLLI, ALUOP_SLLI, OP_SLLI)
#define SRLI(rd, rs1, shamt) ENC_SH_TYPE(rd, rs1, shamt, FUNC7_SRLI, ALUOP_SRLI, OP_SRLI)
#define SRAI(rd, rs1, shamt) ENC_SH_TYPE(rd, rs1, shamt, FUNC7_SRAI, ALUOP_SRAI, OP_SRAI)

#define ADD(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_ADD, ALUOP_ADD, OP_ADD)
#define SUB(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SUB, ALUOP_SUB, OP_SUB)
#define SLL(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SLL, ALUOP_SLL, OP_SLL)
#define SLT(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SLT, ALUOP_SLT, OP_SLT)
#define SLTU(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SLTU, ALUOP_SLTU, OP_SLTU)
#define XOR(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_XOR, ALUOP_XOR, OP_XOR)
#define SRL(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SRL, ALUOP_SRL, OP_SRL)
#define SRA(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_SRA, ALUOP_SRA, OP_SRA)
#define OR(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_OR, ALUOP_OR, OP_OR)
#define AND(rd, rs1, rs2) ENC_R_TYPE(rd, rs1, rs2, FUNC7_AND, ALUOP_AND, OP_AND)

#define FENCE(rd, rs1, imm) ENC_I_TYPE(rd, rs1, imm, ALUOP_FENCE, OP_FENCE)
#define ECALL()             ENC_I_TYPE(0, 0, 0, ALUOP_ECALL, OP_ECALL) 
#define EBREAK()            ENC_I_TYPE(0, 0, 1, ALUOP_EBREAK, OP_EBREAK) 

////
// Pseudo-instructions
//

#define LI(rd, imm) LUI(rd, ((u32)(imm) >> 12)+(((u32)(imm) & 0xFFF) >= 0x800)), ADDI(rd, rd, (imm) & 0xfff)
#define NOP() ADDI(0, 0, 0)
#define NOT(rd, rs) XORI(rd, rs, -1)
#define NEG(rd, rs) SUB(rd, 0, rs)
#define RET() JALR(0, 1, 0)

////
// Control Status Registers
//

#include "rv_csr.h"

////
// RISC-V Capabilities and Extensions
//

typedef enum {
    CAP_Reg32 = 0 << 0,
    CAP_Reg64 = 1 << 0,
    CAP_Reg128 = 2 << 0,
    CAP_RegSize = 3 << 0,

    CAP_UnalignedMem = 1 << 2,
    CAP_Privileges = 1 << 3,
    CAP_ExtM = 1 << 4,
    CAP_ExtA = 1 << 5,
    CAP_ExtZicsr = 1 << 6,
    CAP_Counters = 1 << 7,
    CAP_ExtF = 1 << 8,
    CAP_ExtC = 1 << 9,
} Capabilities;

typedef enum {
    MISAExtBit_A_Atomic = 1 << 0,
    MISAExtBit_B_Extension = 1 << 1,
    MISAExtBit_C_Compressed = 1 << 2,
    MISAExtBit_D_DoubleFloat = 1 << 3,
    MISAExtBit_E_BaseISA = 1 << 4,
    MISAExtBit_F_SingleFloat = 1 << 5,
    MISAExtBit_G_Reserved = 1 << 6,
    MISAExtBit_H_Hypervisor = 1 << 7,
    MISAExtBit_I_BaseISA = 1 << 8,
    MISAExtBit_J_Reserved = 1 << 9,
    MISAExtBit_K_Reserved = 1 << 10,
    MISAExtBit_L_Reserved = 1 << 11,
    MISAExtBit_M_IntegerMultiplyDivide = 1 << 12,
    MISAExtBit_N_UserLevelInterrupts = 1 << 13,
    MISAExtBit_O_Reserved = 1 << 14,
    MISAExtBit_P_PackedSIMD = 1 << 15,
    MISAExtBit_Q_QuadFloat = 1 << 16,
    MISAExtBit_R_Reserved = 1 << 17,
    MISAExtBit_S_SupervisorMode = 1 << 18,
    MISAExtBit_T_Reserved = 1 << 19,
    MISAExtBit_U_UserMode = 1 << 20,
    MISAExtBit_V_Vector = 1 << 21,
    MISAExtBit_W_Reserved = 1 << 22,
    MISAExtBit_X_NonStandard = 1 << 23,
    MISAExtBit_Y_Reserved = 1 << 24,
    MISAExtBit_Z_Reserved = 1 << 25,
} MISA_Extensions;


////
//  MMU Class
//

typedef void (*MemAccessFunc)(void* func_data, u32 addr, u32 *value, u32 size, bool rnw);

typedef struct {
    void* data;
    usize start;
    usize size;
    MemAccessFunc func;
} MemRegion;

typedef struct {
    MemRegion* regions; //TODO: Stretchy buf?
    usize num_regions;
} MMU;

static MemRegion memreg_init(usize start, usize size, MemAccessFunc func) {
    assert(IS_ALIGN(start, PAGE_SIZE));
    assert(IS_ALIGN(size, PAGE_SIZE));
    u8* mem = (u8*)aligned_alloc(PAGE_SIZE, size);
    assert(mem && "OOM");
    memset(mem, 0, size);
    return (MemRegion) {mem, start, size, func};
}

static void memreg_deinit(MemRegion* mem) {
    if (mem->data)
        free(mem->data);
    *mem = (MemRegion){0};
}

static void default_mem_access(void* func_data, u32 addr, u32 *value, u32 size, bool rnw) {
    MemRegion* region = (MemRegion*)func_data;
    assert(region->start <= addr && addr - region->start + size <= region->size
        && "Out of bounds read");
    assert(value);
    u8* mem = (u8*)region->data;
    if (rnw) {
        memcpy(value, mem + (addr - region->start), size); //TODO: replace
    } else {
        memcpy(mem + (addr - region->start), value, size); 
    }
}

static MMU mmu_init(MemRegion* regions, usize num_regions) {
    return (MMU) {regions, num_regions};
}

static void mmu_deinit(MMU* mmu) {
    for (usize i = 0; i < mmu->num_regions; i++)
    {
        memreg_deinit(mmu->regions + i);
    }
}

static void mmu_load(MMU* mmu, u8* data, usize size, u32 at) {
    u32 src_addr = 0;
    u32 dest_addr = at;
    for (usize i = 0; i < mmu->num_regions; i++)
    {
        MemRegion* region = mmu->regions + i;
        usize region_end = region->start + region->size;
        if (IN_RANGE(dest_addr, region->start, region_end)) {
            u32 relative_start = dest_addr - region->start;
            usize can_write = MIN(size, region_end - dest_addr);

            assert(region->data);
            memcpy((u8*)region->data + relative_start, data + src_addr, can_write);
            src_addr += can_write;
            dest_addr += can_write;
        }
    }
}

static void mmu_access(MMU* mmu, u32 addr, u32 *value, u32 size, bool rnw) {
    assert(size == 1 || size == 2 || size == 4);
    //TODO: Optimize to binary search or hash
    for (usize i = 0; i < mmu->num_regions; i++)
    {
        MemRegion* region = mmu->regions + i;
        if (region->start <= addr && (addr - region->start) < region->size) {
            if (addr - region->start + size > region->size) {
                assert(false && "TODO: handle R/W across regions");
            }
            region->func(region, addr, value, size, rnw);
            return;
        }
    }
    assert(false && "Out of bounds memory access!");
}

inline static u8 mmu_read_u8(MMU* mmu, u32 addr) {
    u32 val = 0;
    mmu_access(mmu, addr, &val, sizeof(u8), true);
    return (u8)val;
}

inline static void mmu_write_u8(MMU* mmu, u32 addr, u8 value) {
    u32 val = value;
    mmu_access(mmu, addr, &val, sizeof(u8), false);
}

inline static u16 mmu_read_u_u16(MMU* mmu, u32 addr) {
    u32 val = 0;
    mmu_access(mmu, addr, &val, sizeof(u16), true);
    return (u16)val;
}

inline static u16 mmu_read_u16(MMU* mmu, u32 addr) {
    assert(IS_ALIGN(addr, sizeof(u16))
        && "Unaligned u16 read");
    return mmu_read_u_u16(mmu, addr);
}

inline static void mmu_write_u_u16(MMU* mmu, u32 addr, u16 value) {
    u32 val = value;
    mmu_access(mmu, addr, &val, sizeof(u16), false);
}

inline static void mmu_write_u16(MMU* mmu, u32 addr, u16 value) {
    assert(IS_ALIGN(addr, sizeof(u16))
        && "Unaligned u16 write");
    mmu_write_u_u16(mmu, addr, value);
}

inline static u32 mmu_read_u_u32(MMU* mmu, u32 addr) {
    u32 val = 0;
    mmu_access(mmu, addr, &val, sizeof(u32), true);
    return val;
}

inline static u32 mmu_read_u32(MMU* mmu, u32 addr) {
    assert(IS_ALIGN(addr, sizeof(u32))
        && "Unaligned u32 read");
    return mmu_read_u_u32(mmu, addr);
}

inline static void mmu_write_u_u32(MMU* mmu, u32 addr, u32 value) {
    u32 val = value;
    mmu_access(mmu, addr, &val, sizeof(u32), false);
}

inline static void mmu_write_u32(MMU* mmu, u32 addr, u32 value) {
    assert(IS_ALIGN(addr, sizeof(u32))
        && "Unaligned u32 write");
    mmu_write_u_u32(mmu, addr, value);
}

//// Memory Mapped IO Test
//
//

static void test_device_mem_access(void* func_data, u32 addr, u32 *value, u32 size, bool rnw) {
    default_mem_access(func_data, addr, value, size, rnw);
    printf("[TestDevice] %s at adddress %X with value %X\n",
        rnw ? "READ" : "WRITE", addr, *value);
}

////
//  Tests
//

static void mmu_test() {
    MemRegion regions[] = {
        memreg_init(0x00001000, PAGE_SIZE, default_mem_access),
        memreg_init(0x00003000, PAGE_SIZE, default_mem_access),
        memreg_init(0x00004000, PAGE_SIZE, test_device_mem_access),
    };

    MMU mmu = mmu_init(regions, ARRAY_COUNT(regions));

    assert(mmu_read_u8(&mmu, 0x1000) == 0);
    mmu_write_u8(&mmu, 0x1000, 0xAE);
    assert(mmu_read_u8(&mmu, 0x1000) == 0xAE);
    
    assert(mmu_read_u_u16(&mmu, 0x1FFE) == 0);
    mmu_write_u_u16(&mmu, 0x1FFE, 0xCAFE);
    assert(mmu_read_u_u16(&mmu, 0x1FFE) == 0xCAFE);

    assert(mmu_read_u16(&mmu, 0x1202) == 0);
    mmu_write_u16(&mmu, 0x1202, 0xB00B);
    assert(mmu_read_u16(&mmu, 0x1202) == 0xB00B);

    assert(mmu_read_u_u32(&mmu, 0x3AAA) == 0);
    mmu_write_u_u32(&mmu, 0x3AAA, 0xEEEEEEEE);
    assert(mmu_read_u_u32(&mmu, 0x3AAA) == 0xEEEEEEEE);

    assert(mmu_read_u32(&mmu, 0x3204) == 0);
    mmu_write_u32(&mmu, 0x3204, 0xAAAAAAAA);
    assert(mmu_read_u32(&mmu, 0x3204) == 0xAAAAAAAA);

    // Test Device
    mmu_write_u32(&mmu, 0x4000, 1);
    assert(mmu_read_u32(&mmu, 0x4000) == 1);

    mmu_deinit(&mmu);
}

static void regs_test() {
    Registers regs = {};

    RegWrite(regs, sp, 123);
    assert(RegRead(regs, sp) == 123);

    RegWrite(regs, zero, 999);
    assert(RegRead(regs, zero) == 0);

    RegWriteI(regs, 5, 0xAAAAAAAA);
    assert(RegReadI(regs, 5) == 0xAAAAAAAA);

    RegWriteI(regs, 0, 0xAAAAAAAA);
    assert(RegReadI(regs, 0) == 0);
}

static void sign_extend_test() {
    int x = 0x18ff;
    int sign_at = 11;
    assert(SIGN_EXTEND(x, sign_at) == 0xFFFFF8FF);

    x = 0x7ff; 
    assert(SIGN_EXTEND(x, sign_at) == x && x > 0);

    x = 1; sign_at = 0;
    assert(SIGN_EXTEND(x, sign_at) == -1);

    x = 0; sign_at = 0;
    assert(SIGN_EXTEND(x, sign_at) == 0);

    x = -1; sign_at = 31;
    assert(SIGN_EXTEND(x, sign_at) == -1);

    x = 0x7FAECAFE; sign_at = 30;
    assert(SIGN_EXTEND(x, sign_at) == 0xFFAECAFE);
    
    x = 0x30123; sign_at = 17;
    assert(SIGN_EXTEND(x, sign_at) == 0xFFFF0123 && SIGN_EXTEND(x, sign_at) < 0);
}

static void instr_dec_test() {
    u32 lui = LUI(4, 0x12345);
    assert(DEC_RD(lui) == 4);
    assert(DEC_UIMM(lui) == 0x12345);
    assert(DEC_UIMM_VALUE(lui) == 0x12345000);

    i32 j_offset = 3 << 19 | 1 << 11 | 1 << 5 | 2;
    u32 jal = JAL(31, j_offset);
    assert(DEC_RD(jal) == 31);
    u32 jimm = DEC_JIMM(jal);
    assert(jimm == j_offset);
    assert(DEC_JIMM_VALUE(jal) < 0);

    u32 jalr = JALR(31, 17, 0x821);
    assert(DEC_RD(jalr) == 31);
    assert(DEC_RS1(jalr) == 17);
    assert(DEC_IIMM(jalr) == 0x821);
    assert(DEC_IIMM_VALUE(jalr) == 0xFFFFF821);

    u32 beq = BEQ(15, 3, -4096);
    assert(DEC_RS1(beq) == 15);
    assert(DEC_RS2(beq) == 3);
    assert(DEC_BIMM_VALUE((u32)beq) == -4096);
    u32 bne = BNE(2, 5, 4094);
    assert(DEC_RS1(bne) == 2);
    assert(DEC_RS2(bne) == 5);
    assert(DEC_BIMM_VALUE((u32)bne) == 4094);
    u32 bltu = BLTU(0, 31, 0);
    assert(DEC_RS1(bltu) == 0);
    assert(DEC_RS2(bltu) == 31);
    assert(DEC_BIMM_VALUE((u32)bltu) == 0);

    u32 lb = LB(2, 3, 0xFFF);
    assert(DEC_RD(lb) == 2);
    assert(DEC_RS1(lb) == 3);
    assert(DEC_IIMM_VALUE(lb) == -1);

    u32 sw = SW(0, 23, -2048);
    assert(DEC_RS1(sw) == 0);
    assert(DEC_RS2(sw) == 23);
    assert(DEC_SIMM_VALUE(sw) == -2048);

    u32 addi = ADDI(11, 21, 0x800);
    assert(DEC_RD(addi) == 11);
    assert(DEC_RS1(addi) == 21);
    assert(DEC_IIMM_VALUE(addi) == -2048);

    u32 xori = XORI(8, 8, 0x0FF);
    assert(DEC_RD(xori) == 8);
    assert(DEC_RS1(xori) == 8);
    assert(DEC_IIMM_VALUE(xori) == 0xFF);

    u32 slli = SLLI(31, 15, 31);
    assert(DEC_RD(slli) == 31);
    assert(DEC_RS1(slli) == 15);
    assert(DEC_SHAMT(slli) == 31);

    u32 add = ADD(0, 31, 31);
    assert(DEC_RD(add) == 0);
    assert(DEC_RS1(add) == 31);
    assert(DEC_RS2(add) == 31);

    u32 fence = FENCE(1, 31, 0x7FF);
    assert(DEC_RD(fence) == 1);
    assert(DEC_RS1(fence) == 31);
    assert(DEC_IIMM_VALUE(fence) == 0x7FF);
}

static void disasm_instr(u32 instr) {
    u32 opcode = DEC_OPCODE(instr);
    u32 rd_idx = DEC_RD(instr);
    u32 rs1_idx = DEC_RS1(instr);
    u32 rs2_idx = DEC_RS2(instr);
    u32 u_imm = DEC_UIMM_VALUE(instr);
    u32 i_imm = (u32)DEC_IIMM_VALUE(instr);
    u32 s_imm = (u32)DEC_SIMM_VALUE(instr);
    i32 j_imm = DEC_JIMM_VALUE(instr);
    i32 b_imm = DEC_BIMM_VALUE(instr);
    u32 shamt = DEC_SHAMT(instr);
    u32 func7 = DEC_FUNCT7(instr);
    u32 aluop = DEC_ALUOP(instr);

    const char* op = "?";
    i32 imm_op = 0;
    switch (opcode)
    {
    case OPC_LUI:
        printf("lui %s 0x%X", REGADDR_TO_STR(rd_idx), DEC_UIMM(instr));
        break;
    case OPC_OP_IMM:
        switch (aluop)
        {
        case ALUOP_ADDI:
            op = "addi";
            imm_op = i_imm;
            break;
        case ALUOP_SLLI:
            op = "slli";
            imm_op = shamt;
            break;
        case ALUOP_SLTI:
            op = "slti";
            imm_op = i_imm;
            break;
        case ALUOP_SLTIU:
            op = "sltiu";
            imm_op = i_imm;
            break;
        case ALUOP_XORI:
            op = "xori";
            imm_op = i_imm;
            break;
        case ALUOP_SRLI:
            op = (func7 == FUNC7_SRLI) ? "srli" : "srai";
            imm_op = shamt;
            break;
        case ALUOP_ORI:
            op = "ori";
            imm_op = i_imm;
            break;
        case ALUOP_ANDI:
            op = "andi";
            imm_op = i_imm;
            break;
        }
        printf("%s %s, %s, %d", op, REGADDR_TO_STR(rd_idx), REGADDR_TO_STR(rs1_idx), imm_op);
        break;
    case OPC_AUIPC:
        printf("auipc %s, 0x%08X", REGADDR_TO_STR(rd_idx), (i32)u_imm);
        break;
    case OPC_OP:
        if (func7 == 0 || func7 == FUNC7_SRAI) {          
            switch (aluop)
            {
            case ALUOP_ADD:
                op = (func7 == FUNC7_ADD) ? "add" : "sub";
                break;
            case ALUOP_SLL:
                op = "sll";
                break;
            case ALUOP_SLT:
                op = "slt";
                break;
            case ALUOP_SLTU:
                op = "sltu";
                break;
            case ALUOP_XOR:
                op = "xor";
                break;
            case ALUOP_SRL:
                op = (func7 == FUNC7_SRA) ? "sra" : "srl";
                break;
            case ALUOP_OR:
                op = "or";
                break;
            case ALUOP_AND:
                op = "and";
                break;
            default: assert(0);
            }
        } else {
            op = "op??";
        }
        printf("%s %s, %s, %s", op, REGADDR_TO_STR(rd_idx), REGADDR_TO_STR(rs1_idx), REGADDR_TO_STR(rs2_idx));
        break;
    case OPC_LOAD:
        switch (aluop)
        {
        case ALUOP_LB:
            op = "lb";
            break;
        case ALUOP_LH:
            op = "lh";
            break;
        case ALUOP_LBU:
            op = "lbu";
            break;
        case ALUOP_LHU:
            op = "lhu";
            break;
        case ALUOP_LW:
            op = "lw";
            break;
        }
        printf("%s %s, %d(%s)", op, REGADDR_TO_STR(rd_idx), (i32)i_imm, REGADDR_TO_STR(rs1_idx));
        break;
    case OPC_STORE:
        switch (aluop)
        {
        case ALUOP_SB:
            op = "sb";
            break;
        case ALUOP_SH:
            op = "sh";
            break;
        case ALUOP_SW:
            op = "sw";
            break;
        }
        printf("%s %s, %d(%s)", op, REGADDR_TO_STR(rs2_idx), (i32)s_imm, REGADDR_TO_STR(rs1_idx));
        break;
    case OPC_JAL:
        printf("jal %s, %d", REGADDR_TO_STR(rd_idx), (i32)j_imm);
        break;
    case OPC_JALR:
        printf("jalr %s, %d(%s)", REGADDR_TO_STR(rd_idx), (i32)i_imm, REGADDR_TO_STR(rs1_idx));
        break;
    case OPC_BRANCH:
        switch (aluop)
        {
        case ALUOP_BEQ:
            op = "beq";
            break;
        case ALUOP_BNE:
            op = "bne";
            break;
        case ALUOP_BLT:
            op = "bne";
            break;
        case ALUOP_BGE:
            op = "bge";
            break;
        case ALUOP_BLTU:
            op = "bltu";
            break;
        case ALUOP_BGEU:
            op = "bgeu";
            break;
        }
        printf("%s %d", op, b_imm);
        break;
    case OPC_MISC_MEM:
        printf("fence");
        break;
    case OPC_SYSTEM:
        if (i_imm == 0) {
            printf("ecall");
        } else if (i_imm == 1) {
            printf("ebreak");
        } else {
            printf("system unknown\n");
        }
        break;
    default:
        printf("UNKNOWN");
    }
}

typedef struct {
    u32 pc;
    Registers regs;
    u32 capabilities;
} CPU;

typedef enum {
    CPU_Ok,
    CPU_Halt,
    CPU_IllegalInstr,
    CPU_UnsupportedInstr,
    CPU_UnalignedJump,
    CPU_UnalignedLoad,
    CPU_UnalignedStore,
} CPU_Result;

static CPU cpu_init(u32 capabilities) {
    assert( (capabilities & CAP_RegSize) == 0 && "Unsupported register size");
    assert( capabilities < CAP_Privileges && "No other extensions implemented");
    CPU cpu = {.capabilities = capabilities};
    return cpu;
}

static void dump_registers(CPU* cpu) {
    printf("pc =\t0x%08X\n", cpu->pc);
    for (size_t row = 0; row < 8; row++)
    {
        for (size_t col = 0; col < 4; col++)
        {
            int i = col + row*4; 
            printf("x%d =\t0x%08X\t", i, RegReadI(cpu->regs, i));
        }
        printf("\n");
    }   
}

static CPU_Result cpu_step(CPU* cpu, MMU* mmu) {
    CPU_Result r = CPU_Ok;

    // Fetch
    Registers* registers = &cpu->regs;
    //TODO: IALIGN 16 or 32
    u32 instr = mmu_read_u32(mmu, cpu->pc);
    u32 next_pc = cpu->pc + 4;
    
    // Decode
    u32 opcode = DEC_OPCODE(instr);
    u32 rd_idx = DEC_RD(instr);
    u32 rs1_idx = DEC_RS1(instr);
    u32 rs2_idx = DEC_RS2(instr);
    u32 u_imm = DEC_UIMM_VALUE(instr);
    u32 i_imm = (u32)DEC_IIMM_VALUE(instr);
    u32 s_imm = (u32)DEC_SIMM_VALUE(instr);
    i32 j_imm = DEC_JIMM_VALUE(instr);
    i32 b_imm = DEC_BIMM_VALUE(instr);
    u32 shamt = DEC_SHAMT(instr);
    u32 func7 = DEC_FUNCT7(instr);
    u32 aluop = DEC_ALUOP(instr);

    printf("pc=%08X Decoded instruction %08X -> Opcode Category: %s -> ", cpu->pc, instr, 
        OPCODE_TO_STR(opcode));

    disasm_instr(instr);

    printf("\n");
    
    // Execute
    u32 alu_result, eff_addr, mem_data;
    u32 rs1, rs2;
    switch (opcode)
    {
    case OPC_LUI:
        RegWriteI(*registers, rd_idx, u_imm);
        break;
    case OPC_OP_IMM:
        rs1 = RegReadI(*registers, rs1_idx);
        switch (aluop)
        {
        case ALUOP_ADDI:
            alu_result = rs1 + i_imm;
            break;
        case ALUOP_SLLI:
            alu_result = rs1 << shamt;
            break;
        case ALUOP_SLTI:
            alu_result = (i32)rs1 < (i32)i_imm;
            break;
        case ALUOP_SLTIU:
            alu_result = (u32)rs1 < (u32)i_imm;
            break;
        case ALUOP_XORI:
            alu_result = rs1 ^ i_imm;
            break;
        case ALUOP_SRLI:
            alu_result = (func7 == FUNC7_SRAI) ? SAR(rs1, shamt) : ((u32)rs1) >> (u32)shamt;
            break;
        case ALUOP_ORI:
            alu_result = rs1 | i_imm;
            break;
        case ALUOP_ANDI:
            alu_result = rs1 & i_imm;
            break;
        }
        RegWriteI(*registers, rd_idx, alu_result);
        break;
    case OPC_AUIPC:
        RegWriteI(*registers, rd_idx, u_imm + cpu->pc); //Check +-4?
        break;
    case OPC_OP:
        rs1 = RegReadI(*registers, rs1_idx);
        rs2 = RegReadI(*registers, rs2_idx);
        if (func7 == 0 || func7 == FUNC7_SRAI) { 
            switch (aluop)
            {
            case ALUOP_ADD:
                alu_result = (func7 == FUNC7_ADD) ? rs1 + rs2 : rs1 - rs2;
                break;
            case ALUOP_SLL:
                alu_result = rs1 << rs2;
                break;
            case ALUOP_SLT:
                alu_result = (i32)rs1 < (i32)rs2;
                break;
            case ALUOP_SLTU:
                alu_result = (u32)rs1 < (u32)rs2;
                break;
            case ALUOP_XOR:
                alu_result = rs1 ^ rs2;
                break;
            case ALUOP_SRL:
                alu_result = (func7 == FUNC7_SRA) ? SAR(rs1, rs2) : ((u32)rs1) >> (u32)rs2;
                break;
            case ALUOP_OR:
                alu_result = rs1 | rs2;
                break;
            case ALUOP_AND:
                alu_result = rs1 & rs2;
                break;
            default: 
                r = CPU_IllegalInstr;
                goto incr_pc;
            }
            RegWriteI(*registers, rd_idx, alu_result);
        } else {
            printf("\tUnimplemented func7=%X for ALU operation!\n", func7);
            r = CPU_UnsupportedInstr;
            goto incr_pc;
        }
        break;
    case OPC_LOAD:
        rs1 = RegReadI(*registers, rs1_idx);
        eff_addr = (u32)((i32)rs1 + (i32)i_imm);
        switch (aluop)
        {
        case ALUOP_LB:
        case ALUOP_LBU:
            mem_data = (u32)mmu_read_u8(mmu, eff_addr);
            if (aluop == ALUOP_LB)
                mem_data = SIGN_EXTEND(mem_data, 7);
            break;
        case ALUOP_LH:
        case ALUOP_LHU:
            if ((cpu->capabilities & CAP_UnalignedMem) > 0) {
                mem_data = (u32)mmu_read_u_u16(mmu, eff_addr);
            } else {
                if (!IS_ALIGN(eff_addr, sizeof(u16))) {
                    r = CPU_UnalignedLoad;
                    goto incr_pc;
                }
                mem_data = (u32)mmu_read_u16(mmu, eff_addr);
            }
            if (aluop == ALUOP_LH)
                mem_data = SIGN_EXTEND(mem_data, 15);
            break;
        case ALUOP_LW:
            if ((cpu->capabilities & CAP_UnalignedMem) > 0) {
                mem_data = (u32)mmu_read_u_u32(mmu, eff_addr);
            } else {
                if (!IS_ALIGN(eff_addr, sizeof(u32))) {
                    r = CPU_UnalignedLoad;
                    goto incr_pc;
                }
                mem_data = (u32)mmu_read_u32(mmu, eff_addr);
            }
            break;
        default:
            printf("Illegal LOAD Instruction!\n");
            r = CPU_IllegalInstr;
            goto incr_pc;
        }
        RegWriteI(*registers, rd_idx, mem_data);
        break;
    case OPC_STORE:
        rs1 = RegReadI(*registers, rs1_idx);
        rs2 = RegReadI(*registers, rs2_idx);
        eff_addr = (u32)((i32)rs1 + (i32)s_imm);
        switch (aluop)
        {
        case ALUOP_SB:
            mmu_write_u8(mmu, eff_addr, rs2 & 0xFF);
            break;
        case ALUOP_SH:
            if ((cpu->capabilities & CAP_UnalignedMem) > 0) {
                mmu_write_u_u16(mmu, eff_addr, rs2 & 0xFFFF);
            } else {
                if (!IS_ALIGN(eff_addr, sizeof(u16))) {
                    r = CPU_UnalignedStore;
                    goto incr_pc;
                }
                mmu_write_u16(mmu, eff_addr, rs2 & 0xFFFF);
            }
            break;
        case ALUOP_SW:
            if ((cpu->capabilities & CAP_UnalignedMem) > 0) {
                mmu_write_u_u32(mmu, eff_addr, rs2);
            } else {
                if (!IS_ALIGN(eff_addr, sizeof(u32))) {
                    r = CPU_UnalignedStore;
                    goto incr_pc;
                }
                mmu_write_u32(mmu, eff_addr, rs2);
            }
            break;
        default:
            printf("Illegal STORE Instruction!\n");
            r = CPU_IllegalInstr;
            goto incr_pc; //Illegal instruction
        }
        break;
    case OPC_JAL:
        RegWriteI(*registers, rd_idx, next_pc);
        next_pc = (u32)((i32)cpu->pc + j_imm);
        printf("\tJAL called, jumping to %08X\n", next_pc);
        break;
    case OPC_JALR:
        rs1 = RegReadI(*registers, rs1_idx);
        RegWriteI(*registers, rd_idx, next_pc);
        next_pc = (u32)(i_imm + (i32)rs1) & 0xFFFFFFFEU;
        printf("\tJALR called, jumping to %08X\n", next_pc);
        break;
    case OPC_BRANCH:
        rs1 = RegReadI(*registers, rs1_idx);
        rs2 = RegReadI(*registers, rs2_idx);
        switch (aluop)
        {
        case ALUOP_BEQ:
            alu_result = rs1 == rs2;
            break;
        case ALUOP_BNE:
            alu_result = rs1 != rs2;
            break;
        case ALUOP_BLT:
            alu_result = (i32)rs1 < (i32)rs2;
            break;
        case ALUOP_BGE:
            alu_result = (i32)rs1 > (i32)rs2;
            break;
        case ALUOP_BLTU:
            alu_result = (u32)rs1 < (u32)rs2;
            break;
        case ALUOP_BGEU:
            alu_result = (u32)rs1 < (u32)rs2;
            break;
        default: 
            printf("Illegal BRANCH Instruction!\n"); 
            r = CPU_IllegalInstr; //Illegal instruction
            goto incr_pc;
        }
        if (alu_result) {
            next_pc = cpu->pc + b_imm;
            if ((next_pc & 0b11) != 0) {
                printf("Unaligned conditional from %08X jump towards %08X!\n", cpu->pc, next_pc);
                r = CPU_UnalignedJump;
                break;
            }
        }
        break;
    case OPC_SYSTEM:
        if (i_imm == 0) {
            //TODO: implement some syscall?
            printf("\tECALL received!\n");
        } else if (i_imm == 1) {
            printf("\tEBREAK received!. Program finished\n");
            r = CPU_Halt;
        } else {
            printf("Unknown SYSTEM instruction received!\n");
        }
        break;
    case OPC_MISC_MEM:
        printf("FENCE received. Currently NOOP.\n");
        break;
    default:
        printf("Illegal or unimplemented instruction: %s!\n", OPCODE_TO_STR(opcode));
        r = CPU_IllegalInstr;
        break;
    }

incr_pc:
    cpu->pc = next_pc;
    return r;
}

static void instr_test() {
    CPU cpu = cpu_init(0);
    
    const u32 ROM_START = 0x00010000;
    const u32 RAM_START = 0x00040000;
    //const u32 RAM_STACK_BOT = RAM_START + 0x1000;

    MemRegion regions[] = {
        memreg_init(ROM_START, 16*PAGE_SIZE, default_mem_access),
        memreg_init(RAM_START, PAGE_SIZE, default_mem_access),
    };

    MMU mmu = mmu_init(regions, ARRAY_COUNT(regions));

    u32 program[] = {
        LUI(5, 0x12345),
        ADDI(5, 5, 0x678),
        LUI(6, 0xAAAAB),
        ADDI(6, 6, 0xAAA),
        LI(6, 0xBBBBBBBB),
        LI(7, 0xFFFFFFFF),
        LI(8, 2),

        AUIPC(5, 0),
        AUIPC(5, 16),

        LI(5, RAM_START),
        SB(5, 6, 0),
        SH(5, 6, 1),
        SH(5, 6, 2),
        SW(5, 6, 5),
        SW(5, 6, 4),

        LBU(7, 5, 0),
        LB(7, 5, 0),
        LH(7, 5, 1),
        LHU(7, 5, 2),
        LH(7, 5, 2),
        LW(7, 5, 5),
        LW(7, 5, 4),

        LI(8, -11),
        SLTI(10, 8, -10),
        SLTIU(10, 8, -1),
        SLTI(10, 8, -20),
        ADDI(8, 8, 20),
        SLTIU(10, 8, 1),

        LI(8, 0xFFFFFFFF),
        XORI(8, 8, 0x0FF),
        ORI(8, 8, 0xaa),
        ANDI(8, 8, 0x800),

        LI(31, 0xFFFFFFFF),
        SLLI(31, 31, 8),
        SRAI(31, 31, 8),
        SRLI(30, 31, 4),

        LI(10, 123),
        LI(11, 456),
        ADD(12, 10, 11),
        SUB(12, 11, 10),
        XOR(12, 10, 11),
        OR(12, 10, 11),
        AND(12, 10, 11),

        LI(10, -2),
        LI(11, -1),
        SLT(12, 10, 11),
        SLTU(12, 11, 10),
        NEG(10, 10),
        NEG(11, 11),
        SLTU(12, 11, 10),

        SLL(31, 31, 11),
        SRA(31, 31, 10),
        SRL(31, 31, 10),

        JAL(0, 12),
        NOP(), NOP(), NOP(),
        JAL(1, 16),

        ADDI(31, 0, 0), ADDI(30, 0, 0), RET(),
        JAL(1, -12),
        NOP(),
    };

    cpu.pc = ROM_START; //pc_end = ROM_START + sizeof(program);
    mmu_load(&mmu, (u8*)program, sizeof(program), ROM_START);

#define CYCLE(expect) assert(cpu_step(&cpu, &mmu) == (expect))

    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x5 == 0x12345678);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x6 == 0xAAAAAAAA);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x6 == 0xBBBBBBBB);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xFFFFFFFF);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x8 == 2);
    CYCLE(CPU_Ok); assert(cpu.regs.x5 == cpu.pc - 4);
    CYCLE(CPU_Ok); assert(cpu.regs.x5 == cpu.pc - 4 + (16 << 12));

    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x5 == RAM_START);
    CYCLE(CPU_Ok); assert(mmu_read_u8(&mmu, RAM_START) == 0xBB);
    //SH unaligned
    CYCLE(CPU_UnalignedStore);
    //SH aligned
    CYCLE(CPU_Ok); assert(mmu_read_u16(&mmu, RAM_START+2) == 0xBBBB);
    //SW unaligned
    CYCLE(CPU_UnalignedStore);
    //SW aligned
    CYCLE(CPU_Ok); assert(mmu_read_u32(&mmu, RAM_START+4) == 0xBBBBBBBB);

    CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xBB);
    CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xFFFFFFBB);
    CYCLE(CPU_UnalignedLoad);
    CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xBBBB);
    CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xFFFFBBBB);
    CYCLE(CPU_UnalignedLoad);
    CYCLE(CPU_Ok); assert(cpu.regs.x7 == 0xBBBBBBBB);

    //SLTI, SLTIU
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); assert(cpu.regs.x10 == 1);
    CYCLE(CPU_Ok); assert(cpu.regs.x10 == 1);
    CYCLE(CPU_Ok); assert(cpu.regs.x10 == 0);
    CYCLE(CPU_Ok); assert(cpu.regs.x8 > 0);
    CYCLE(CPU_Ok); assert(cpu.regs.x10 == 0);

    //XORI, ORI, ANDI
    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x8 == 0xFFFFFFFF);
    CYCLE(CPU_Ok); assert(cpu.regs.x8 == 0xFFFFFF00);
    CYCLE(CPU_Ok); assert(cpu.regs.x8 == 0xFFFFFFaa);
    CYCLE(CPU_Ok); assert(cpu.regs.x8 == 0xFFFFF800);

    CYCLE(CPU_Ok); CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0xFFFFFFFF);
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0xFFFFFF00);
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0xFFFFFFFF);
    CYCLE(CPU_Ok); assert(cpu.regs.x30 == 0x0FFFFFFF);

    // ADD, SUB, XOR, OR, AND
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 579);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 333);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 0x1B3);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 0x1FB);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 0x48);

    // SLT, SLTU
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 1);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 0);
    CYCLE(CPU_Ok); CYCLE(CPU_Ok);
    CYCLE(CPU_Ok); assert(cpu.regs.x12 == 1);

    // SLL, SRL, SRA
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0xFFFFFFFE);
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0xFFFFFFFF);
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0x3FFFFFFF);

    CYCLE(CPU_Ok); assert(cpu.pc == 0x0001010C);
    CYCLE(CPU_Ok);

    CYCLE(CPU_Ok); assert(cpu.regs.x1 == 0x00010114);
    CYCLE(CPU_Ok); assert(cpu.pc == 0x00010114);
    CYCLE(CPU_Ok); assert(cpu.regs.x31 == 0);
    CYCLE(CPU_Ok); assert(cpu.regs.x30 == 0);
    CYCLE(CPU_Ok); assert(cpu.pc == cpu.regs.x1);
    CYCLE(CPU_Ok);

    dump_registers(&cpu);

    mmu_deinit(&mmu);

#undef CYCLE
}

__attribute_maybe_unused__
static void program_test() {
    CPU cpu = {};
    
    const u32 ROM_START = 0x00010000;
    const u32 RAM_START = 0x00040000;
    const u32 RAM_STACK_BOT = RAM_START + 0x1000;

    MemRegion regions[] = {
        memreg_init(ROM_START, 16 * PAGE_SIZE, default_mem_access),
        memreg_init(RAM_START, PAGE_SIZE, default_mem_access),
    };

    MMU mmu = mmu_init(regions, ARRAY_COUNT(regions));
    u32 pc_end;

    u32 program[] = {
        0x00058613,
        0x00050593,
        0x00100513,
        0x04000893,
        0x00000073,
        0x00008067,
        0xff410113,
        0x00112423,
        0x00800393,
        0xfff38393,
        0x00f57293,
        0x00900313,
        0x00534663,
        0x03028293,
        0x0080006f,
        0x03728293,
        0x00238eb3,
        0x005e8023,
        0x00455513,
        0xfc704ce3,
        0x00010513,
        0x00800593,
        0xfa9ff0ef,
        0x00812083,
        0x00c10113,
        0x00008067,
        0x00051663,
        0x00100513,
        0x00008067,
        0xff810113,
        0x00112223,
        0x00a12023,
        0xfff50513,
        0xfe5ff0ef,
        0x00012303,
        0x00650533,
        0x00412083,
        0x00810113,
        0x00008067,
        0x00a00513,
        0xfc9ff0ef,
        0xf75ff0ef,
        0x00000517,
        0x02950513,
        0x00200593,
        0xf4dff0ef,
        0x00000513,
        0x05d00893,
        0x00000073,
        EBREAK(),
    };
    cpu.pc = 0x10110; pc_end = cpu.pc + sizeof(program);
    cpu.regs.sp = RAM_STACK_BOT;
    mmu_load(&mmu, (u8*)program, sizeof(program), 0x00010074);

    while (cpu.pc < pc_end)
    {
        CPU_Result result = cpu_step(&cpu, &mmu);
        if (result == CPU_Halt) {
            break;
        } else if(result != CPU_Ok) {
            printf("CPU Event: %d\n", result);
        }
        dump_registers(&cpu);
    }

    mmu_deinit(&mmu);
}

static CSR_Type* csr_access(CSR_Type* csrs, u32 address) {
    if (address >= CSR_COUNT)
        return NULL;

    if (csr_str[address] != NULL)
        return csrs + address;

    return NULL;
}

static bool csr_raw_read(CSR_Type* csrs, u32 address, CSR_Type* out_val)
{
    CSR_Type* at = csr_access(csrs, address);
    if (at) {
        *out_val = *at;
        return true;
    }
    return false;
}

static bool csr_raw_write(CSR_Type* csrs, u32 address, CSR_Type val)
{    
    CSR_Type* at = csr_access(csrs, address);
    if (at) {
        *at = val;
        return true;
    }
    return false;
}

static void csr_test() {
    CSR_Type* csrs = malloc(sizeof(CSR_Type) * CSR_COUNT);
    
    CSR_Type misa = (0b01 << 30) | MISAExtBit_I_BaseISA;
    assert(csr_raw_write(csrs, CSR_misa, misa));
    CSR_Type val;
    assert(csr_raw_read(csrs, CSR_misa, &val) && val == misa);

    assert(csr_raw_write(csrs, CSR_mvendorid, 0));
    assert(csr_raw_write(csrs, CSR_marchid, 0));
    assert(csr_raw_write(csrs, CSR_mimpid, 'M'));
    assert(csr_raw_write(csrs, CSR_mhartid, 0));

    free(csrs);
}

int main(int argc, char** argv)
{
    mmu_test();
    regs_test();
    sign_extend_test();
    instr_dec_test();
    program_test();
    instr_test();
    csr_test();
    return 0;
}