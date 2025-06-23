#ifndef SANDAL
#define SANDAL

#include <stdint.h>

typedef struct{
	uint32_t return_point;
	uint32_t id;
	uint32_t variables_offset;
}frame;

typedef struct{
	uint32_t status;
	union {
		uint32_t registers[16];
		uint64_t bigregisters[8];
	};

	uint8_t *bytecode;
	uint32_t program_counter;

	uint32_t *stack;
	uint32_t stack_pointer;

	uint32_t *variable_stack;
	uint32_t variable_pointer;

	frame *frame_stack;
	uint32_t frame_pointer;
	uint32_t *current_variables;
}sandal_vm;

#define OPINFO vm->bytecode[vm->program_counter + 1]
#define OPINFO_DECL uint8_t opinfo = OPINFO

#define OPINFO_A(source) source & 0xf
#define OPINFO_B(source) source >> 4
#define OPINFO_C1(source) source & 0x3
#define OPINFO_C2(source) (source >> 2) & 0x7
#define OPINFO_C3(source) source >> 5
#define OPINFO_E1(source) source & 0b11
#define OPINFO_E2(source) (source >> 2) & 0b11
#define OPINFO_E3(source) (source >> 4) & 0b11
#define OPINFO_E4(source) (source >> 6)

#define REG_A vm->registers[OPINFO_A(opinfo)]
#define REG_B vm->registers[OPINFO_B(opinfo)]
#define REG_C1 vm->registers[OPINFO_C1(opinfo)]
#define REG_C2 vm->registers[OPINFO_C2(opinfo)]
#define REG_C3 vm->registers[OPINFO_C3(opinfo)]

void sandal_step(sandal_vm *vm);

void run_sandal_bytecode(uint8_t *bytecode, sandal_vm *vm);

uint32_t _sandal_read_literal(sandal_vm *__restrict__ vm, int type, int *counter);

extern void sandal_syscall(sandal_vm *__restrict__ vm);

enum sandal_opinfo{
	r0 = 0x0, r1 = 0x1, r2 = 0x2, r3 = 0x3,
	r4 = 0x4, r5 = 0x5, r6 = 0x6, r7 = 0x7,
	r8 = 0x8, r9 = 0x9, ra = 0xa, rb = 0xb,
	rc = 0xc, rd = 0xd, re = 0xe, rf = 0xf,

	r0_ = 0x00, r1_ = 0x10, r2_ = 0x20, r3_ = 0x30,
	r4_ = 0x40, r5_ = 0x50, r6_ = 0x60, r7_ = 0x70,
	r8_ = 0x80, r9_ = 0x90, ra_ = 0xa0, rb_ = 0xb0,
	rc_ = 0xc0, rd_ = 0xd0, re_ = 0xe0, rf_ = 0xf0,

	cr0_ = 0x00, cr1_ = 0x04, cr2_ = 0x08, cr3_ = 0x0c, cr4_ = 0x10, cr5_ = 0x14, cr6_ = 0x18, cr7_ = 0x1c,
	cr0__ = 0x00, cr1__ = 0x20, cr2__ = 0x40, cr3__ = 0x60, cr4__ = 0x80, cr5__ = 0xa0, cr6__ = 0xc0, cr7__ = 0xe0,

	l1B = 0x10, l2B = 0x20, l3B = 0x30, l4B = 0x40,
	l0 = 0x00,l1 = 0x50, l1f = 0x80, l2f = 0x90, l3f = 0xa0, l4f = 0xb0, l5f = 0xc0, l6f = 0xd0, l7f = 0xe0, l8f = 0xf0,
};

enum sandal_bytecode{
	opnop =		0x00,//[----------:8] nil
	opmov =		0x01,//[src:4][dst:4] dst = src
	opmovl =	0x02,//[lit:4][dst:4](lit:8~32) dst = lit
	opjmp =		0x03,//[---:3][rel:1][dst:4] (---) -> (dst + (rel == 1 ? pc : 0))
	opjez =		0x04,//[src:4][dst:4] src == 0 ? -> (dst)
	opjgz = 	0x05,//[src:4][dst:4] src > 0 ? -> (dst)
	opjlz =		0x06,//[src:4][dst:4] src < 0 ? -> (dst)
	opjezr =	0x07,//[src:4][dst:4] src == 0 ? -> (dst + pc)
	opjgzr =	0x08,//[src:4][dst:4] src > 0 ? -> (dst + pc)
	opjlzr =	0x09,//[src:4][dst:4] src < 0 ? -> (dst + pc)
	opje = 		0x0a,//[v2:3][v1:3][dst:2] v1 == v2 ? -> (dst)
	opjg =		0x0b,//[v2:3][v1:3][dst:2] v1 > v2 ? -> (dst)
	opjl =		0x0c,//[v2:3][v1:3][dst:2] v1 < v2 ? -> (dst)
	opjer =		0x0d,//[v2:3][v1:3][dst:2] v1 == v2 ? -> (dst + pc)
	opjgr =		0x0e,//[v2:3][v1:3][dst:2] v1 > v2 ? -> (dst + pc)
	opjlr =		0x0f,//[v2:3][v1:3][dst:2] v1 < v2 ? -> (dst + pc)

	opadd =		0x10,//[src:4][dst:4] dst += src
	opsub =		0x11,//[src:4][dst:4] dst -= src
	opmul =		0x12,//[src:4][dst:4] dst *= src
	opdiv =		0x13,//[src:4][dst:4] dst /= src
	opmod =		0x14,//[src:4][dst:4] dst %= src
	opand =		0x15,//[src:4][dst:4] dst &= src
	opor =		0x16,//[src:4][dst:4] dst |= scr
	opxor =		0x17,//[src:4][dst:4] dst ^= scr
	opshl =		0x18,//[src:4][dst:4] dst = dst << src
	opshr =		0x19,//[src:4][dst:4] dst = dst >> src
	oppow =		0x1a,//[exp:4][dst:4] dst = dst^exp
	opsqrt =	0x1b,//[src:4][dst:4] dst = sqrt(dst) prec = src
	opinv =		0x1c,//[---:4][dst:4] dst = ~dst
	opneg =		0x1d,//[---:4][dst:4] dst = -dst
	opinc =		0x1e,//[---:4][dst:4] dst++
	opdec =		0x1f,//[---:4][dst:4] dst--

	opcadd =	0x20,//[v2:3][v1:3][dst:2] dst = v1 + v2
	opcsub =	0x21,//[v2:3][v1:3][dst:2] dst = v1 - v2
	opcmul =	0x22,//[v2:3][v1:3][dst:2] dst = v1 * v2
	opcdiv =	0x23,//[v2:3][v1:3][dst:2] dst = v1 / v2
	opcmod =	0x24,//[v2:3][v1:3][dst:2] dst = v1 % v2
	opcand =	0x25,//[v2:3][v1:3][dst:2] dst = v1 & v2
	opcor =		0x26,//[v2:3][v1:3][dst:2] dst = v1 | v2
	opcxor =	0x27,//[v2:3][v1:3][dst:2] dst = v1 ^ v2
	opcshl =	0x28,//[v2:3][v1:3][dst:2] dst = v1 << v2
	opcshr =	0x29,//[v2:3][v1:3][dst:2] dst = v1 >> v2
	opcpow =	0x2a,//[v2:3][v1:3][dst:2] dst = v1^v2
	opcsqrt =	0x2b,//[v2:3][v1:3][dst:2] dst = sqrt(v1) prec = v2

	oppush =	0x30,//[---:4][reg:4] reg -> +stack
	oppushl =	0x31,//[lit:4][---:4] lit -> +stack
	oppop =		0x32,//[---:4][reg:4] reg <- -stack
	oppeek =	0x33,//[src:4][dst:4] dst = stack[src]
	oppeekt =	0x34,//[---:4][reg:4] reg = stack[sp-1]
	opdup = 	0x35,//[----------:8] stack[sp-1] -> +stack
	opswap =	0x36,//[----------:8] stack[sp-1] <=> stack[sp-2]
	opdrop =	0x37,//[----------:8] sp--
	opaddst =	0x38,//[----------:8] stack[sp-2] += stack[sp-1] (sp--)
	opsubst =	0x39,//[----------:8] stack[sp-2] -= stack[sp-1] (sp--)
	opmulst =	0x3a,//[----------:8] stack[sp-2] *= stack[sp-1] (sp--)
	opdivst =	0x3b,//[----------:8] stack[sp-2] /= stack[sp-1] (sp--)
	opmodst =	0x3c,//[----------:8] stack[sp-2] %= stack[sp-1] (sp--)
	opandst =	0x3d,//[----------:8] stack[sp-2] &= stack[sp-1] (sp--)
	oporst =	0x3e,//[----------:8] stack[sp-2] |= stack[sp-1] (sp--)
	opxorst =	0x3f,//[----------:8] stack[sp-2] ^= stack[sp-1] (sp--)

	opcall =	0x40,//[---:4][dst:4] dst -> ()
	opcalll =	0x41,//[lit:4][---:4] lit -> ()
	opreturn =	0x42,//[----------:8] . <- ()
	opvalloc =	0x43,//[---size---:8] size -> +vp
	opgetvar =	0x44,//[reg:4][var:4] reg = variables[vp + var]
	opgethvar =	0x45,//[reg:4][hex:4] reg = variable[vp + hex]
	opsetvar =	0x46,//[reg:4][var:4] variables[vp + var] = reg
	opsetvarl =	0x47,//[lit:4][var:4] variables[vp + var] = lit
	opsethvar =	0x48,//[reg:4][hex:4] variables[vp + hex] = reg
	opsethvarl=	0x49,//[lit:4][hex:4] variables[vp + hex] = lit
	oppushvar =	0x4a,//[---:4][var:4] variables[vp + var] -> +stack
	oppushhvar=	0x4b,//[----hex---:8] variables[vp + hex] -> +stack
	oppopvar =	0x4c,//[---:4][var:4] variables[vp + var] <- -stack
	oppophvar =	0x4d,//[----hex---:8] variables[vp + hex] <- -stack
	opmovvar =	0x4e,//[src:4][dst:4] variables[vp + dst] = variables[vp + src]
	opmovhvar =	0x4f,//[vh2:4][vh1:4] variables[vp + vh1] = variables[vp + vh2]

	opsyscall =	0xf0,//[----------:8] Calls an extern function.
	opptr =		0xfc,//[ptr:4][dst:4] dst = (pc,sp,vp,fp)[ptr]
	opreset =	0xfd,//[----__fvsp] p ? pc = 0, s ? sp = 0, v ? vp = 0, f ? fp = 0
	opkill = 	0xfe,//[----------:8] kills the script
	opeos = 	0xff//[----------:8] End of script, kills the script
};

//----Debugger----

void debug_sandal_bytecode(uint8_t *bytecode, sandal_vm *vm);

typedef struct {
    const char* name;
    int operand[4];
    int operand_count;
    int flags;
} sandal_instruction_info;

enum sandal_debug_operand{
	val,
	lit,
	reg,
	var,
	hvar,
};

enum sandal_debug_flags{
	WRITES = 0b1,
	READS = 0b10,
	STACK = 0b100,
};

static const sandal_instruction_info instr_table[256] = {
	[opnop] = {"NOP", {}, 0, 0},
	[opmov] = {"MOV", {reg,reg}, 2, WRITES | READS},
	[opmovl] = {"MOVL", {lit,reg}, 2, WRITES | READS},
	[opjmp] = {"JMP", {val,reg}, 2, READS},
	[opjez] = {"JEZ", {reg,reg}, 2, READS},
	[opjgz] = {"JGZ", {reg,reg}, 2, READS},
	[opjlz] = {"JLZ", {reg,reg}, 2, READS},
	[opjezr] = {"JEZR", {reg,reg}, 2, READS},
	[opjgzr] = {"JGZR", {reg,reg}, 2, READS},
	[opjlzr] = {"JLZR", {reg,reg}, 2, READS},
	[opje] = {"JE", {reg,reg,reg}, 3, READS},
	[opjg] = {"JG", {reg,reg,reg}, 3, READS},
	[opjl] = {"JL", {reg,reg,reg}, 3, READS},
	[opjer] = {"JER", {reg,reg,reg}, 3, READS},
	[opjgr] = {"JGR", {reg,reg,reg}, 3, READS},
	[opjlr] = {"JLR", {reg,reg,reg}, 3, READS},

	[opadd] = {"ADD", {reg,reg}, 2, WRITES | READS},
	[opsub] = {"SUB", {reg,reg}, 2, WRITES | READS},
	[opmul] = {"MUL", {reg,reg}, 2, WRITES | READS},
	[opdiv] = {"DIV", {reg,reg}, 2, WRITES | READS},
	[opmod] = {"MOD", {reg,reg}, 2, WRITES | READS},
	[opand] = {"AND", {reg,reg}, 2, WRITES | READS},
	[opor] = {"OR", {reg,reg}, 2, WRITES | READS},
	[opxor] = {"XOR", {reg,reg}, 2, WRITES | READS},
	[opshl] = {"SHL", {reg,reg}, 2, WRITES | READS},
	[opshr] = {"SHR", {reg,reg}, 2, WRITES | READS},
	[oppow] = {"POW", {reg,reg}, 2, WRITES | READS},
	[opsqrt] = {"SQRT", {reg,reg}, 2, WRITES | READS},
	[opinv] = {"INV", {reg}, 1, WRITES | READS},
	[opneg] = {"NEG", {reg}, 1, WRITES | READS},
	[opinc] = {"INC", {reg}, 1, WRITES | READS},
	[opdec] = {"DEC", {reg}, 1, WRITES | READS},

	[opcadd] = {"CADD", {reg,reg,reg}, 3, WRITES | READS},
	[opcsub] = {"CSUB", {reg,reg,reg}, 3, WRITES | READS},
	[opcmul] = {"CMUL", {reg,reg,reg}, 3, WRITES | READS},
	[opcdiv] = {"CDIV", {reg,reg,reg}, 3, WRITES | READS},
	[opcmod] = {"CMOD", {reg,reg,reg}, 3, WRITES | READS},
	[opcand] = {"CAND", {reg,reg,reg}, 3, WRITES | READS},
	[opcor] = {"COR", {reg,reg,reg}, 3, WRITES | READS},
	[opcxor] = {"XOR", {reg,reg,reg}, 3, WRITES | READS},
	[opcshl] = {"CSHL", {reg,reg,reg}, 3, WRITES | READS},
	[opcshr] = {"CSHR", {reg,reg,reg}, 3, WRITES | READS},
	[opcpow] = {"CPOW", {reg,reg,reg}, 3, WRITES | READS},
	[opcsqrt] = {"CSQRT", {reg,reg,reg}, 3, WRITES | READS},

	[oppush] = {"PUSH", {reg}, 1, READS | STACK},
	[oppushl] = {"PUSHL", {lit}, 1, READS | STACK},
	[oppop] = {"POP", {reg}, 1, WRITES | READS | STACK},
	[oppeek] = {"PEEK", {reg,reg}, 2, WRITES | READS | STACK},
	[oppeekt] = {"PEEKT", {reg}, 1, WRITES | READS | STACK},
	[opdup] = {"DUP", {}, 0, STACK},
	[opswap] = {"SWAP", {}, 0, STACK},
	[opdrop] = {"DROP", {}, 0, STACK},
	[opaddst] = {"ADDST", {}, 0, STACK},
	[opsubst] = {"SUBST", {}, 0, STACK},
	[opmulst] = {"MULST", {}, 0, STACK},
	[opdivst] = {"DIVST", {}, 0, STACK},
	[opmodst] = {"MODST", {}, 0, STACK},
	[opandst] = {"ANDST", {}, 0, STACK},
	[oporst] = {"ORST", {}, 0, STACK},
	[opxorst] = {"XORST", {}, 0, STACK},

	[opcall] = {"CALL", {reg}, 1, READS},
	[opcalll] = {"CALLL", {lit}, 1, READS},
	[opreturn] = {"RETURN", {}, 0, 0},
	[opvalloc] = {"VALLOC", {val}, 1, READS},
	[opgetvar] = {"GETVAR", {reg,var}, 2, WRITES | READS},
	[opgethvar] = {"GETHVAR", {reg,hvar}, 2, WRITES | READS},
	[opsetvar] = {"SETVAR", {reg,var}, 2, WRITES | READS},
	[opsetvarl] = {"SETVARL", {lit,var}, 2, WRITES | READS},
	[opsethvar] = {"SETHVAR", {reg,hvar}, 2, WRITES | READS},
	[opsethvarl] = {"SETHVARL", {lit,hvar}, 2, WRITES | READS},
	[oppushvar] = {"PUSHVAR", {var}, 1, WRITES | READS | STACK},
	[oppushhvar] = {"PUSHHVAR", {hvar}, 1, WRITES | READS | STACK},
	[oppopvar] = {"POPVAR", {var}, 1, WRITES | READS | STACK},
	[oppophvar] = {"POPHVAR", {hvar}, 1, WRITES | READS | STACK},
	[opmovvar] = {"MOVVAR", {var,var}, 2, WRITES | READS},
	[opmovhvar] = {"MOVHVAR", {hvar,hvar}, 2, WRITES | READS},

	[opsyscall] = {"SYSCALL", {}, 0, 0},
	[opptr] = {"PTR", {val,reg}, 2, WRITES | READS},
	[opreset] = {"RESET", {val}, 1, 0},
	[opkill] = {"KILL", {}, 0, 0},
	[opeos] = {"EOS", {}, 0, 0}
};

//----Compiler----

void compile_sandal_assembly_tokens(uint8_t* assembly_tokens, uint8_t *bytecode);

enum sandal_assembly{
	ASM_NOP	= 0x00,
	ASM_MOV = 0x01,
	ASM_JMP = 0x02, ASM_JE = 0x03, ASM_JG = 0x04, ASM_JL = 0x05,
	ASM_ADD = 0x06, ASM_SUB = 0x07, ASM_MUL = 0x08, ASM_DIV = 0x09, ASM_MOD = 0x0a,
	ASM_AND = 0x0b, ASM_OR = 0x0c, ASM_XOR = 0x0d, ASM_SHL = 0x0e, ASM_SHR = 0x0f,
	ASM_POW = 0x10, ASM_SQRT = 0x11,
	ASM_INV = 0x12, ASM_NEG = 0x13,
	ASM_PUSH = 0x14, ASM_POP = 0x15, ASM_PEEK = 0x16,
	ASM_DUP = 0x17, ASM_SWAP = 0x18, ASM_DROP = 0x19,
	ASM_CALL = 0x1a, ASM_RETURN = 0x1b, ASM_VALLOC = 0x1c,
	ASM_RESET = 0x1d, ASM_KILL = 0x1e, ASM_EOS = 0x1f,
	ASM_SET_LABEL = 0x2f,//Followed by 8 char
};

enum sandal_assembly_operands{
	ASM_R0 = 0x80, ASM_R1 = 0x81, ASM_R2 = 0x82, ASM_R3 = 0x83,
	ASM_R4 = 0x84, ASM_R5 = 0x85, ASM_R6 = 0x86, ASM_R7 = 0x87,
	ASM_R8 = 0x88, ASM_R9 = 0x89, ASM_RA = 0x8a, ASM_RB = 0x8b,
	ASM_RC = 0x8c, ASM_RD = 0x8d, ASM_RE = 0x8e, ASM_RF = 0x8f,
	ASM_LITERAL = 0x90,//Followed by 4 bytes
	ASM_VALUE = 0x91,//Followed by 1 byte (actually a nibble)
	ASM_VARIABLE = 0x92,//Should be followed by a register, literal or value.
	ASM_STACK = 0x93,
	ASM_SYSTEM = 0x94,
	ASM_LABEL = 0x9f,//Followed by 8 char
};

#endif
