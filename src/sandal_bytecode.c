#include <sandal.h>

#ifndef SANDAL_SYSCALL
#define SANDAL_SYSCALL
void sandal_syscall(sandal_vm *__restrict__ vm){
	vm->program_counter = vm->program_counter - 1 + 1;
	return;
}
#endif

static void _sandal_nop(sandal_vm *__restrict__ vm);
static void _sandal_mov(sandal_vm *__restrict__ vm);
static void _sandal_movl(sandal_vm *__restrict__ vm);

static void _sandal_jmp(sandal_vm *__restrict__ vm);
static void _sandal_jez(sandal_vm *__restrict__ vm);
static void _sandal_jgz(sandal_vm *__restrict__ vm);
static void _sandal_jlz(sandal_vm *__restrict__ vm);
static void _sandal_jezr(sandal_vm *__restrict__ vm);
static void _sandal_jgzr(sandal_vm *__restrict__ vm);
static void _sandal_jlzr(sandal_vm *__restrict__ vm);

static void _sandal_je(sandal_vm *__restrict__ vm);
static void _sandal_jg(sandal_vm *__restrict__ vm);
static void _sandal_jl(sandal_vm *__restrict__ vm);
static void _sandal_jer(sandal_vm *__restrict__ vm);
static void _sandal_jgr(sandal_vm *__restrict__ vm);
static void _sandal_jlr(sandal_vm *__restrict__ vm);

static void _sandal_add(sandal_vm *__restrict__ vm);
static void _sandal_sub(sandal_vm *__restrict__ vm);
static void _sandal_mul(sandal_vm *__restrict__ vm);
static void _sandal_div(sandal_vm *__restrict__ vm);
static void _sandal_mod(sandal_vm *__restrict__ vm);
static void _sandal_and(sandal_vm *__restrict__ vm);
static void _sandal_or(sandal_vm *__restrict__ vm);
static void _sandal_xor(sandal_vm *__restrict__ vm);
static void _sandal_shl(sandal_vm *__restrict__ vm);
static void _sandal_shr(sandal_vm *__restrict__ vm);
static void _sandal_pow(sandal_vm *__restrict__ vm);
static void _sandal_sqrt(sandal_vm *__restrict__ vm);
static void _sandal_inv(sandal_vm *__restrict__ vm);
static void _sandal_neg(sandal_vm *__restrict__ vm);
static void _sandal_inc(sandal_vm *__restrict__ vm);
static void _sandal_dec(sandal_vm *__restrict__ vm);

static void _sandal_cadd(sandal_vm *__restrict__ vm);
static void _sandal_csub(sandal_vm *__restrict__ vm);
static void _sandal_cmul(sandal_vm *__restrict__ vm);
static void _sandal_cdiv(sandal_vm *__restrict__ vm);
static void _sandal_cmod(sandal_vm *__restrict__ vm);
static void _sandal_cand(sandal_vm *__restrict__ vm);
static void _sandal_cor(sandal_vm *__restrict__ vm);
static void _sandal_cxor(sandal_vm *__restrict__ vm);
static void _sandal_cshl(sandal_vm *__restrict__ vm);
static void _sandal_cshr(sandal_vm *__restrict__ vm);
static void _sandal_cpow(sandal_vm *__restrict__ vm);
static void _sandal_csqrt(sandal_vm *__restrict__ vm);

static void _sandal_push(sandal_vm *__restrict__ vm);
static void _sandal_pushl(sandal_vm *__restrict__ vm);
static void _sandal_pop(sandal_vm *__restrict__ vm);
static void _sandal_peek(sandal_vm *__restrict__ vm);
static void _sandal_peekt(sandal_vm *__restrict__ vm);
static void _sandal_dup(sandal_vm *__restrict__ vm);
static void _sandal_swap(sandal_vm *__restrict__ vm);
static void _sandal_drop(sandal_vm *__restrict__ vm);
static void _sandal_addst(sandal_vm *__restrict__ vm);
static void _sandal_subst(sandal_vm *__restrict__ vm);
static void _sandal_mulst(sandal_vm *__restrict__ vm);
static void _sandal_divst(sandal_vm *__restrict__ vm);
static void _sandal_modst(sandal_vm *__restrict__ vm);
static void _sandal_andst(sandal_vm *__restrict__ vm);
static void _sandal_orst(sandal_vm *__restrict__ vm);
static void _sandal_xorst(sandal_vm *__restrict__ vm);

static void _sandal_call(sandal_vm *__restrict__ vm);
static void _sandal_calll(sandal_vm *__restrict__ vm);
static void _sandal_return(sandal_vm *__restrict__ vm);
static void _sandal_valloc(sandal_vm *__restrict__ vm);
static void _sandal_getvar(sandal_vm *__restrict__ vm);
static void _sandal_gethvar(sandal_vm *__restrict__ vm);
static void _sandal_setvar(sandal_vm *__restrict__ vm);
static void _sandal_setvarl(sandal_vm *__restrict__ vm);
static void _sandal_sethvar(sandal_vm *__restrict__ vm);
static void _sandal_sethvarl(sandal_vm *__restrict__ vm);
static void _sandal_pushvar(sandal_vm *__restrict__ vm);
static void _sandal_pushhvar(sandal_vm *__restrict__ vm);
static void _sandal_popvar(sandal_vm *__restrict__ vm);
static void _sandal_pophvar(sandal_vm *__restrict__ vm);
static void _sandal_movvar(sandal_vm *__restrict__ vm);
static void _sandal_movhvar(sandal_vm *__restrict__ vm);

static void _sandal_syscall(sandal_vm *__restrict__ vm);
static void _sandal_ptr(sandal_vm *__restrict__ vm);
static void _sandal_reset(sandal_vm *__restrict__ vm);
static void _sandal_kill(sandal_vm *__restrict__ vm);

//--------------------------------------------------------------------------------------------

void sandal_step(sandal_vm *vm){
	switch(vm->bytecode[vm->program_counter]){
		case opnop: _sandal_nop(vm); break;
		case opmov: _sandal_mov(vm); break;
		case opmovl: _sandal_movl(vm); break;

		case opjmp: _sandal_jmp(vm); break;
		case opjez: _sandal_jez(vm); break;
		case opjgz: _sandal_jgz(vm); break;
		case opjlz: _sandal_jlz(vm); break;
		case opjezr: _sandal_jezr(vm); break;
		case opjgzr: _sandal_jgzr(vm); break;
		case opjlzr: _sandal_jlzr(vm); break;
		case opje: _sandal_je(vm); break;
		case opjg: _sandal_jg(vm); break;
		case opjl: _sandal_jl(vm); break;
		case opjer: _sandal_jer(vm); break;
		case opjgr: _sandal_jgr(vm); break;
		case opjlr: _sandal_jlr(vm); break;

		case opadd: _sandal_add(vm); break;
		case opsub: _sandal_sub(vm); break;
		case opmul: _sandal_mul(vm); break;
		case opdiv: _sandal_div(vm); break;
		case opmod: _sandal_mod(vm); break;
		case opand:  _sandal_and(vm); break;
		case opor:  _sandal_or(vm); break;
		case opxor: _sandal_xor(vm); break;
		case opshl: _sandal_shl(vm); break;
		case opshr: _sandal_shr(vm); break;
		case oppow: _sandal_pow(vm); break;
		case opsqrt: _sandal_sqrt(vm); break;
		case opinv: _sandal_inv(vm); break;
		case opneg: _sandal_neg(vm); break;
		case opinc: _sandal_inc(vm); break;
		case opdec: _sandal_dec(vm); break;

		case opcadd:  _sandal_cadd(vm); break;
		case opcsub:  _sandal_csub(vm); break;
		case opcmul:  _sandal_cmul(vm); break;
		case opcdiv:  _sandal_cdiv(vm); break;
		case opcmod:  _sandal_cmod(vm); break;
		case opcand:  _sandal_cand(vm); break;
		case opcor:   _sandal_cor(vm);  break;
		case opcxor:  _sandal_cxor(vm); break;
		case opcshl:  _sandal_cshl(vm); break;
		case opcshr:  _sandal_cshr(vm); break;
		case opcpow:  _sandal_cpow(vm); break;
		case opcsqrt: _sandal_csqrt(vm); break;

		case oppush: _sandal_push(vm); break;
		case oppushl: _sandal_pushl(vm); break;
		case oppop: _sandal_pop(vm); break;
		case oppeek: _sandal_peek(vm); break;
		case oppeekt: _sandal_peekt(vm); break;
		case opdup: _sandal_dup(vm); break;
		case opswap: _sandal_swap(vm); break;
		case opdrop: _sandal_drop(vm); break;
		case opaddst: _sandal_addst(vm); break;
		case opsubst: _sandal_subst(vm); break;
		case opmulst: _sandal_mulst(vm); break;
		case opdivst: _sandal_divst(vm); break;
		case opmodst: _sandal_modst(vm); break;
		case opandst: _sandal_andst(vm); break;
		case oporst: _sandal_orst(vm); break;
		case opxorst: _sandal_xorst(vm); break;

		case opcall: _sandal_call(vm); break;
		case opcalll: _sandal_calll(vm); break;
		case opreturn: _sandal_return(vm); break;
		case opvalloc: _sandal_valloc(vm); break;
		case opgetvar: _sandal_getvar(vm); break;
		case opgethvar: _sandal_gethvar(vm); break;
		case opsetvar: _sandal_setvar(vm); break;
		case opsetvarl: _sandal_setvarl(vm); break;
		case opsethvar: _sandal_sethvar(vm); break;
		case opsethvarl: _sandal_sethvarl(vm); break;
		case oppushvar: _sandal_pushvar(vm); break;
		case oppushhvar: _sandal_pushhvar(vm); break;
		case oppopvar: _sandal_popvar(vm); break;
		case oppophvar: _sandal_pophvar(vm); break;
		case opmovvar: _sandal_movvar(vm); break;
		case opmovhvar: _sandal_movhvar(vm); break;

		case opsyscall: _sandal_syscall(vm); break;

		case opptr: _sandal_ptr(vm); break;
		case opreset: _sandal_reset(vm); break;

		case opeos:
		case opkill:
		default:
			_sandal_kill(vm);
			break;
	}
}

void run_sandal_bytecode(uint8_t *bytecode, sandal_vm *vm){
	vm->bytecode = bytecode;
	do{
		sandal_step(vm);
	}while(vm->status);
}

uint32_t _sandal_read_literal(sandal_vm *__restrict__ vm, int type, int *counter){
	uint32_t result = 0;

	switch(type){
	case 4: result |= vm->bytecode[vm->program_counter + 5] << 24; counter[0]++;
	/* no break */
	case 3: result |= vm->bytecode[vm->program_counter + 4] << 16; counter[0]++;
	/* no break */
	case 2: result |= vm->bytecode[vm->program_counter + 3] << 8; counter[0]++;
	/* no break */
	case 1: result |= vm->bytecode[vm->program_counter + 2]; counter[0]++; break;
	case 5: result = 1; break;
	case 8: result = 0xf; break;
	case 9: result = 0xff; break;
	case 10: result = 0xfff; break;
	case 11: result = 0xffff; break;
	case 12: result = 0xfffff; break;
	case 13: result = 0xffffff; break;
	case 14: result = 0xfffffff; break;
	case 15: result = 0xffffffff; break;
	}

	return result;
}

//--------------------------------------------------------------------------------------------

static uint32_t _sandal_math_pow(register int pow, register uint32_t value){
	register uint32_t acc = 1;
	for(int i = 0;i<pow;i++){
		acc *= value;
	}
	return acc;
}

static uint32_t _sandal_math_sqrt(uint32_t value, register int precision){
	register uint32_t refiner = value >> 1;
	register uint32_t accumulator = 0;
	for(int i = 0;i<precision && refiner != 0;i++){
		uint32_t result = refiner + accumulator;
		if(value >= (result * result)){
			accumulator += refiner;
		}
		refiner = refiner >> 1;
	}

	return accumulator;
}

//--------------------------------------------------------------------------------------------

static void _sandal_nop(sandal_vm *__restrict__ vm){
	vm->program_counter++;
}

static void _sandal_mov(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = REG_B;
	vm->program_counter += 2;
}

static void _sandal_movl(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	int counter = 2;
	uint32_t result = _sandal_read_literal(vm,OPINFO_B(opinfo),&counter);
	REG_A = result;
	vm->program_counter += counter;
}

static void _sandal_jmp(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(opinfo & 0x10){
		vm->program_counter += REG_A;
		return;
	}
	vm->program_counter = REG_A;
}

static void _sandal_jez(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_B == 0){
		vm->program_counter = REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jgz(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(((int32_t)REG_B) > 0){
		vm->program_counter = REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jlz(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(((int32_t)REG_B) < 0){
		vm->program_counter = REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jezr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_B == 0){
		vm->program_counter += REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jgzr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(((int32_t)REG_B) > 0){
		vm->program_counter += REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jlzr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(((int32_t)REG_B) < 0){
		vm->program_counter += REG_A;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_je(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 == vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter = REG_C1;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jg(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 > vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter = REG_C1;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jl(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 < vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter = REG_C1;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jer(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 == vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter += REG_C1;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jgr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 > vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter += REG_C1;
		return;
	}
	vm->program_counter += 2;
}

static void _sandal_jlr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(REG_C2 == vm->registers[(opinfo >> 5) & 0x7]){
		vm->program_counter += REG_C1;
		return;
	}
	vm->program_counter += 2;
}

//--------------------------------------------------------------------------------------------

static void _sandal_add(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A += REG_B;
	vm->program_counter += 2;
}

static void _sandal_sub(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A -= REG_B;
	vm->program_counter += 2;
}

static void _sandal_mul(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A *= REG_B;
	vm->program_counter += 2;
}

static void _sandal_div(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	uint32_t denom = REG_B;
	REG_A = denom ? REG_A / denom : 0;
	vm->program_counter += 2;
}

static void _sandal_mod(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	uint32_t denom = REG_B;
	REG_A = denom ? REG_A % denom : 0;
	vm->program_counter += 2;
}

static void _sandal_and(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A &= REG_B;
	vm->program_counter += 2;
}

static void _sandal_or(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A |= REG_B;
	vm->program_counter += 2;
}

static void _sandal_xor(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A ^= REG_B;
	vm->program_counter += 2;
}

static void _sandal_shl(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = REG_A << REG_B;
	vm->program_counter += 2;
}

static void _sandal_shr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = REG_A >> REG_B;
	vm->program_counter += 2;
}

static void _sandal_pow(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = _sandal_math_pow(REG_B,REG_A);
	vm->program_counter += 2;
}

static void _sandal_sqrt(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	uint32_t precision = (opinfo >> 4) ? REG_B : 100;
	REG_A = _sandal_math_sqrt(REG_A,precision);
	vm->program_counter += 2;
}

static void _sandal_inv(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = ~REG_A;
	vm->program_counter += 2;
}

static void _sandal_neg(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = -REG_A;
	vm->program_counter += 2;
}

static void _sandal_inc(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A += 1;
	vm->program_counter += 2;
}

static void _sandal_dec(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A -= 1;
	vm->program_counter += 2;
}

//--------------------------------------------------------------------------------------------

static void _sandal_cadd(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_C1 = REG_C2 + REG_C3;
	vm->program_counter += 2;
}

static void _sandal_csub(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 - REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cmul(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 * REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cdiv(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	uint32_t denom = REG_C3;
	REG_C1 = denom ? (REG_C2 / denom) : 0;
	vm->program_counter += 2;
}

static void _sandal_cmod(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	uint32_t denom = REG_C3;
	REG_C1 = denom ? (REG_C2 % denom) : 0;
	vm->program_counter += 2;
}

static void _sandal_cand(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 & REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cor(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 | REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cxor(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 ^ REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cshl(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 << REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cshr(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = REG_C2 >> REG_C3;
	vm->program_counter += 2;
}

static void _sandal_cpow(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = _sandal_math_pow(REG_C3, REG_C2);
	vm->program_counter += 2;
}

static void _sandal_csqrt(sandal_vm *__restrict__ vm) {
	OPINFO_DECL;
	REG_C1 = _sandal_math_sqrt(REG_C2, REG_C3);
	vm->program_counter += 2;
}

//--------------------------------------------------------------------------------------------

static void _sandal_push(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->stack[vm->stack_pointer++] = REG_A;
	vm->program_counter += 2;
}

static void _sandal_pushl(sandal_vm *__restrict__ vm){
	int counter = 2;
	uint32_t result = _sandal_read_literal(vm,OPINFO_A(OPINFO),&counter);
	vm->stack[vm->stack_pointer++] = result;
	vm->program_counter += counter;
}

static void _sandal_pop(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = vm->stack[--vm->stack_pointer];
	vm->program_counter += 2;
}

static void _sandal_peek(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = vm->stack[REG_B];
	vm->program_counter += 2;
}

static void _sandal_peekt(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_A = vm->stack[vm->stack_pointer - 1];
	vm->program_counter += 2;
}

static void _sandal_dup(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer] = vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer++;
	vm->program_counter += 1;
}

static void _sandal_swap(sandal_vm *__restrict__ vm){
	uint32_t holder = vm->stack[vm->stack_pointer - 1];
	vm->stack[vm->stack_pointer - 1] = vm->stack[vm->stack_pointer - 2];
	vm->stack[vm->stack_pointer - 2] = holder;
	vm->program_counter += 1;
}

static void _sandal_drop(sandal_vm *__restrict__ vm){
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_addst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] += vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_subst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] -= vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_mulst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] *= vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_divst(sandal_vm *__restrict__ vm){
	uint32_t denom = vm->stack[vm->stack_pointer - 1];
	vm->stack[vm->stack_pointer - 2] = denom ? (vm->stack[vm->stack_pointer - 2] / denom) : 0;
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_modst(sandal_vm *__restrict__ vm){
	uint32_t denom = vm->stack[vm->stack_pointer - 1];
	vm->stack[vm->stack_pointer - 2] = denom ? (vm->stack[vm->stack_pointer - 2] % denom) : 0;
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_andst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] &= vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_orst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] |= vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

static void _sandal_xorst(sandal_vm *__restrict__ vm){
	vm->stack[vm->stack_pointer - 2] ^= vm->stack[vm->stack_pointer - 1];
	vm->stack_pointer--;
	vm->program_counter += 1;
}

//--------------------------------------------------------------------------------------------

static void _sandal_call(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->frame_stack[vm->frame_pointer].return_point = vm->program_counter + 2;
	vm->frame_stack[vm->frame_pointer].variables_offset = vm->variable_pointer;
	vm->current_variables = &vm->variable_stack[vm->variable_pointer];
	vm->program_counter = REG_A;
	vm->frame_stack[vm->frame_pointer].id = vm->program_counter;
	vm->frame_pointer++;
}

static void _sandal_calll(sandal_vm *__restrict__ vm){
	vm->frame_stack[vm->frame_pointer].return_point = vm->program_counter + 2;
	vm->frame_stack[vm->frame_pointer].variables_offset = vm->variable_pointer;
	vm->current_variables = &vm->variable_stack[vm->variable_pointer];
	int counter = 2;
	uint32_t result = _sandal_read_literal(vm,OPINFO_A(OPINFO),&counter);
	vm->program_counter = result;
	vm->frame_stack[vm->frame_pointer].id = result;
	vm->frame_pointer++;
}

static void _sandal_return(sandal_vm *__restrict__ vm){
	vm->frame_pointer--;
	vm->variable_pointer = vm->frame_stack[vm->frame_pointer].variables_offset;
	vm->current_variables = &vm->variable_stack[vm->frame_stack[vm->frame_pointer].variables_offset];
	vm->program_counter = vm->frame_stack[vm->frame_pointer].return_point;
}

static void _sandal_valloc(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->variable_pointer += opinfo;
	vm->program_counter += 2;
}

static void _sandal_getvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_B = vm->current_variables[REG_A];
	vm->program_counter += 2;
}

static void _sandal_gethvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	REG_B = vm->current_variables[opinfo & 0xf];
	vm->program_counter += 2;
}

static void _sandal_setvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[REG_A] = REG_B;
	vm->program_counter += 2;
}

static void _sandal_setvarl(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	int counter = 2;
	uint32_t result = _sandal_read_literal(vm,OPINFO_B(opinfo),&counter);
	vm->current_variables[REG_A] = result;
	vm->program_counter += counter;
}

static void _sandal_sethvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[opinfo & 0xf] = REG_B;
	vm->program_counter += 2;
}

static void _sandal_sethvarl(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	int counter = 2;
	uint32_t result = _sandal_read_literal(vm,OPINFO_B(opinfo),&counter);
	vm->current_variables[opinfo & 0xf] = result;
	vm->program_counter += counter;
}

static void _sandal_pushvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->stack[vm->stack_pointer++] = vm->current_variables[REG_A];
	vm->program_counter += 2;
}

static void _sandal_pushhvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->stack[vm->stack_pointer++] = vm->current_variables[opinfo];
	vm->program_counter += 2;
}

static void _sandal_popvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[REG_A] = vm->stack[--vm->stack_pointer];
	vm->program_counter += 2;
}

static void _sandal_pophvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[opinfo] = vm->stack[--vm->stack_pointer];
	vm->program_counter += 2;
}

static void _sandal_movvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[REG_A] = vm->current_variables[REG_B];
	vm->program_counter += 2;
}

static void _sandal_movhvar(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	vm->current_variables[opinfo & 0xf] = vm->current_variables[opinfo >> 4];
	vm->program_counter += 2;
}

//--------------------------------------------------------------------------------------------

static void _sandal_syscall(sandal_vm *__restrict__ vm){
	sandal_syscall(vm);
	vm->program_counter += 1;
}

static void _sandal_ptr(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	switch(opinfo >> 4){
	case 0: REG_A = vm->program_counter; break;
	case 1: REG_A = vm->stack_pointer; break;
	case 2: REG_A = vm->variable_pointer; break;
	case 3: REG_A = vm->frame_pointer; break;
	}
	vm->program_counter += 2;
}

static void _sandal_reset(sandal_vm *__restrict__ vm){
	OPINFO_DECL;
	if(opinfo & 0b0001){
		vm->program_counter = 0;
	}
	if(opinfo & 0b0010){
		vm->stack_pointer = 0;
	}
	if(opinfo & 0b0100){
		vm->variable_pointer = 0;
	}
	if(opinfo & 0b1000){
		vm->frame_pointer = 0;
	}
	vm->program_counter += 2;
}

static void _sandal_kill(sandal_vm *__restrict__ vm){
	vm->status = 0;
}
