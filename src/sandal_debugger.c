#include <stdio.h>
#include "../include/sandal.h"

//TODO: Eliminar todos os printf, a fim de manter tudo em escrita direta a buffer.

typedef union {
    uint32_t full;
    uint8_t parsed[4];
} debug_result;

static debug_result print_instruction(sandal_vm *vm);

static void print_full_script(sandal_vm *vm);

static void print_values(sandal_vm *vm);

static void print_dump(sandal_vm *vm, int size);

static void _debug_step(sandal_vm *vm);

static int _find_size(sandal_vm *vm);

//--------------------------------------------------------------------------------------------

void debug_sandal_bytecode(uint8_t *bytecode, sandal_vm *vm){
	vm->bytecode = bytecode;
	int code_size = _find_size(vm);
	char buffer[1024];
	int locks;
	while(1){
		printf("\n\n%s\n%s\n%s\n%s\n%s\n\n%s\n%s\n\n>",
				"c: Reads the current line (C to lock for every step)",
				"l: Reads the full script (L to lock for every step)",
				"b: Shows the full script binary data (B to lock for every step)",
				"r: Runs the script from current point",
				"o: Resets the script execution",
				"ENTER: Steps the current point",
				"q: Quits the debugger");
		fgets(buffer,1024,stdin);
		printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
		switch(buffer[0]){
		case 'c': print_instruction(vm); break;
		case 'l': print_full_script(vm); break;
		case 'r': do{ _debug_step(vm); printf("\n\n"); } while(vm->status); break;
		case 'o': vm->program_counter = 0; break;
		case 'b': print_dump(vm,code_size); break;

		case 'C': locks ^= 0b1; break;
		case 'L': locks ^= 0b10; break;
		case 'B': locks ^= 0b100; break;
		case '\n':
			if(locks & 0xff){
				if(locks & 0b10) {print_full_script(vm); printf("\n\n");}
				if(locks & 0b100) {print_dump(vm,code_size); printf("\n\n");}
			}

			_debug_step(vm);

			if(locks & 0b1) { printf("\n\n"); print_instruction(vm); }
			break;
		case 'q': return;
		}
	}
}

//--------------------------------------------------------------------------------------------

static uint8_t _write_operand(sandal_vm *vm, int segment, int operand, char* symbol_buffer){
	int count = 0;
	switch(operand){
	case reg: sprintf(symbol_buffer, "R%X", segment); break;
	case lit: sprintf(symbol_buffer, "%d", _sandal_read_literal(vm, segment, &count)); break;
	case val: sprintf(symbol_buffer, "%d", segment); break;
	case var: sprintf(symbol_buffer, "VAR[R%X]", segment); break;
	case hvar: sprintf(symbol_buffer, "VAR[%d]", segment); break;
	}
	return count;
}

static uint8_t _write_operands(sandal_vm *vm, sandal_instruction_info *info, char* symbol_buffer){
	int count = 0;
	uint8_t opinfo = vm->bytecode[vm->program_counter + 1];
	switch(info->operand_count){
	case 0: break;
	case 1:
		count += _write_operand(vm, opinfo, info->operand[0], symbol_buffer);
		break;
	case 2:
		count += _write_operand(vm, OPINFO_B(opinfo), info->operand[0], symbol_buffer);
		count += _write_operand(vm, OPINFO_A(opinfo), info->operand[1], symbol_buffer + 16);
		break;
	case 3:
		count += _write_operand(vm, OPINFO_C3(opinfo), info->operand[0], symbol_buffer);
		count += _write_operand(vm, OPINFO_C2(opinfo), info->operand[1], symbol_buffer + 16);
		count += _write_operand(vm, OPINFO_C1(opinfo), info->operand[2], symbol_buffer + 32);
		break;
	case 4:
		count += _write_operand(vm, OPINFO_E4(opinfo), info->operand[0], symbol_buffer);
		count += _write_operand(vm, OPINFO_E3(opinfo), info->operand[1], symbol_buffer + 16);
		count += _write_operand(vm, OPINFO_E2(opinfo), info->operand[2], symbol_buffer + 32);
		count += _write_operand(vm, OPINFO_E1(opinfo), info->operand[3], symbol_buffer + 48);
		break;
	}
	return count;
}

static debug_result print_instruction(sandal_vm *vm){
	uint8_t *ptr = &vm->bytecode[vm->program_counter];
	sandal_instruction_info info = instr_table[ptr[0]];
	char symbol_buffer[64];
	char line[64] = {0};

	debug_result result;

	result.parsed[0] = 2;
	result.parsed[1] = info.flags;

	result.parsed[0] += _write_operands(vm, &info, symbol_buffer);

	switch(info.operand_count){
	case 0: result.parsed[0]--; break;
	case 1:
		sprintf(line,"%s", symbol_buffer);
		break;
	case 2:
		sprintf(line,"%s %s", symbol_buffer, symbol_buffer + 16);
		break;
	case 3:
		sprintf(line,"%s %s %s", symbol_buffer, symbol_buffer + 16, symbol_buffer + 32);
		break;
	case 4:
		sprintf(line,"%s %s %s %s", symbol_buffer, symbol_buffer + 16, symbol_buffer + 32, symbol_buffer + 48);
		break;
	}

	printf("[%8X] %s %s",vm->program_counter,info.name,line);
	return result;
}

static void print_full_script(sandal_vm *vm){
	uint32_t oldpc = vm->program_counter;
	vm->program_counter = 0;
	while(vm->bytecode[vm->program_counter] != opeos){
		debug_result result = print_instruction(vm);
		if(vm->program_counter == oldpc) printf("\t<=====-----");
		vm->program_counter += result.parsed[0];
		printf("\n");
	}
	print_instruction(vm);
	vm->program_counter = oldpc;
}

static int _write_valued_operand(sandal_vm *vm, int segment, int operand, char* symbol_buffer){
	switch(operand){
	case reg: sprintf(symbol_buffer, "R%X(%d)", segment, vm->registers[segment]); break;
	case var: sprintf(symbol_buffer, "VAR[R%X(%d)](%d)", segment, vm->registers[segment], vm->current_variables[vm->registers[segment]]); break;
	case hvar: sprintf(symbol_buffer, "VAR[%d](%d)", segment, vm->current_variables[segment]); break;
	default:
		return 0;
	}
	return 1;
}

static void print_values(sandal_vm *vm){
	uint8_t *opptr = &vm->bytecode[vm->program_counter];
	sandal_instruction_info info = instr_table[opptr[0]];
	char symbol_buffer[128] = {0};
	char line[128] = {0};

	int written = 0;

	switch(info.operand_count){
	case 0: break;
	case 1:
		written += _write_valued_operand(vm, opptr[1], info.operand[0], symbol_buffer);
		break;
	case 2:
		written += _write_valued_operand(vm, OPINFO_B(opptr[1]), info.operand[0], symbol_buffer);
		written += _write_valued_operand(vm, OPINFO_A(opptr[1]), info.operand[1], symbol_buffer + 32 * written);
		break;
	case 3:
		written += _write_valued_operand(vm, OPINFO_C3(opptr[1]), info.operand[0], symbol_buffer);
		written += _write_valued_operand(vm, OPINFO_C2(opptr[1]), info.operand[1], symbol_buffer + 32 * written);
		written += _write_valued_operand(vm, OPINFO_C1(opptr[1]), info.operand[2], symbol_buffer + 32 * written);
		break;
	case 4:
		written += _write_valued_operand(vm, OPINFO_E4(opptr[1]), info.operand[0], symbol_buffer);
		written += _write_valued_operand(vm, OPINFO_E3(opptr[1]), info.operand[1], symbol_buffer + 32 * written);
		written += _write_valued_operand(vm, OPINFO_E2(opptr[1]), info.operand[2], symbol_buffer + 32 * written);
		written += _write_valued_operand(vm, OPINFO_E1(opptr[1]), info.operand[3], symbol_buffer + 32 * written);
		break;
	}

	switch(written){
	case 0: return;
	case 1: sprintf(line, "%s",symbol_buffer); break;
	case 2: sprintf(line, "%s %s", symbol_buffer, symbol_buffer + 32); break;
	case 3: sprintf(line, "%s %s %s", symbol_buffer, symbol_buffer + 32, symbol_buffer + 64); break;
	case 4: sprintf(line, "%s %s %s %s", symbol_buffer, symbol_buffer + 32, symbol_buffer + 64, symbol_buffer + 96); break;
	}

	if(info.flags & STACK){
		uint32_t stackptr = vm->stack_pointer;
		uint32_t stackval = stackptr == 0 ? 0xffffffff : vm->stack[stackptr];
		printf("%s STACK[%d](%d)", line, stackptr, stackval);
		return;
	}
	printf(line);
}

static int _find_size(sandal_vm *vm){
	int oldpc = vm->program_counter;
	vm->program_counter = 0;
	char symbol_buffer[64];
	int counter = 0;
	while(vm->bytecode[vm->program_counter] != opeos){
		sandal_instruction_info info = instr_table[vm->bytecode[vm->program_counter]];
		counter = info.operand_count > 0 ? 2 : 1;
		counter += _write_operands(vm, &info, symbol_buffer);
		vm->program_counter += counter;
	}
	int newpc = vm->program_counter;
	vm->program_counter = oldpc;
	return newpc + 1;
}

static void print_dump(sandal_vm *vm, int size){
	int pc = vm->program_counter;
	uint8_t *bytecode = vm->bytecode;
	for(int i = 0; i < size; i++){
		if((i & 0b111) == 0){
			if(i > 0) printf("\n");
			printf("[%8X] ",i);
		}
		else if((i & 0b111) == 0b100){
			printf("-");
		}

		if(i == pc){
			printf("[%02X]",bytecode[i]);
			continue;
		}
		printf(" %02X ",bytecode[i]);
	}
}

static void _debug_step(sandal_vm *vm){
	vm->status = 1;
	debug_result result = print_instruction(vm);
	if(result.parsed[1] & READS){
		printf("\n[-VALUES-] ");
		print_values(vm);
	}
	if(result.parsed[1] & WRITES){
		printf("\n[-WRITES-] ");
		uint32_t oldpc = vm->program_counter;
		sandal_step(vm);
		uint32_t newpc = vm->program_counter;
		vm->program_counter = oldpc;
		print_values(vm);
		vm->program_counter = newpc;
	}
	else{
		sandal_step(vm);
	}
}
