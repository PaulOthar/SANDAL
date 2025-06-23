#include <sandal.h>
#include <stdio.h>
#include <stdlib.h>


int main(/*int argc,char** argv*/){
	uint8_t bytecode[] = {
			opreset, 0xff,
			opmovl, r0 | 0,//int i = 0
			opmovl, r1 | l1B, 0x09,//int l = 9
			opmovl, r2 | l1B, 0x08,//int j = 8
			opmovl, r3 | l4B, 0xfa, 0xff, 0xff, 0xff,//int j2 = -6
			opjgr, cr1__ | cr0_ | r2,//if(l < i){ jump j }
			opnop, opnop,//pretend this is print
			opinc, r0,//i++
			opjmp, 0x10 | r3,//jump j2
			opsyscall,
			opkill,
			opeos
	};

	uint32_t stack[1024];
	uint32_t variables[1024];
	frame frames[32];

	sandal_vm vm;
	vm.stack = stack;
	vm.variable_stack = variables;
	vm.frame_stack = frames;

	run_sandal_bytecode(bytecode,&vm);
	return 0;
}
