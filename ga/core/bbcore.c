// Spork Factory Copyright (C) 2012 David Griffiths
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

//gcc -DLINUX -std=gnu99 bbcore.c -o bbcore

#ifndef EMU 
#ifndef LINUX
#include <avr/io.h>
#include <util/delay.h>
#else
#include <stdio.h>
#endif
#else
#include <stdio.h>
#endif

#include <stdlib.h>

#include "bbcore.h"
#include "program.h"

//////////////////////////////////////////////////////////

void thread_create(thread *this) {
    this->m_active=0;
    this->m_start=0;
    this->m_pc=0;
    this->m_stack_pos=-1;
    this->m_stack=(u8*)malloc(sizeof(u8)*STACK_SIZE);

#ifdef EMU
    this->m_portb=0;
    this->m_output=(u8*)malloc(sizeof(u8)*OUTPUT_SIZE);
    this->m_outpos=0;
#endif

	for (int n=0; n<STACK_SIZE; n++)
	{
		this->m_stack[n]=0;
	}
}

u8 thread_peek(thread* this, machine *m, u8 addr) {
	return machine_peek(m,this->m_start+addr);
}

void thread_poke(thread* this, machine *m, u8 addr, u8 data) {
	machine_poke(m,this->m_start+addr,data);
}

void thread_set_start(thread* this, u8 s) { 
    this->m_start=s; 
}

void thread_set_pc(thread* this, u8 s) { 
    this->m_pc=s; 
}

u8 thread_get_pc(thread* this) { 
    return this->m_pc+this->m_start; 
}

u8 thread_get_start(thread* this) { 
    return this->m_start; 
}

void thread_run(thread* this, machine *m, u32 clock) {
	if (!this->m_active) return;
	u8 instr=thread_peek(this,m,this->m_pc);
	this->m_pc++;

 	switch(instr)
	{
    case NOP: break;
    case ORG: this->m_start=this->m_start+this->m_pc-1; this->m_pc=1; break;
    case EQU: thread_push(this,thread_pop(this)==thread_pop(this)); break;
    case JMP: this->m_pc=thread_peek(this,m,this->m_pc++); break;
    case JMPZ: if (thread_pop(this)==0) this->m_pc=thread_peek(this,m,this->m_pc); else this->m_pc++; break;
    case PSHL: thread_push(this,thread_peek(this,m,this->m_pc++)); break;
    case PSH: thread_push(this,thread_peek(this,m,thread_peek(this,m,this->m_pc++))); break;
    case PSHI: thread_push(this,thread_peek(this,m,thread_peek(this,m,thread_peek(this,m,this->m_pc++)))); break;
    case POP: thread_poke(this,m,thread_peek(this,m,this->m_pc++),thread_pop(this)); break;
    case POPI: thread_poke(this,m,thread_peek(this,m,thread_peek(this,m,this->m_pc++)),thread_pop(this)); break;
    case ADD: thread_push(this,thread_pop(this)+thread_pop(this)); break;
    case SUB: thread_push(this,thread_pop(this)-thread_pop(this)); break;
    case INC: thread_push(this,thread_pop(this)+1); break;
    case DEC: thread_push(this,thread_pop(this)-1); break;
    case AND: thread_push(this,thread_pop(this)&thread_pop(this)); break;
    case OR: thread_push(this,thread_pop(this)|thread_pop(this)); break;
    case XOR: thread_push(this,thread_pop(this)^thread_pop(this)); break;
    case NOT: thread_push(this,~thread_pop(this)); break;
    case ROR: thread_push(this,thread_pop(this)>>(thread_peek(this,m,this->m_pc++)%8)); break;
    case ROL: thread_push(this,thread_pop(this)<<(thread_peek(this,m,this->m_pc++)%8)); break;
    case PIP: 
    {
        u8 d=thread_peek(this,m,this->m_pc++); 
        thread_poke(this,m,d,thread_peek(this,m,d)+1); 
    } break;
    case PDP: 
    {
        u8 d=thread_peek(this,m,this->m_pc++); 
        thread_poke(this,m,d,thread_peek(this,m,d)-1); 
    } break;
    case DUP: thread_push(this,thread_top(this)); break;
    case LMOT: set_portb_bit(this,LEFT_MOTOR,!(thread_pop(this)&0x1)); break;
    case RMOT: set_portb_bit(this,RIGHT_MOTOR,!(thread_pop(this)&0x1)); break;
    case LEYE: thread_push(this,get_portb_bit(this,LEFT_EYE)); break;
    case REYE: thread_push(this,get_portb_bit(this,RIGHT_EYE)); break;
    default : break;
	};   
}

const u8* thread_get_stack(thread* this) { 
    return this->m_stack; 
}

const int thread_get_stack_pos(thread* this) { 
    return this->m_stack_pos; 
}

u8 thread_is_active(thread* this) { 
    return this->m_active; 
}

void thread_set_active(thread* this, u8 s) { 
    this->m_active=s; 
}

void thread_push(thread* this, u8 data) {
	if (this->m_stack_pos<STACK_SIZE-1)
	{
		this->m_stack[++this->m_stack_pos]=data;
	}
}

u8 thread_pop(thread* this) {
 	if (this->m_stack_pos>=0)
	{
		u8 ret=this->m_stack[this->m_stack_pos];
		this->m_stack_pos--;
		return ret;
	}
	return 0;   
}

u8 thread_top(thread* this) {
	if (this->m_stack_pos>=0)
	{
		return this->m_stack[this->m_stack_pos];
	}
	return 0;
}

///////////////////////////////////////////////////////////////

void machine_create(machine *this) {
    this->m_heap = (u8*)malloc(sizeof(u8)*HEAP_SIZE);
    this->m_threads = (thread*)malloc(sizeof(thread)*MAX_THREADS);
    this->m_clock=0;

	for (u32 n=0; n<MAX_THREADS; n++)
	{
        thread_create(&this->m_threads[n]);
    }

	for (u32 n=0; n<HEAP_SIZE; n++)
	{
		this->m_heap[n]=0;
	}
}

u8 machine_peek(const machine* this, u32 addr) {
	return this->m_heap[addr%HEAP_SIZE];
}

void machine_poke(machine* this, u32 addr, u8 data) {
	this->m_heap[addr%HEAP_SIZE]=data;
}

void machine_run(machine* this) {
	for (u32 n=0; n<MAX_THREADS; n++) {
		thread_run(&this->m_threads[n],this,this->m_clock);
	}
    this->m_clock++;
}

///////////////////////////////////////////////////////////////////

#ifdef EMU
#include <string.h>
const char *byte_to_binary(int x) {
    static char b[9];
    b[0] = '\0';
    int z;
    for (z = 128; z > 0; z >>= 1) {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }
    return b;
}
#endif

u8 get_pinb(thread *t) {
#ifndef EMU
    return PINB;
#else
    return t->m_portb;
#endif 
}

u8 get_portb_bit(thread *t, u8 mask) {
    if ((get_pinb(t)&mask)>0) return 1;
    return 0;
}

void set_portb_bit(thread *t, u8 mask, u8 v) {
#ifndef EMU    
    if (v) PORTB|=mask;
    else PORTB&=~mask;
#else
    if (v) t->m_portb|=mask;
    else t->m_portb&=~mask;

    if (t->m_outpos<OUTPUT_SIZE) {
        t->m_output[t->m_outpos++]=v;
    }
#endif
}

void write_mem(machine *m, u8 *a, u32 len) {
    for (u32 i=0; i<len; i++) {
        machine_poke(m,i,a[i]);
    }
}

#ifndef EMU

u8 code[]={
// three phaze
//    20, 136, 121, 8, 246, 1, 23, 7, 4, 23, 23, 23, 23, 23, 20, 3

// atonal switch
//    23, 165, 19, 99, 228, 120, 23, 21, 23, 7, 23, 5, 207, 23, 241, 23, 221, 17, 126, 23, 23, 23, 203, 
//    17, 23, 31, 2, 72, 3, 6, 68, 63

//    23, 165, 19, 98, 186, 190, 23, 21, 23, 132, 23, 5, 207, 23, 23, 23, 205,
//    17, 22, 23, 23, 23, 189, 17, 23, 23, 2, 72, 3, 6, 58, 174

//    23, 214, 186, 98, 242, 2, 23, 21, 23, 192, 23, 5, 207, 23, 23, 23, 205,
//    17, 23, 23, 23, 17, 23, 17, 23, 23, 2, 23, 3, 6, 108, 113

//    124, 124, 186, 98, 103, 190, 23, 21, 23, 221, 23, 5, 207, 23, 23, 23, 205,
//    17, 23, 23, 23, 17, 23, 17, 23, 23, 2, 23, 3, 6, 108, 66

// periodic clicker
//    3, 13, 250, 23, 86, 23, 17, 23, 13, 23, 187, 199, 23, 2, 152, 23, 124,
//    51, 12, 22, 6, 36, 6, 15, 17, 0, 21, 101, 20, 253, 133, 22, 8


// odd scale

    23, 19, 11, 23, 13, 84, 196, 179, 166, 23, 23, 23, 13, 94, 23, 178, 
    67, 136, 17, 108, 223, 251, 106, 23, 2, 186, 141, 23, 56, 20, 1, 4 

//    23, 15, 11, 23, 13, 84, 222, 179, 230, 23, 23, 23, 13, 132, 23, 178,
//    67, 120, 17, 108, 84, 251, 162, 23, 2, 186, 141, 23, 56, 20, 1, 4

// nice flip

//    2, 22, 23, 23, 21, 10, 69, 2, 23, 17, 206, 247, 23, 7, 10, 23, 23, 12,
//    68, 23, 23, 23, 14, 4, 249, 227, 118, 121, 96, 50, 91, 213


};




int main(void)
{
#ifndef LINUX

    DDRB = 0x00;
    DDRB |= LEFT_MOTOR;
    DDRB |= RIGHT_MOTOR;
    DDRB |= LED;

    // activate pull up resistors
    PORTB|=LEFT_EYE;
    PORTB|=RIGHT_EYE;

//    PORTB|=LEFT_MOTOR;
//    PORTB|=RIGHT_MOTOR;

#endif

    machine *m=(machine *)malloc(sizeof(machine));
    machine_create(m);

    write_mem(m,program,program_size);

    thread *t=&m->m_threads[0];
    thread_set_active(t,1);

    u32 count=0;
    while(1) { 

        if (count<10000) {
            set_portb_bit(t,LED,count%500<250);
        } else {
            if (get_portb_bit(t,LEFT_EYE) ||
                get_portb_bit(t,RIGHT_EYE))
                set_portb_bit(t, LED, 0);
            else set_portb_bit(t, LED, 1);
        }

//        set_portb_bit(t,LEFT_MOTOR,count%500<250);

        machine_run(m);

        count++;
    }
}

#endif
