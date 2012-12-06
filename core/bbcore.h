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

#ifndef BBCORE
#define BBCORE

#ifdef __cplusplus
extern "C" {
#endif 

    typedef unsigned char u8;
    typedef char s8;
    typedef unsigned short u16;
    typedef short s16;
    typedef unsigned int u32;
    typedef int s32;
    typedef float f32;
    
    static const u8 STACK_SIZE=8;
    static const u32 HEAP_SIZE=256;
    static const u8 MAX_THREADS=8;
    static const u32 OUTPUT_SIZE=1024; // in emu mode

#define NOP 0
#define ORG 1
#define EQU 2
#define JMP 3 
#define JMPZ 4
#define PSHL 5
#define PSH 6 
#define PSHI 7
#define POP 8
#define POPI 9
#define ADD 10
#define SUB 11
#define INC 12
#define DEC 13
#define AND 14
#define OR 15
#define XOR 16
#define NOT 17
#define ROR 18
#define ROL 19
#define PIP 20
#define PDP 21
#define DUP 22
#define OUT 23
#define IN 24

#define LEFT_MOTOR 0x1
#define RIGHT_MOTOR 0x2
#define LEFT_EYE 0x4
#define RIGHT_EYE 0x8

    typedef struct {
        u8 m_active;
        u8 m_start;
        u8 m_pc;
        int m_stack_pos;
        u8 *m_stack;
#ifdef EMU
        u8 m_portb;
        u8* m_output;
        u32 m_outpos;
#endif
    } thread;
    
    typedef struct {
        thread *m_threads;
        u8 *m_heap;
        u32 m_recycle_thread;
        u32 m_clock;
    } machine;
    
    u8 thread_peek(thread *t, machine *m, u8 addr);
    void thread_poke(thread *t, machine *m, u8 addr, u8 data);
    void thread_set_start(thread *t, u8 s);
    void thread_set_pc(thread *t, u8 s);
    u8 thread_get_pc(thread *t);
    u8 thread_get_start(thread *t);
    void thread_run(thread *t, machine *m, u32 clock);
    const u8* thread_get_stack(thread *t);
    const int thread_get_stack_pos(thread *t);
    u8 thread_is_active(thread *t);
    void thread_set_active(thread *t, u8 s);
    void thread_push(thread *t, u8 data);
    u8 thread_pop(thread *t);
    u8 thread_top(thread *t);
    
/////////////////////////////////////////////////////////
    
    void machine_create(machine *t);
    u8 machine_peek(const machine *t, u32 addr);
    void machine_poke(machine *t, u32 addr, u8 data);	
    void machine_run(machine *t);
    
    void set_portb(thread* t, u8 s);
    u8 get_portb(thread* t);
    
    void write_mem(machine *m, u8 *a, u32 len);

#ifdef EMU
    const char *byte_to_binary(int x);
#endif

#ifdef __cplusplus
}


#endif 

#endif
