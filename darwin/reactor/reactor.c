////
#include <stdio.h>
#include <stdlib.h>

#define USE_INTERFACE 1
#include "scheme.h"
#include "scheme-private.h"
#define EMU
#include "../core/bbcore.h"

#define MAX_MACHINES 4096

machine *swarm[MAX_MACHINES];
unsigned int next_machine=0;

pointer sc_machine_create(scheme *sc, pointer args) {
    swarm[next_machine]=malloc(sizeof(machine));
    machine_create(swarm[next_machine]);
    printf("made a machine\n");
    return sc->vptr->mk_integer(sc,next_machine++);
}

pointer sc_machine_peek(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args)) &&
           sc->vptr->is_number(sc->vptr->pair_car(sc->vptr->pair_cdr(args)))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            unsigned int a=sc->vptr->ivalue(sc->vptr->pair_car(sc->vptr->pair_cdr(args)));
            return sc->vptr->mk_integer(sc,machine_peek(swarm[m],a));
        }
    }
    return sc->NIL;
}

pointer sc_machine_poke(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args)) &&
           sc->vptr->is_number(sc->vptr->pair_car(sc->vptr->pair_cdr(args))) &&
           sc->vptr->is_number(sc->vptr->pair_car(sc->vptr->pair_cdr(sc->vptr->pair_cdr(args))))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            unsigned int a=sc->vptr->ivalue(sc->vptr->pair_car(sc->vptr->pair_cdr(args)));
            unsigned int v=sc->vptr->ivalue(sc->vptr->pair_car(sc->vptr->pair_cdr(sc->vptr->pair_cdr(args))));
            //printf("poking %d @ %d w %d\n",m,a,v);
            machine_poke(swarm[m],a,v);
        }
    }
    return sc->NIL;
}

pointer sc_machine_run(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args)) &&
           sc->vptr->is_number(sc->vptr->pair_car(sc->vptr->pair_cdr(args)))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            unsigned int c=sc->vptr->ivalue(sc->vptr->pair_car(sc->vptr->pair_cdr(args)));
            for (int i=0; i<c; i++) {
                machine_run(swarm[m]);
            }
        }
    }
    return sc->NIL;
}

pointer sc_machine_say(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            return sc->vptr->mk_integer(
                sc,swarm[m]->m_threads[0].m_dma);
        }
    }
    return sc->NIL;
}

pointer sc_machine_say_addr(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            return sc->vptr->mk_integer(
                sc,swarm[m]->m_threads[0].m_dma_addr);
        }
    }
    return sc->NIL;
}

pointer sc_machine_say_size(scheme *sc, pointer args) {
    if (args!=sc->NIL) {
        if(sc->vptr->is_number(sc->vptr->pair_car(args))) {
            unsigned int m=sc->vptr->ivalue(sc->vptr->pair_car(args));
            return sc->vptr->mk_integer(
                sc,swarm[m]->m_threads[0].m_dma_size);
        }
    }
    return sc->NIL;
}

#define SDECL(name,func) \
sc->vptr->scheme_define(\
    sc,\
    sc->global_env,\
    sc->vptr->mk_symbol(sc, name),\
    sc->vptr->mk_foreign_func(sc, func));

int main(void) {
  scheme *sc;
  
  // Init Scheme interpreter
  sc = scheme_init_new();
  scheme_set_output_port_file(sc, stdout);
  
  // Load init.scm
  FILE *finit = fopen("../tinyscheme/init.scm", "r");
  scheme_load_file(sc, finit);
  fclose(finit);

  SDECL("machine-create",sc_machine_create);
  SDECL("machine-peek",sc_machine_peek);
  SDECL("machine-poke",sc_machine_poke);
  SDECL("machine-run",sc_machine_run);
  SDECL("machine-say",sc_machine_say);
  SDECL("machine-say-addr",sc_machine_say_addr);
  SDECL("machine-say-size",sc_machine_say_size);

  FILE *fus = fopen("reactor.scm", "r");
  scheme_load_file(sc, fus);
  fclose(fus);
  
  // Bye!
  scheme_deinit(sc);
  return 0;
}

