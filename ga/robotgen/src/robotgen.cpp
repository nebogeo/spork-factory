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

#include "../core/bbcore.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <time.h>

#include <iostream>
#include <vector>

#include "robot.h"

using namespace std;

//////////////////////////////////////////////////

void split(const string& str,
           vector<string>& tokens,
           const string& delimiters = " ")
{
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    string::size_type pos     = str.find_first_of(delimiters, lastPos);
    
    while (string::npos != pos || string::npos != lastPos)
    {
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        lastPos = str.find_first_not_of(delimiters, pos);
        pos = str.find_first_of(delimiters, lastPos);
    }
}

void unit_test()
{
    robot r(vec2(0,0), vec2(1,0));
    light l(vec2(0,100));
    r.update(l);
    
    cerr<<r.m_left_eye<<" should be true"<<endl;
    cerr<<r.m_right_eye<<" should be false"<<endl;

    r.m_dir.x=-1;
    r.update(l);
    
    cerr<<r.m_left_eye<<" should be false"<<endl;
    cerr<<r.m_right_eye<<" should be true"<<endl;

    r.m_left_motor=0.5;
    r.m_right_motor=0.5;
    r.m_dir.x=1;
    r.update(l);
    cerr<<r.m_pos.x<<" should be 1"<<endl;

    for (int i=0; i<1000; i++) {
        r.m_right_motor=i/1000.0f;

        r.update(l);
//        cerr<<"dir:"<<r.m_dir.x<<" "<<r.m_dir.y<<endl;
//        cerr<<r.m_dir.mag()<<endl;
        cerr<<r.m_pos.x<<" "<<r.m_pos.y<<endl;
    }

}

u32 rprogram_size=32;
u8 rprogram[]={
//    211, 155, 2, 126, 24, 54, 12, 90, 153, 154, 85, 25, 155, 51, 73, 9

//    153, 215, 190, 169, 2, 23, 140, 8, 199, 102, 240, 25, 24, 186, 145, 32, 190, 19, 70, 147, 23, 96, 25, 3, 28, 135, 116, 247, 150, 246, 33, 116

    LEYE, RMOT, REYE, LMOT, JMP, 0
};

//#define TEST_MODE

#ifdef TEST_MODE
#define N_RUNS 10
#else
#define N_RUNS 10
#endif

int main(int argc, char *argv[]) {

//    unit_test();
//    srand(time(NULL));

    machine *m=(machine *)malloc(sizeof(machine));
    machine_create(m);
    // make 1 thread active
    thread_set_active(&m->m_threads[0],1);

#ifdef TEST_MODE
    write_mem(m,rprogram,rprogram_size);
#else
    // load the code from stdin
    string inp;
    cin>>inp;
    vector<string> splitted;
    split(inp,splitted,",");
    
    u32 count=0;
    for(vector<string>::iterator i=splitted.begin();
        i!=splitted.end(); ++i) {
        machine_poke(m,count++,atof(i->c_str()));
    }


    for(vector<string>::iterator i=splitted.begin();
        i!=splitted.end(); ++i) {
        machine_poke(m,count++,atof(i->c_str()));
    }
#endif

    // read in the parameters
    u32 cycles=atof(argv[1]);
    float fitness=0;

    light l(vec2(0,0));

    // do multiple runs with the same code
    for (u32 run=0; run<N_RUNS; run++)
    {
        // create a robot model (random position and direction)
//        robot r(vec2(-100,-10),vec2(1,0));
        robot r(srndvec2().mul(100),srndvec2().normalised());
        
        //m->m_threads[0].m_portb=0xff;

        // run!
        for(u32 i=0; i<cycles; i++) {
            r.update(l);
            
            if (r.m_left_eye) m->m_threads[0].m_portb|=LEFT_EYE;
            else m->m_threads[0].m_portb&=~LEFT_EYE;
            if (r.m_right_eye) m->m_threads[0].m_portb|=RIGHT_EYE;
            else m->m_threads[0].m_portb&=~RIGHT_EYE;
            
            //cerr<<"left "<<r.m_left_eye<<endl;
            //cerr<<"right "<<r.m_right_eye<<endl;
            //cerr<<byte_to_binary(m->m_threads[0].m_portb)<<endl;
            
            machine_run(m);
            
            r.m_left_motor=0.1;
            r.m_right_motor=0.1;
            if ((m->m_threads[0].m_portb&LEFT_MOTOR)>0) r.m_left_motor=0;
            if ((m->m_threads[0].m_portb&RIGHT_MOTOR)>0) r.m_right_motor=0;

#ifdef TEST_MODE
            cout<<r.m_pos.x<<" "<<r.m_pos.y<<endl;
#endif
        }

        fitness+=r.m_fitness;
    }

    // return average fitness
    fitness/=(float)N_RUNS;
    if (fitness>100) fitness=0;
    else fitness=100-fitness;
    cout<<fitness<<endl;

	return 0;
}

