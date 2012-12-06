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

u32 rprogram_size=16;
u8 rprogram[]={
    18, 160, 147, 20, 241, 28, 12, 23, 5, 114, 249, 156, 20, 246, 1, 71
};

int main(int argc, char *argv[]) {

//    unit_test();

//    srand(time(NULL));

    machine *m=(machine *)malloc(sizeof(machine));
    machine_create(m);
    // make 1 thread active
    thread_set_active(&m->m_threads[0],1);

//    write_mem(m,rprogram,rprogram_size);

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

    // read in the parameters
    u32 cycles=atof(argv[1]);
    float fitness=0;

    // do multiple runs with the same code
    for (u32 run=0; run<10; run++)
    {
        // create a robot model (random position and direction)
        robot r(srndvec2().mul(100),srndvec2());
        light l(srndvec2().mul(100));
        
        //m->m_threads[0].m_portb=0xff;

        // run!
        for(u32 i=0; i<cycles; i++) {
            r.update(l);
            
            if (r.m_left_eye) m->m_threads[0].m_portb|=LEFT_EYE;
            else m->m_threads[0].m_portb&=~LEFT_EYE;
            if (r.m_right_eye) m->m_threads[0].m_portb|=RIGHT_EYE;
            else m->m_threads[0].m_portb&=~RIGHT_EYE;
            
//        cerr<<byte_to_binary(m->m_threads[0].m_portb)<<endl;
            
            machine_run(m);
            
            r.m_left_motor=0.1;
            r.m_right_motor=0.1;
            if ((m->m_threads[0].m_portb&LEFT_MOTOR)>0) r.m_left_motor=0;
            if ((m->m_threads[0].m_portb&RIGHT_MOTOR)>0) r.m_right_motor=0;
            
//            cout<<r.m_pos.x<<" "<<r.m_pos.y<<endl;
        }

        fitness+=r.m_fitness;
    }

    // return average fitness
    cout<<fitness/10.0f<<endl;
  
	return 0;
}

