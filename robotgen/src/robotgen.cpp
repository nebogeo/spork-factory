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

u8 test[]={
    ORG,
    PSHL,0,
    PSHL,0,
    OUT,
    OUT,
    PSHL,1,
    PSHL,1,
    OUT,
    OUT,
    JMP,0,
};

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


int main(int argc, char *argv[]) {

    unit_test();


/*    machine *m=(machine *)malloc(sizeof(machine));
    machine_create(m);
    // make 1 thread active
    thread_set_active(&m->m_threads[0],1);

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

    // read in the parameters
    u32 cycles=atof(argv[1]);
    u32 fft_chunk_size=atof(argv[2]);

    // pre-run/warm up
    for(u32 i=0; i<5000; i++) {
        machine_run(m);
    }

    // reset output values
    for (u32 i=0; i<OUTPUT_SIZE; i++) m->m_threads[0].m_output[i]=0;
    m->m_threads[0].m_outpos=0;

    // run!
    for(u32 i=0; i<cycles; i++) {
        machine_run(m);
    }

*/

  
	return 0;
}

