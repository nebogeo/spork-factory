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
using namespace std;

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

double *fft(u8* data, u32 len)
{
    double ReTmp = 0.0;
    double ImTmp = 0.0;
    double *ReXf = new double[len];
    double *ImXf = new double[len];
    double *ret = new double[len];
    
    for (u32 k=0; k<len; ++k) {
        ReTmp = 0.0;
        ImTmp = 0.0;
        for (u32 n = 0; n < len; ++n) {
            ReTmp += data[n] * cos((2*M_PI*n*k)/len) * 255;
            ImTmp += data[n] * sin((2*M_PI*n*k)/len) * 255;
        }        
        ReXf[k] = ReTmp;
        ImXf[k] = ImTmp;
    }

    for (u32 k = 0; k < len; ++k) {
        ret[k]=sqrt(ReXf[k]*ReXf[k] + ImXf[k]*ImXf[k]);
    }

    delete[] ReXf;
    delete[] ImXf;
    return ret;
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

int main(int argc, char *argv[]) {
    machine *m=(machine *)malloc(sizeof(machine));
    machine_create(m);
    // make 1 thread active
    thread_set_active(&m->m_threads[0],1);

    // clear the output recording
    for (u32 i=0; i<OUTPUT_SIZE; i++) m->m_threads[0].m_output[i]=0;
    m->m_threads[0].m_outpos=0;

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

    // now run fft and output results to stdout
    u32 sample_count=m->m_threads[0].m_outpos;
    u8* samples=m->m_threads[0].m_output;
    u32 num_fft_segments = sample_count/fft_chunk_size;
    u32 current_pos=0;
    

    // do the fft in chunks (to encourage changing frequencies)
    for (u32 i=0; i<num_fft_segments; i++) {
        double* fft_chunk=fft(samples+(i*fft_chunk_size),fft_chunk_size);
        
        for(u32 j=0; j<fft_chunk_size; j++) {
            cout<<fft_chunk[j]<<" ";
        }

        current_pos+=fft_chunk_size;

        delete[] fft_chunk;
    }

    // if we don't have enough, or some left over,
    // simply run fft on what we do have
    if (current_pos<sample_count) {
        double* fft_chunk=fft(samples+current_pos,
                              sample_count-current_pos);
        
        for(u32 j=0; j<sample_count-current_pos; j++) {
            cout<<fft_chunk[j]<<" ";
        }
        
        delete[] fft_chunk;
    }
  
	return 0;
}

