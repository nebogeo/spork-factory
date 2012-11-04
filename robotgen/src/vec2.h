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

#include <math.h>

#ifndef VEC2
#define VEC2

class vec2 
{
public:
    vec2(float a, float b): x(a), y(b) {}
    float dot(vec2 &other) { return x*other.x+y*other.y; }
    vec2 add(vec2 &other) { return vec2(x+other.x,y+other.y); }
    vec2 sub(vec2 &other) { return vec2(x-other.x,y-other.y); }
    vec2 div(float v) { return vec2(x/v,y/v); }
    float mag() { return sqrt(x*x+y*y); }
    vec2 normalised() { return div(mag()); }
    
    float dist(vec2 &other)
    {
        return sqrt((other.x-x)*(other.x-x)+
                    (other.y-y)*(other.y-y));
    }

    float x;
    float y;
};

#endif
