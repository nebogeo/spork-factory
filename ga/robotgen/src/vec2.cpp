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

#include "vec2.h"

float rndf() { return rand()%10000/10000.0f; }
float crndf() { return (rndf()-0.5)*2; }
vec2 rndvec2() { return vec2(rndf(),rndf()); }
vec2 crndvec2() { return vec2(crndf(),crndf()); }
vec2 srndvec2() 
{
    vec2 v(0,0);
    v=crndvec2();
    while (v.mag()>1) {
        v=crndvec2();
    }
    return v;
}
