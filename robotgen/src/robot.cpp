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

#include "robot.h" 

robot::robot(vec2 pos, vec2 dir): 
    m_left_eye(false),
    m_right_eye(false),
    m_fitness(0),
    m_pos(pos), 
    m_dir(dir), 
    m_left_motor(0),
    m_right_motor(0)
{}

void robot::update(light &l)
{
    // update dir and pos from motors
    float relative_speed_diff=m_left_motor-m_right_motor;
    float angle=atan(relative_speed_diff);
    
    // rotate the direction by the angle
    float nx=(m_dir.x*cos(angle))-(m_dir.y*sin(angle));
    float ny=(m_dir.x*sin(angle))+(m_dir.y*cos(angle));
    m_dir.x=nx;
    m_dir.y=ny;
    
    // update the position
    m_pos=m_pos.add(m_dir);
    
    // update ldrs
    vec2 to_light=l.m_pos.sub(m_pos).normalised();
    vec2 sideways(m_dir.y,m_dir.x);
    float d=to_light.dot(sideways);
    m_left_eye=false;        
    m_right_eye=false;
    if (d>0) m_left_eye=true;
    else m_right_eye=true;
    
    // how close we are to the light
    m_fitness=l.m_pos.dist(m_pos);
}

