#include <avr/io.h>
#include <util/delay.h>

typedef unsigned char u8;
typedef unsigned int u32;

#define LEFT_MOTOR PB4
#define RIGHT_MOTOR PB3
#define LEFT_EYE PB0
#define RIGHT_EYE PB1
#define LED PB2

// change motor state
// m: 0=left 1=right
// s: 0=off 1=on 
void set_motor_state(u8 m, u8 s)
{
    u8 motor = m==0?_BV(LEFT_MOTOR):_BV(RIGHT_MOTOR);
    if (s) PORTB&=~motor;
    else PORTB|=motor;
}

u8 get_eye_state(u8 e)
{
    u8 eye = e==0?_BV(LEFT_EYE):_BV(RIGHT_EYE);
    return PINB&eye; 
}

void set_led_state(u8 s)
{
    if (s) PORTB|=_BV(LED);
    else PORTB&=~_BV(LED);
}

int main(void)
{
    DDRB = 0x00;
    DDRB |= _BV( LEFT_MOTOR );
    DDRB |= _BV( RIGHT_MOTOR );
    DDRB |= _BV( LED );

    // activate pull up resistors
    PORTB|=_BV(LEFT_EYE);
    PORTB|=_BV(RIGHT_EYE);

    set_motor_state(0,0);
    set_motor_state(1,0);

    u32 ledtime=0;

    while(1) {
//        set_motor_state(0,(ledtime++)%5000<2500);
//        set_led_state((ledtime++)%2500<1200);
//        if (get_eye_state(0)) set_motor_state(0,1);
//        else set_motor_state(0,0);

        if (get_eye_state(1)) set_led_state(0);
        else set_led_state(1);

    } 
}

