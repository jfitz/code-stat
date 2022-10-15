/*
** DRAW: A Light Pen Demonstration Program
**
** This program should be compiled, and then linked with the PEN.ASM
** module, using the following commands;
** CL DRAW.C /c
** LINK DRAW.OBJ PEN.OBJ /NOI;
*/

main() {
    int x, y, h;
    for(y = 0; y < 24;y++) {    /* fill the screen with periods */
        for(x = 0; x < 80;x++) {
            printf(".");
            }
        printf("\n");
        }
    pen_on(); /* enable the light pen */
    do {                        /* indicate each light pen hit with '*' */
        h = pen_hit();
        if( h != -1) {
            printf("\33Y%c%c*", (h/80) + ' ', (h % 80) + ' ');
            }
        } while(kbhit() == 0); /* keep going until a key is struck */
    pen_off();                  /* disable light pen */
    }

