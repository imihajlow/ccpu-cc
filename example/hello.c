#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <libsys/ps2keyboard.h>
#include <libsys/vga.h>


void main(void) {
    vga_clear(COLOR(COLOR_GRAY, COLOR_BLACK));

    sprintf(VGA_CHAR_SEG, "Hello world! %u %020lu %c %04X", 1234, 100000ul, 60, 0xF00D);
    uint8_t row = 1;
    uint8_t col = 0;
    while (1) {
        uint16_t c = ps2_get_ascii();
        if (c) {
            if (PS2_IS_ASCII(c)) {
                VGA_CHAR_SEG[VGA_OFFSET(col, row)] = c;
                col += 1;
                if (col == VGA_COLS) {
                    col = 0;
                    row += 1;
                }
            } else if ((uint8_t)c == PS2_KEY_ENTER) {
                row += 1;
                col = 0;
            }
            if (row == VGA_ROWS) {
                row = 0;
            }
        }
    }
}

int getchar(void) { return 0; }
int putchar(int c) { return 0; }
