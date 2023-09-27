#include <string.h>
#include <stdio.h>

#define VGA_ROWS 30
#define VGA_COLS 80

#define COLOR_BLACK 0
#define COLOR_BLUE 1
#define COLOR_GREEN 2
#define COLOR_CYAN 3
#define COLOR_RED 4
#define COLOR_MAGENTA 5
#define COLOR_BROWN 6
#define COLOR_GRAY 7
#define COLOR_DARK_GRAY 8
#define COLOR_LIGHT_BLUE 9
#define COLOR_LIGHT_GREEN 10
#define COLOR_LIGHT_CYAN 11
#define COLOR_LIGHT_RED 12
#define COLOR_LIGHT_MAGENTA 13
#define COLOR_YELLOW 14
#define COLOR_WHITE 15

#define VGA_OFFSET_COL 1
#define VGA_OFFSET_ROW 128

#define COLOR(fg, bg) ((fg) + ((bg) << 4))
#define VGA_OFFSET(col, row) ((col) + ((row) << 7))

#define VGA_CHAR_SEG ((unsigned char*)0xe000)
#define VGA_COLOR_SEG ((unsigned char*)0xd000)

static void vga_clear(unsigned char color) {
    unsigned char *pchar = VGA_CHAR_SEG;
    unsigned char *pcolor = VGA_COLOR_SEG;
    for (int i = 0; i != VGA_ROWS * 128; ++i) {
        *pchar = 0;
        *pcolor = color;
        ++pchar;
        ++pcolor;
    }
}

void main(void) {
    vga_clear(COLOR(COLOR_GRAY, COLOR_BLACK));

    sprintf(VGA_CHAR_SEG, "Hello world! %u %020lu %c %04X", 1234, 100000ul, 60, 0xF00D);
}

int getchar(void) { return 0; }
int putchar(int c) { return 0; }
