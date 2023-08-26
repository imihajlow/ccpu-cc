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

static int vga_clear(unsigned char color) {
    unsigned char *pchar = VGA_CHAR_SEG;
    unsigned char *pcolor = VGA_COLOR_SEG;
    for (int i = 0; i != VGA_ROWS * 128; ++i) {
        *pchar = 0;
        *pcolor = color;
        ++pchar;
        ++pcolor;
    }
    return 1;
}

static void putstring(unsigned char col, unsigned char row, const char *s) {
    unsigned char *p = VGA_CHAR_SEG + VGA_OFFSET(col, row);
    while (*s) {
        *p = *s;
        ++p;
        ++s;
    }
}

static void itoa(char *buf, unsigned int x) {
    if (x == 0) {
        *buf = '0';
        ++buf;
    } else {
        char tmp[5];
        char *p = tmp;
        while (x) {
            *p = (x % 10) + '0';
            x /= 10;
            ++p;
        }
        while (p != tmp) {
            --p;
            *buf = *p;
            ++buf;
        }
    }
    *buf = 0;
}

void main(void) {
    int x = vga_clear(COLOR(COLOR_GRAY, COLOR_BLACK));
    putstring(0, 0, "Hello World!");
    char buf[10];
    itoa(buf, 239);
    putstring(0, 1, buf);
    while (1) ;
}
