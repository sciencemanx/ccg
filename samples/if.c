#include <stdio.h>

int main(int argc, char **argv) {
    int x;
    x = 0;
    if (x == 1) {
        puts("this probably shouldnt happen");
        x = 1336;
    } else {
        puts("this should happen");
        x = 31336;
    }
    x = x + 1;
    return x;
}