#include <stdio.h>
#include <stdlib.h>

void swap(int *a, int *b) {
    int tmp;

    tmp = *a;
    *a = *b;
    *b = tmp;
}

int main(int argc, char **argv) {
    int x, y;

    printf("x: %d, y: %d\n", x, y);
    swap(&x, &y);
    printf("x: %d, y: %d\n", x, y);

    return 0;
}
