
#include <stdio.h>
#include <stdlib.h>

int randint() {
    return rand();
}

int getint() {
    int i;

    while (scanf("%d\n", &i) != 1);

    return i;
}

void success() {
    printf("success!");
}

void failure() {
    printf("failure!");
}

void puti(int i) {
    printf("%d\n", i);
}