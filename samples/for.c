#include <stdio.h>

int main(int argc, char **argv) {
    int i, sum;
    for (i = 0; i < 10; i++) {
        sum += i;
    }
    printf("sum: %d\n", sum);
    return sum;
}