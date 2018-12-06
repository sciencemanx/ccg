#include "lib.h"

int main(int argc, char **argv) {
    int x;
    x = 0;
    if (x == 1) {
        failure();
        x = 1336;
    } else {
        success();
        x = 31336;
    }
    x = x + 1;
    return x;
}