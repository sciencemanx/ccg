#include "lib.h"

int main() {
    if ((randint() & 1) == 0) {
        success();
        return 0;
    } else {
        failure();
        return 1;
    }
}