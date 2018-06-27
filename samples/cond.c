
int add(int i, int j) {
    return i + j;
}

int main() {
    int i, j, sum;

    sum = 0;
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {
            sum = add(sum, add(i, j));
        }
    }
}