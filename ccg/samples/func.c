
int add(int i, int j) {
    return i + j;
}

int main() {
    int i, sum;

    sum = 0;
    for (i = 0; i < 10; i++) {
        sum = add(i, sum);
    }

    return sum;
}
