int main() {
    int a = 1;

mylabel:
    a = 2;

    goto mylabel;

    return 0;
}