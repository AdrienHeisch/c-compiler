int main() {
    int a = 1;

    switch (a) {
        case 0:
            a = 1;
            break;
        case 1:
            a = 0;
            break;
        //default:
            return 1;
    }

    return 0;
}