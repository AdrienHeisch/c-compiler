struct Person {
    int age;
}

int main() {
    struct Person persons[1];
    int p = persons[0];
    int a = p.age;
    int *b = &persons[3].age;
    return 0;
}