// #include <stdio.h>
// #define X 5
#define STR(x) #x
// #define PRINTSTR(x) printf("%s", #x)
// #define PRINTSTR(x, y) printf("%s", #x #y)
// #define VAR(a, b) a ## b

/* enum Days : short {
    Mon,
    Tue,
    Wen = -999,
    Tue,
    Fri = 77777
}; */

int main() {
    // char day = Tue;
    // char a = 'a';
    // int l = X;
    char s[] = STR(hello world);
    // char s[] = STR(hello);
    // int VAR(a, 1) = 5;
    // return 0;
}