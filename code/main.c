// #include <stdio.h>
// #include "dir/add.h"
// #define X 5
// #define STR(x) #x
// #define PRINTSTR(x) printf("%s", # x)
#define PRINTSTR(x, y) printf("%s", #x#y)
// #define PRINTSTR(x, y) printf("%s %s", #x, #y)
// #define VAR(a, b) a ## b

/* enum Days : short {
    Mon,
    Tue,
    Wen = -999,
    Tue,
    Fri = 77777
}; */

int main() {
    // char s = X;
    // char day = Tue;
    // char a = STR(a);
    // int l = X;
    // PRINTSTR(  hello    world  );
    PRINTSTR(hello , world );
    // int VAR(a, 1) = 5;
    // int a = ADD(5, 7);
    return 0;
}