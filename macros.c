#include <stdio.h>

// Define some mathematical macros
#define SQUARE(x) ((x) * (x))
#define CUBE(x) ((x) * (x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

// Conditional compilation
#define DEBUG 1

// Debugging macro
#if DEBUG
    #define DEBUG_PRINT(fmt, ...) printf("DEBUG: " fmt, __VA_ARGS__)
#else
    #define DEBUG_PRINT(fmt, ...)
#endif

// Macro for array size
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

int main() {
    int num = 5;
    int arr[] = {1, 2, 3, 4, 5};

    // Using mathematical macros
    printf("Square of %d: %d\n", num, SQUARE(num));
    printf("Cube of %d: %d\n", num, CUBE(num));

    // Using MAX and MIN macros
    int a = 10, b = 20;
    printf("Max of %d and %d: %d\n", a, b, MAX(a, b));
    printf("Min of %d and %d: %d\n", a, b, MIN(a, b));

    // Using the array size macro
    printf("Size of array: %zu\n", ARRAY_SIZE(arr));

    // Debug print
    DEBUG_PRINT("This is a debug message with num: %d\n", num);

    return 0;
}