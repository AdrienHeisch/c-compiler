// Imports
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// WHAT

/* OH

LOOK AT THIS COMMENT
*/

// Define a structure
struct Student {
    int id;
    char name[50];
    float grade;
};

// Function prototypes
void printStudent(struct Student *s);
void sortStudents(struct Student *students, int count);
void writeToFile(struct Student *students, int count, const char *filename);
void readFromFile(struct Student *students, int *count, const char *filename);

int main() {
    // Variable declarations
    struct Student students[100];
    int count = 0;
    char filename[] = "students.txt";

    // Input student data
    printf("Enter number of students: ");
    scanf("%d", &count);
    for (int i = 0; i < count; i++) {
        printf("Enter ID, Name, and Grade for student %d: ", i + 1);
        scanf("%d %s %f", &students[i].id, students[i].name, &students[i].grade);
    }

    // Print student data
    printf("\nStudent Data:\n");
    for (int i = 0; i < count; i++) {
        printStudent(&students[i]);
    }

    // Sort students by grade
    sortStudents(students, count);
    printf("\nSorted Student Data (by grade):\n");
    for (int i = 0; i < count; i++) {
        printStudent(&students[i]);
    }

    // Write to file
    writeToFile(students, count, filename);
    printf("\nStudent data written to %s\n", filename);

    // Read from file
    struct Student fileStudents[100];
    int fileCount = 0;
    readFromFile(fileStudents, &fileCount, filename);
    printf("\nData read from file:\n");
    for (int i = 0; i < fileCount; i++) {
        printStudent(&fileStudents[i]);
    }

    return 0;
}

// Function to print student details
void printStudent(struct Student *s) {
    printf("ID: %d, Name: %s, Grade: %.2f\n", s->id, s->name, s->grade);
}

// Function to sort students by grade (Bubble Sort)
void sortStudents(struct Student *students, int count) {
    for (int i = 0; i < count - 1; i++) {
        for (int j = 0; j < count - i - 1; j++) {
            if (students[j].grade > students[j + 1].grade) {
                struct Student temp = students[j];
                students[j] = students[j + 1];
                students[j + 1] = temp;
            }
        }
    }
}

// Function to write student data to a file
void writeToFile(struct Student *students, int count, const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        perror("Error opening file");
        return;
    }
    for (int i = 0; i < count; i++) {
        fprintf(file, "%d %s %.2f\n", students[i].id, students[i].name, students[i].grade);
    }
    fclose(file);
}

// Function to read student data from a file
void readFromFile(struct Student *students, int *count, const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        return;
    }
    while (fscanf(file, "%d %s %f", &students[*count].id, students[*count].name, &students[*count].grade) == 3) {
        (*count)++;
    }
    fclose(file);
}