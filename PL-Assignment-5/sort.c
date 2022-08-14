#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct grade {
  char *name; float grade;
} grade;
int i;
grade students[5];
char *names[5] = {"John", "Eric", "Paul", "Carol", "Jim"};
float grades [5] = { 90, 76, 80, 70, 60 };
int compare1(const void *x, const void *y)
{
  grade *a, *b;
  a = (grade *)x;
  b = (grade *)y;
  if(a->grade < b->grade) return -1;
  else if(a->grade > b->grade) return 1;
  else return 0;
} //your code
int compare2(const void *x, const void *y)
{
  grade *a, *b;
  a = (grade *)x;
  b = (grade *)y;
  return strcmp(a->name, b->name);
} //your code
int main() {
  for (i=0; i<5; i++) {
    students[i].name = names[i];
    students[i].grade = grades[i];
  }
  // call qsort twice on students
  // one to sort by name, the other by grade
  qsort(students, 5, sizeof(grade), compare1); //your code
  for (i=0; i<5; i++) {
    printf("(%s, %f) ", students[i].name, students[i].grade);
  }
  printf("\n");
  qsort(students, 5, sizeof(grade), compare2); //your code

  for (i=0; i<5; i++) {
    printf("(%s, %f)\n", students[i].name, students[i].grade);
  }
  printf("\n");
  return 0;
}// main
 //output:
 // (1) (Eric, 60.000000) … (John, 90.000000)
 // (2) (Carol, 70.00000) (Eric, 60.00000) … (Paul, 80.000000)