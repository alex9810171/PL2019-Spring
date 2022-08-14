#include <stdlib.h>
#include <stdio.h>
#include "Queue.h"
int main() {
  QueueHndl q = NewQueue();
  int i =10, j=20;
  Enqueue(q, i);
  Enqueue(q, j);
  printf("Queue length =%d\n", getLength(q));
  if (!isEmpty(q)) printf("Front of queue is %d\n", getFront(q));
  Dequeue(q);
  if (!isEmpty(q)) printf("Front of queue is %d\n", getFront(q));
  FreeQueue(&q);
  return 0;
}