#include <stdlib.h>
#include "Queue.h"/* file Queue-adt.c */
/* other includes such as stdio.h, stdlib.h, etc. */
typedef struct Node /* a private inner struct which is not exported */
{
  int data;
  struct Node* next;
} Node;

typedef struct QueueStruct
{
  Node* front;
  Node* back;
  int length;
} QueueStruct;

Node *NewNode(void);
void FreeNode(Node* ptrN);

/* Constructor-Destructor */
QueueHndl NewQueue(void)
{
  QueueHndl Q;
  Q = malloc(sizeof(QueueStruct));
  Q->front = Q->back = NULL;
  Q->length = 0;
  return(Q);
}
void FreeQueue(QueueHndl* ptrQ) 
{
  int i;
  while((*ptrQ)->length != 0)
  {
    Dequeue(*ptrQ);
  }
}

/* Access functions */
int getFront(QueueHndl Q) 
{
  return Q->front->data;
}
int getLength(QueueHndl Q)
{
  return Q->length;
}
boolean isEmpty(QueueHndl Q)
{
  if(Q->length == 0) return true;
  else return false; 
}

/* Manipulation procedures */
void Enqueue(QueueHndl Q, int data)
{
  Node *newNode = NewNode();
  if(newNode != NULL){
    newNode->data = data;
    newNode->next = NULL;
    if(Q->front == NULL) Q->front = newNode;
    else Q->back->next = newNode;
    Q->back = newNode;
    Q->length ++;
  }
}
void Dequeue(QueueHndl Q)
{
  Node *freeNode = Q->front;
  Q->front = Q->front->next;
  if(Q->front == NULL) Q->back = NULL;
  Q->length --;
  FreeNode(freeNode);
}

Node *NewNode(void)
{
  Node *newNode;
  newNode = malloc(sizeof(Node));
  return newNode;
}
void FreeNode(Node* ptrN)
{
  ptrN->data = 0;
  ptrN->next = NULL;
  free(ptrN);
}