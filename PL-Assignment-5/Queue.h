/* File: Queue.h , queues of ints */
typedef enum {false, true} boolean;
typedef struct QueueStruct * QueueHndl ;

/* Constructor-destructor */
QueueHndl NewQueue(void);
void FreeQueue(QueueHndl * ptrQ);

/* Access functions */
int getFront(QueueHndl Q); // return the first element
int getLength(QueueHndl Q); // return queue length
boolean isEmpty(QueueHndl Q); // is the queue empty ?

/* Manipulation procedures */
void Enqueue(QueueHndl Q, int data);
void Dequeue(QueueHndl Q);