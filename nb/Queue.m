(* ::Package:: *)

BeginPackage["Queue`", {"OOP`"}];

(* ---------------------------------------------------------- Public Elements *)

Queue::usage = 
  "Abstract base class for all queues, cannot be instantiated.";

Enqueue::usage =
  "Enqueue[queue, elem] enqueues elem to queue.";

Dequeue::usage =
  "Dequeue[queue] returns the element at the top of the queue.";

EmptyQ::usage =
  "EmptyQ[queue] tests if queue is empty.";

FifoQueue::usage = 
  "First-in-first-out queue. ";

LifoQueue::usage =
  "Last-in-first-out queue (a.k.a. stack).";

PriorityQueue::usage =
  "Priority queue (a.k.a heap). " <>
  "Example: myHeap = New[PriorityQueue, p] " <>
  "constructs a priority queue using p as ordering function.";

Enumerator::usage = 
  "e = New[Enumerator, queue, f] constructs an Enumerator " <> 
  "with transition function f. " <>
  "f should take one argument corresponding to the queue's element type " <> 
  "and return a list of elements of same type to be enqueued.";

GetNext::usage =
  "GetNext[e] returns the next element of the Enumerator e. " <>
  "The next element n is taken from the top of the Enumerator's queue " <>
  "and all elements returned by f[n] are added to the queue.";

HasNext::usage = 
  "HasNext[e] tests if the Enumerator e has a next element. " <>
  "This is the case, if and only if the Enumerator's queue is not empty."; 

Begin["`Private`"];

(* -------------------------------------------------------------- Class Queue *)
NewAbstractClass[Queue];

Init[Queue, obj_] ^:= Null;

(* ---------------------------------------------------------- Class FifoQueue *)
NewClass[FifoQueue];

Init[FifoQueue, obj_] ^:= (
  Super[Queue, obj];
  Contents@obj ^= {};
);

Enqueue[queue_?(InstanceQ[FifoQueue]), elem_] := (
  Contents@queue ^= Append[Contents@queue, elem];
);

EmptyQ[queue_?(InstanceQ[FifoQueue])] := (
  Length@Contents@queue == 0
);

Dequeue[queue_?(InstanceQ[FifoQueue])] := 
Module[{elem},
  elem = First@Contents@queue;
  Contents@queue ^= Rest@Contents@queue;
  elem
];

(* ---------------------------------------------------------- Class LifoQueue *)
NewClass[LifoQueue];

Init[LifoQueue, obj_] ^:= (
  Super[Queue, obj];
  Contents@obj ^= {};
);

Enqueue[queue_?(InstanceQ[LifoQueue]), elem_] := (
  Contents@queue ^= {elem, Contents@queue};
);

EmptyQ[queue_?(InstanceQ[LifoQueue])] := (
  Length@Contents@queue == 0
);

Dequeue[queue_?(InstanceQ[LifoQueue])] := 
Module[{elem},
  elem = Contents[queue][[1]];
  Contents@queue ^= Contents[queue][[2]];
  elem
];

(* ------------------------------------------------------ Class PriorityQueue *)
NewClass[PriorityQueue];

Init[PriorityQueue, obj_, orderingFunction_] ^:= (
  Super[Queue, obj];
  OrderingFunction[obj] ^= orderingFunction;
  Contents[obj] ^= Table[Null, {i,16}];
  Size[obj] ^= 0;
);

Enqueue[queue_?(InstanceQ[PriorityQueue]), elem_] := 
Module[{contents},
  contents = Grow[queue];
  Size@queue ^= Size@queue + 1;
  contents[[Size@queue]] = elem;
  Contents@queue ^= contents;
  Contents@queue ^= HeapUp[queue];
];

Grow[queue_] := Module[{contents, capacity},
  contents = Contents@queue;
  capacity = Length@contents;
  If[Size@queue >= capacity,
    contents = Join[contents, Table[Null, {i, capacity}]];
  ]; 
  contents
];

HeapUp[queue_] := 
Module[{contents, orderingFunction, i, p},
  contents = Contents@queue;
  orderingFunction = OrderingFunction@queue;
  i = Size@queue;
  p = BitShiftRight[i, 1];
  While[p > 0 && !orderingFunction[contents[[p]], contents[[i]]],
    {contents[[p]], contents[[i]]} = {contents[[i]], contents[[p]]};
    i = p; 
    p = BitShiftRight[p, 1];
  ]; 
  contents
];

EmptyQ[queue_?(InstanceQ[PriorityQueue])] := (
  Size@queue == 0
);

Dequeue[queue_?(InstanceQ[PriorityQueue])] := 
Module[{contents, top},
  contents = Contents@queue;
  top = contents[[1]];
  contents[[1]] = contents[[Size[queue]]];
  Size[queue] ^= Size[queue] - 1;
  Contents@queue ^= contents;
  Contents@queue ^= HeapDown[queue];
  top
];

HeapDown[queue_] :=
Module[{contents, orderingFunction, size, i, c},
  contents = Contents@queue;
  orderingFunction = OrderingFunction@queue;
  size = Size@queue;
  i = 1;
  c = SmallerChild[contents, queue, i];
  While[c <= size && !orderingFunction[contents[[i]], contents[[c]]],
    {contents[[i]], contents[[c]]} = {contents[[c]], contents[[i]]};
    i = c;
    c = SmallerChild[contents, queue, i];
  ];
  contents
];

SmallerChild[contents_, queue_, i_] := 
Module[{c},
  c = BitShiftLeft[i, 1];
  If[c+1 <= Size@queue && OrderingFunction[queue][contents[[c+1]], contents[[c]]],
    c++;
  ];
  c
];

(* --------------------------------------------------------- Class Enumerator *)
NewClass[Enumerator];

Init[Enumerator, obj_, queue_?(InstanceQ[Queue]), f_] ^:= (
  Queue@obj ^= queue;
  TransitionFunction@obj ^= f;
);

HasNext[obj_?(InstanceQ[Enumerator])] := (
  !EmptyQ[Queue@obj]
);

GetNext[obj_?(InstanceQ[Enumerator])] :=
Module[{q, next},
  q = Queue@obj;
  next = Dequeue[q];
  Do[Enqueue[q, e], {e, TransitionFunction[obj][next]}];
  next
];

End[];

Protect[Queue, Enqueue, Dequeue, EmptyQ];
Protect[FifoQueue, LifoQueue, PriorityQueue];
Protect[Enumerator, GetNext, HasNext];

EndPackage[];
