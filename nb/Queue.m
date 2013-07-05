(* ::Package:: *)

BeginPackage["Queue`", {"OOP`"}];

Unprotect[Queue, Enqueue, Dequeue, EmptyQ];
Unprotect[FifoQueue, LifoQueue, PriorityQueue];
Unprotect[SearchIteartor, BFSIterator, DFSIterator, PFSIterator];

(* ---------------------------------------------------------- Public Elements *)

Queue::usage = 
  "Queue is an abstract base class for queues and cannot be instantiated.";

Enqueue::usage =
  "Enqueue[q, e] enqueues the element e to the queue q.";

Dequeue::usage =
  "Dequeue[q] returns the element at the top of the queue q.";

EmptyQ::usage =
  "EmptyQ[q] tests if queue q is empty.";

FifoQueue::usage = 
  "New[FifoQueue] constructs an empty first-in-first-out queue.";

LifoQueue::usage = StringJoin[
  "New[LifoQueue] constructs an empty last-in-first-out queue, ",
  "also known as stack."
];

PriorityQueue::usage = StringJoin[
  "New[PriorityQueue, p] constructs an empty priority queue ",
  "with p as the ordering predicate."
];

SearchIterator::usage = StringJoin[
  "it = New[SearchIterator, q, f] constructs an iterator over a sequence ",
  "which is defined by the initial contents of the Queue q ",
  "and the transition function f.\n",
  "HasNext[it] returns True as long as q is not empty.\n",
  "GetNext[it] returns the top element t of q ",
  "and enqueues all elements f[t]={a,b,c,...} to q."
];

BFSIterator::usage = StringJoin[
  "New[BFSIterator, s, f] constructs an iterator ",
  "for performing a breadth-first-search ",
  "starting with the element s. For any element e, ",
  "f[e] should return a (possibly empty) list of elements to be visited next."
];

DFSIterator::usage = StringJoin[
  "New[DFSIterator, s, f] constructs an iterator ",
  "for performing a depth-first-search ",
  "starting with the element s. For any element e, ",
  "f[e] should return a (possibly empty) list of elements to be visited next."
];

PFSIterator::usage = StringJoin[
  "New[PFSIterator, s, f, p] constructs an iterator ",
  "for performing a priority-first-search ",
  "starting with the element s and using the ordering predicate p. ",
  "For any element e, ",
  "f[e] should return a (possibly empty) list of elements to be visited next."
];

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
NewClass[SearchIterator];
Init[SearchIterator, obj_, queue_?(InstanceQ[Queue]), f_] ^:= (
  HasNext[obj] ^:= !EmptyQ[queue];
  GetNext[obj] ^:= Module[{next},
    next = Dequeue[queue];
    Do[Enqueue[queue, e], {e, f[next]}];
    next
  ];
);

NewClass[BFSIterator];
Init[BFSIterator, obj_, s_, f_] ^:= Module[{fifo},
  fifo = New[FifoQueue];
  If[Head[s] === List, Do[Enqueue[fifo, e], {e,s}], Enqueue[fifo,s]];
  Super[SearchIterator, obj, fifo, f];
];

NewClass[DFSIterator];
Init[DFSIterator, obj_, s_, f_] ^:= Module[{stack},
  stack = New[LifoQueue];
  If[Head[s] === List, Do[Enqueue[stack, e], {e,s}], Enqueue[stack,s]];
  Super[SearchIterator, obj, stack, f];
];

NewClass[PFSIterator];
Init[PFSIterator, obj_, s_, f_, p_] ^:= Module[{heap},
  heap = New[PriorityQueue, p];
  If[Head[s] === List, Do[Enqueue[heap, e], {e,s}], Enqueue[heap,s]];
  Super[SearchIterator, obj, heap, f];
];

End[];

Protect[Queue, Enqueue, Dequeue, EmptyQ];
Protect[FifoQueue, LifoQueue, PriorityQueue];
Protect[SearchIteartor, BFSIterator, DFSIterator, PFSIterator];

EndPackage[];
