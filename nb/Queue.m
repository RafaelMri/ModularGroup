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
  "First-in-first-out queue. " <>
  "Example: myFifo = New[FifoQueue[t]] " <>
  "constructs a fifo queue for element type t.";

LifoQueue::usage =
  "Last-in-first-out queue (a.k.a. stack). " <>
  "Example: myStack = New[LifoQueue[t]] " <>
  "constructs a lifo queue for element type t.";

PriorityQueue::usage =
  "Priority queue (a.k.a heap). " <>
  "Example: myHeap = New[PriorityQueue[t, p]] " <>
  "constructs a priority queue for element type t " <>
  "with p as ordering function.";

Begin["`Private`"];

(* -------------------------------------------------------------- Class Queue *)
NewAbstractClass[Queue];

Queue[elemType_?(InstanceQ[Class])] := Queue[elemType] = 
Module[{classObj},
  NewAbstractClass[classObj];
  Init[classObj, obj_] ^:= (
    obj /: InstanceQ[Queue][obj] = True;
  );
  classObj
];

(* ---------------------------------------------------------- Class FifoQueue *)
NewClass[FifoQueue];

FifoQueue[elemType_?(InstanceQ[Class])] := FifoQueue[elemType] = 
Module[{classObj},
  NewClass[classObj];
  
  Init[classObj, obj_] ^:= (
    Super[Queue[elemType], obj];
    obj /: InstanceQ[FifoQueue][obj] = True;
    Contents@obj ^= {};
  );

  Enqueue[queue_?(InstanceQ[classObj]), elem_?(InstanceQ[elemType])] := (
    Contents@queue ^= Append[Contents@queue, elem];
  );

  classObj
];

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

LifoQueue[elemType_?(InstanceQ[Class])] := LifoQueue[elemType] = Module[{classObj},
  NewClass[classObj];
  
  Init[classObj, obj_] ^:= (
    Super[Queue[elemType], obj];
    obj /: InstanceQ[LifoQueue][obj] = True;
    Contents@obj ^= {};
  );

  Enqueue[queue_?(InstanceQ[classObj]), elem_?(InstanceQ[elemType])] := (
    Contents@queue ^= {elem, Contents@queue};
  );

  classObj
];

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

PriorityQueue[elemType_?(InstanceQ[Class])] := PriorityQueue[elemType] = 
Module[{classObj},
  NewClass[classObj];
    
  Init[classObj, obj_, orderingFunction_] ^:= (
    Super[Queue[elemType], obj];
    obj /: InstanceQ[PriorityQueue][obj] = True;
    OrderingFunction[obj] ^= orderingFunction;
    Contents[obj] ^= Table[Null, {i,16}];
    Size[obj] ^= 0;
  );

  Enqueue[queue_?(InstanceQ[classObj]), elem_?(InstanceQ[elemType])] := 
  Module[{contents},
    contents = Grow[queue];
    Size@queue ^= Size@queue + 1;
    contents[[Size@queue]] = elem;
    Contents@queue ^= contents;
    Contents@queue ^= HeapUp[queue];
  ];

  classObj
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
    c = SmallerChild[queue, i];
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

End[];

EndPackage[];
