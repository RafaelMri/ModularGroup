(* ::Package:: *)

BeginPackage["OOP`"];

Unprotect[Class, NewClass, New, Init, Super, InstanceQ, Info];
Unprotect[Iterator, ListIterator, HasNext, GetNext];

(* ---------------------------------------------------------- Public Elements *)

Class::usage = 
  "Class is a reserved symbol identifying the type of a class.";
NewClass::usage = 
  "NewClass[c] declares the symbol c to be a class.";
NewAbstractClass::usage =
  "NewAbstractClass[c] declares the symbol c to be an abstract class.";

AbstractQ::usage =
  "AbstractQ[c] returns True if c is an abstract class.";

New::usage = StringJoin[
  "New[c, args...] instantiates an objtect of class c, ",
  "passing the given arguments to its constructor."
];
New::notaclass = "`1` is not a class.";
New::abstractinst = "`1` is an abstract class and cannot be instantiated.";

Init::usage = StringJoin[
  "Init[c, o, args...] defines the constructor of a class c ",
  "and initializes the new object o. ",
  "Never call directly, use o = New[c, args...] instead."
];
Init::undef = StringJoin[
  "Constructor for class `1` and arguments `2` not defined. ",
  "Make sure you have defined Init[`1`, obj_, args___] ^:= ..."
];

Super::usage = StringJoin[
  "Super[b, o, args...] calls the constructor of a base-class b ",
  "for a new object o. ",
  "To be used within the Init function of a class c which inherits from b."
];

InstanceQ::usage = StringJoin[
  "InstanceQ[c][o] returns True, if the object o is instance of class c."
];

Info::usage = StringJoin[
  "Info[o] displays data associated with the object o."
];

Iterator::usage = StringJoin[
  "Iterator is an abstract base class for objects o providing methods ",
  "HasNext[o] and GetNext[o]."
];

HasNext::usage =
  "HasNext[o] tests if the Iterator o has a next element.";

GetNext::usage =
  "GetNext[o] returns the next value of the Iterator o.";

ListIterator::usage =
  "New[ListIterator, l] creates an iterator for the objects in the list l.";

Begin["`Private`"];

NewClass[class_] := (
  class /: InstanceQ[Class][class] = True;
  class /: Init[class, obj_, args___] := (
    Message[Init::undef, class, {args}];
    $Failed
  );
);
NewAbstractClass[class_] := (
  NewClass[class];
  AbstractQ[class] ^= True;
);

New[class_?(InstanceQ[Class]), args___] := (
  If[TrueQ@AbstractQ@class, (
    Message[New::abstractclass, class];
    $Failed
  ), (
    Module[{obj},
      Format[obj] ^= Unique[class];
      Super[class, obj, args];
      obj
    ]
  )]
);

New[notaclass_, args___] := (
  Message[New::notaclass, notaclass];
  $Failed
);

Super[class_?(InstanceQ[Class]), obj_, args___] := (
  Init[class, obj, args];
  obj /: InstanceQ[class][obj] = True;
);

Info[obj_] := Information@Evaluate@obj;

NewAbstractClass[Iterator];
Init[Iterator, obj_] ^:= Null;

NewClass[ListIterator];
Init[ListIterator, obj_, list_] ^:= (
  Super[Iterator, obj];
  Pos[obj] ^= 1;
  HasNext[obj] ^:= Pos[obj] <= Length[list];
  GetNext[obj] ^:= Module[{next}, 
    next = list[[Pos[obj]]]; 
    Pos[obj] ^= Pos[obj]+1;
    next
  ];
);

End[];

Protect[Class, NewClass, New, Init, Super, InstanceQ, Info];
Protect[Iterator, ListIterator, HasNext, GetNext];

EndPackage[];
