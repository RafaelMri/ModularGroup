(* ::Package:: *)

BeginPackage["OOP`"];

(* ---------------------------------------------------------- Public Elements *)

Class::usage = 
  "The type of a class. " <>
  "After calling NewClass[myClass], InstanceQ[Class][myClass] will return True.";

NewClass::usage = 
  "NewClass[myClass] declares the symbol myClass to be a class.";
NewAbstractClass::usage =
  "NewAbstractClass[myClass] declares the symbol myClass to be an abstract class.";

AbstractQ::usage =
  "AbstractQ[myClass] returns true if myClass is an abstract class.";

New::usage = 
  "New[myClass, args...] instantiates an objtect of type myClass, " <>
  "passing the given arguments to the constructor.";
New::notaclass = "`1` is not a class.";
New::abstractinst = "`1` is an abstract class and cannot be instantiated.";

Init::usage = 
  "Init[myClass, newObj, args...] initializes an object of type myClass." <>
  "Never call directly, use newObj = New[myClass, args...] instead.";
Init::undef = 
  "Constructor for class `1` and arguments `2` not defined. " <> 
  "Make sure you have defined Init[`1`, obj_, args___] ^:= ...";

Super::usage = 
  "Super[myBaseClass, newObj, args...] calls the constructor of  " <>
  "Example: Super[myBaseClass, obj, args...]";

InstanceQ::usage = 
  "Test if object is instance of class. Example: InstanceQ[myClass][myObj]";

Info::usage = 
  "Display data associated with an object. Example: Info[myObj]";

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

End[];

Protect[Class, NewClass, New, Init, Super, InstanceQ, Info];

EndPackage[];
