(* ::Package:: *)

BeginPackage["OOP`"];

Class::usage = "Symbol for classes for usage with InsanceQ. Example: InstanceQ[Class][myClass]";

NewClass::usage = "Creates a new class. Example: NewClass[myClass]";

New::usage = "Instantiate an object. Example: myObj = New[myClass, args...]";
New::notaclass = "`1` is not a class.";

Init::usage = "Initialize an object.";
Init::undef = "Constructor for class `1` and arguments `2` not defined. Make sure you have defined Init[`1`, obj_, args___] ^:= ...";

InstanceQ::usage = "Test if object is instance of class. Example: InstanceQ[myClass][myObj]";
Info::usage = "Display data associated with an object. Example: Info[myObj]";

Begin["`Private`"];

NewClass[class_] := (
  class /: InstanceQ[Class][class] = True;
  class /: Init[class, obj_, args___] := (
    Message[Init::undef, class, {args}];
    $Failed
  );
);
SetAttributes[NewClass, HoldFirst];

New[class_?(InstanceQ[Class]), args___] := Module[{obj},
  obj /: InstanceQ[class][obj] = True;
  Format[obj] ^= Unique[class];
  Init[class, obj, args];
  obj
];
New[notaclass_, args___] := (
  Message[New::notaclass, notaclass];
  $Failed
);

Info[obj_] := Information@Evaluate@obj;

End[];
EndPackage[];
