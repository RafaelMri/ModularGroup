(* ::Package:: *)

BeginPackage["ModularGroup`", {"OOP`"}];

(* ---------------------------------------------------------- Public Elements *)

MoebiusTransformation::usage = "Class for moebius transformations";
Mat::usage = "The matrix representation of a MoebiusTransformation";
Inv::usage = "Returns the inverse of a MoebiusTransformation";

ModularTransformation::usage = "Class for modular transformations, extends MoebiusTransformation";

(* Some frequently used ModularTransformations *)
mtId::usage = "The identity element of classes MoebiusTransformation and ModularTransformation";
mtU::usage = "The ModularTransformation U: z \[RightTeeArrow] z+1";
mtT::usage = "The ModularTransformation T: z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z\)]\)";
mtR::usage = "The ModularTransformation R = TU : z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z + 1\)]\)";

Begin["`Private`"];

(* ---------------------------------------------- Class MoebiusTransformation *)

NewClass[MoebiusTransformation];

Init[MoebiusTransformation, obj_, mat_?MatrixQ] ^:= (
  Mat[obj] ^= mat;
  Inv[obj] ^:= Inv[obj] ^= Module[{invMat, inverse},
    invMat = {{mat[[2,2]], -mat[[1,2]]}, {-mat[[2,1]], mat[[1,1]]}};
    inverse = New[MoebiusTransformation, invMat];
    Inv[inverse] ^= obj;
    inverse
  ];
  obj /: InstanceQ[ModularTransformation][obj] := (
     obj /: InstanceQ[ModularTransformation][obj] = MatrixQ[mat,IntegerQ] && Det[mat]==1
  );
);

NewClass[ModularTransformation];

Init[ModularTransformation, obj_, mat_?(MatrixQ[#,IntegerQ]&)] ^:= (
  Init[MoebiusTransformation, obj, mat];
);

(* ------------------------------------------- Special ModularTransformations *)
mtId = New[ModularTransformation, IdentityMatrix[2]];
Inv[mtId] ^= mtId; (* Identity is self-inverse *)

mtU = New[ModularTransformation, {{1,1},{0,1}}];

mtT = New[ModularTransformation, {{0,-1},{1,0}}];
Inv[mtT] ^= mtT; (* T is self-inverse *)

mtR = New[ModularTransformation, Mat[mtT].Mat[mtU]];

End[];

EndPackage[];
