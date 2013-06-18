(* ::Package:: *)

BeginPackage["ModularGroup`", {"OOP`"}];

(* ---------------------------------------------------------- Public Elements *)

MoebiusTransformation::usage = "Class for moebius transformations";
Mat::usage = "The matrix representation of a MoebiusTransformation";
Inv::usage = "Returns the inverse of a MoebiusTransformation";

ModularTransformation::usage = "Class for modular transformations, inherits from MoebiusTransformation";
TUExponents::usage = "For a ModularTransformation P, return a list {\!\(\*SubscriptBox[\(e\), \(0\)]\),...,\!\(\*SubscriptBox[\(e\), \(n\)]\)} such that P = \!\(\*SuperscriptBox[\(U\), SubscriptBox[\(e\), \(0\)]]\)\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(1\)]]\)...\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(n\)]]\)";
TUEval::usage = "Evaluate the group word in T and U of a ModularTransformation P by supplying values for T and U and a product and power function. Example: TUEval[phi, Mat[mtT], Mat[mtU], Dot, MatrixPower]";
TUWord::usage = "For a ModularTransformation P, return a group word representation in terms of the generators T and U";

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
  obj /: InstanceQ[ModularTransformation][obj] := (
     obj /: InstanceQ[ModularTransformation][obj] = MatrixQ[mat,IntegerQ] && Det[mat]==1
  );
);

Inv[obj_?(InstanceQ[MoebiusTransformation])] := Inv[obj] ^= 
  Module[{mat, invMat, inverse},
    mat = Mat[obj];
    invMat = {{mat[[2,2]], -mat[[1,2]]}, {-mat[[2,1]], mat[[1,1]]}};
    inverse = New[MoebiusTransformation, invMat];
    Inv[inverse] ^= obj;
    inverse
  ];

(* ---------------------------------------------- Class ModularTransformation *)

NewClass[ModularTransformation];

Init[ModularTransformation, obj_, mat_?(MatrixQ[#,IntegerQ]&)] ^:= (
  Super[MoebiusTransformation, obj, mat];
);

TUExponents[obj_?(InstanceQ[ModularTransformation])] := 
  Module[{quotients, m, q},
    quotients = Reap[
      m = Mat[obj];
      While[m[[2,1]] != 0,
       q = Quotient[m[[1,1]], m[[2,1]]];
       m = {{0, 1}, {-1, q}}.m;
       Sow[q];
     ];
     Sow[m[[1,1]] m[[1,2]]];
    ][[2,1]];
    TUExponents[obj] ^= quotients;
    quotients
  ];

TUEval[obj_?(InstanceQ[ModularTransformation]), t_, u_, product_, power_] :=
  product@@Riffle[power[u,#]&/@ExponentList[obj],"T"]/."T"->t;
TUWord[obj_?(InstanceQ[ModularTransformation])] :=
  TUEval[obj, "T", "U", Composition[Row,List], Superscript];

(* ------------------------------------------- Special ModularTransformations *)
mtId = New[ModularTransformation, IdentityMatrix[2]];
Inv[mtId] ^= mtId; (* Identity is self-inverse *)

mtU = New[ModularTransformation, {{1,1},{0,1}}];

mtT = New[ModularTransformation, {{0,-1},{1,0}}];
Inv[mtT] ^= mtT; (* T is self-inverse *)

mtR = New[ModularTransformation, Mat[mtT].Mat[mtU]];

End[];

EndPackage[];
