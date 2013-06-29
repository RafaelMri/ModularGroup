(* ::Package:: *)

BeginPackage["ModularGroup`", {"OOP`", "Queue`"}];

(* ---------------------------------------------------------- Public Elements *)

MoebiusTransformation::usage = 
  "New[MoebiusTransformation, {{a, b}, {c, d}}] " <>
  "creates the moebius transformation " <>
  "z \[RightTeeArrow] \!\(\*FractionBox[\(\(a\[CenterDot]z\)\(\\\ \)\(+\)\(\\\ \)\(b\)\(\\\ \)\), \(c\[CenterDot]z\\\  + \\\ d\)]\).";
Mat::usage = 
  "Mat[t] returns the coefficients of the MoebiusTransformation t as matrix.";
Inv::usage = 
  "Inv[t] returns the inverse of the MoebiusTransformation t.";

ModularTransformation::usage = 
  "New[ModularTransformation, {{a, b}, {c, d}}] " <>
  "creates the modular transformation " <>
  "z \[RightTeeArrow] \!\(\*FractionBox[\(\(a\[CenterDot]z\)\(\\\ \)\(+\)\(\\\ \)\(b\)\(\\\ \)\), \(c\[CenterDot]z\\\  + \\\ d\)]\).";

TUExponents::usage = 
  "TUExponents[t] returns for a ModularTransformation t a list of exponents " <>
  "{\!\(\*SubscriptBox[\(e\), \(0\)]\),...,\!\(\*SubscriptBox[\(e\), \(n\)]\)} " <>
  "such that t = \!\(\*SuperscriptBox[\(U\), SubscriptBox[\(e\), \(0\)]]\)\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(1\)]]\)...\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(n\)]]\).";

QuotientFunction::usage = 
  "QuotientFunction is an option for TUExponents and defines the " <>
  "integer quotient function to use for the euclidean algorithm. " <>
  "Default is Mathematica's built-in function Quotient. " <>
  "Predefined alternatives are CentralQuotient and CQuotient.";

CentralQuotient::usage = 
  "CentralQuotient[m,n] returns the integer quotient q of m = q n + r, " <>
  "such that Abs[r] \[LessEqual] Abs[n/2].";

CQuotient::usage = 
  "CQuotient[m,n] returns the integer quotient of m and n " <>
  "with rounding towards 0 " <>
  "(equivalent to integer division e.g. in the programming language C).";

TUEval::usage = 
  "Evaluate the group word in T and U of a ModularTransformation P " <>
  "by supplying values for T and U and a product and power function. " <>
  "Example: TUEval[phi, Mat[mtT], Mat[mtU], Dot, MatrixPower]";

TUWord::usage = 
  "TUWord[t] returns a symbolic group word representation " <>
  "of the ModularTransformation t, in terms of the group generators T and U.";

(* Some frequently used ModularTransformations *)
mtId::usage = 
  "mtId is the identity element " <>
  "of the classes MoebiusTransformation and ModularTransformation.";

mtU::usage = "The ModularTransformation U: z \[RightTeeArrow] z+1.";
mtT::usage = "The ModularTransformation T: z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z\)]\).";
mtR::usage = "The ModularTransformation R = TU : z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z + 1\)]\).";

GeneralizedDisk::usage = 
  "New[GeneralizedDisk, {{a, b},{c, d}}] constructs the a generalized disk " <> 
  "whose points z satisfy a\[CenterDot]|z\!\(\*SuperscriptBox[\(|\), \(2\)]\) + b\[CenterDot]\!\(\*OverscriptBox[\(z\), \(_\)]\) + c\[CenterDot]z + d \[LessSlantEqual] 0. "
  "The matrix {{a, b}, {c, d}} must be Hermitian with negative determinant.";
NoncompactDisk::usage =
  "New[NoncompactDisk, c, r] constructs a GeneralizedDisk " <>
  "which contains the point \[Infinity] and whose complement in \[DoubleStruckCapitalC] is a disk " <>
  "with center c and radius r".
Halfplane::usage = 
  "New[Halfplane, z, n] constructs a GeneralizedDisk " <>
  "containing all points of a halfplane " <> 
  "which is given by a point z on its border " <>
  "and a complex number n pointing in normal direction to the border inside the halfplane.";
CompactDisk::usage =
  "New[CompactDisk, c, r] constructs a GeneralizedDisk " <>
  "with center c and radius r.";
InteriorQ::usage =
  "InteriorQ[d,z] tests if the GeneralizedDisk d contains the point z.";

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

TUExponents[obj_?(InstanceQ[ModularTransformation]), OptionsPattern[]] := 
  Module[{quotientFu, sgn, m, q},
    quotientFu = OptionValue[QuotientFunction];
    Reap[
      m = Mat[obj];
      sgn = 1;
      While[m[[2,1]] != 0,
        q = quotientFu[m[[1,1]], m[[2,1]]];
        m = {{0, 1}, {1, -q}}.m;
        Sow[sgn q];
        sgn = -sgn;
      ];
      Sow[m[[1,1]] m[[1,2]]];
    ][[2,1]]
  ];
Options[TUExponents] = {QuotientFunction->Quotient};
CentralQuotient[p_, q_] := Quotient[p, q, -q/2];
CQuotient[p_, q_] := If[p q > 0, Quotient[p, q], -Quotient[-p, q]];

TUEval[obj_?(InstanceQ[ModularTransformation]), t_, u_, product_, power_, opts : OptionsPattern[]] := (
  product@@Riffle[power[u,#]&/@TUExponents[obj,opts],"T "]/."T "->t
);

TUWord[obj_?(InstanceQ[ModularTransformation]), opts:OptionsPattern[]] := (
  TUEval[obj, "T", "U", Composition[Row,List], Superscript, opts]
);

(* -------------------------------------------------- Class GeneralizedDisk *)
NewClass[GeneralizedDisk];
NewClass[NoncompactDisk];
NewClass[Halfplane];
NewClass[CompactDisk];

Init[GeneralizedDisk, obj_, mat_?MatrixQ] ^:= Module[{a},
  Mat[obj] ^= mat;
  a = mat[[1,1]];
  obj /: InstanceQ[NoncompactDisk][obj] = (a < 0);
  obj /: InstanceQ[Halfplane][obj] = (a == 0);
  obj /: InstanceQ[CompactDisk][obj] = (a > 0); 
];

Init[NoncompactDisk, obj_, cocenter_, coradius_] ^:= Module[{mat},
  mat = {{-1, cocenter}, {Conjugate@cocenter, coradius^2 - cocenter*Conjugate@cocenter}};
  Super[GeneralizedDisk, obj, mat];
];

Init[CompactDisk, obj_, center_, radius_] ^:= Module[{mat},
  mat = {{1, -center}, {-Conjugate@center, center*Conjugate@center - radius^2}};
  Super[GeneralizedDisk, obj, mat];
];

Init[Halfplane, obj_, pt_, normal_] ^:= Module[{b, d, mat},
  b = -normal/2;
  d = Re[pt*Conjugate@normal];
  mat = {{0, b}, {Conjugate@b, d}};
  Super[GeneralizedDisk, obj, mat];
];

InteriorQ[disk_?(InstanceQ[GeneralizedDisk]), z_] := (
  Re[{Conjugate@z,1}.Mat[disk].{z,1}] <= 0
);

(* --------------------------------------------------- Enumeration algorithms *)

(*ListModularGroup[stopCriteria_] := (
);*)

(* ------------------------------------------- Special ModularTransformations *)
mtId = New[ModularTransformation, IdentityMatrix[2]];
Inv[mtId] ^= mtId; (* Identity is self-inverse *)

mtU = New[ModularTransformation, {{1,1},{0,1}}];

mtT = New[ModularTransformation, {{0,-1},{1,0}}];
Inv[mtT] ^= mtT; (* T is self-inverse *)

mtR = New[ModularTransformation, Mat[mtT].Mat[mtU]];

End[];

(*Protect[MoebiusTransformation, Mat, Inv];
Protect[ModularTransformation, QuotientFunction];
Protect[TUExponents, TUEval, TUWord];*)

EndPackage[];
