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

Inhom::usage = 
  "Inhom[{z1,z2}] returns z1 / z2 or \[Infinity], if z2 == 0.";

ModularTransformation::usage = 
  "New[ModularTransformation, {{a, b}, {c, d}}] " <>
  "creates the modular transformation " <>
  "z \[RightTeeArrow] \!\(\*FractionBox[\(\(a\[CenterDot]z\)\(\\\ \)\(+\)\(\\\ \)\(b\)\(\\\ \)\), \(c\[CenterDot]z\\\  + \\\ d\)]\).";

TUExponents::usage = 
  "TUExponents[t] returns for a ModularTransformation t a list of exponents " <>
  "{\!\(\*SubscriptBox[\(e\), \(0\)]\),...,\!\(\*SubscriptBox[\(e\), \(n\)]\)} " <>
  "such that t = \!\(\*SuperscriptBox[\(U\), SubscriptBox[\(e\), \(0\)]]\)\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(1\)]]\)...\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(n\)]]\). \n" <>
  "Options: QuotientFunction.";
 
QuotientFunction::usage = 
  "QuotientFunction is an option which defines the " <>
  "integer quotient function to be used within the euclidean algorithm. " <>
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
  "TUEval[t, subsT, subsU, product, power] " <>
  "substitutes the symbols T and U " <>
  "in the group word representation of the ModularTransformation t " <>
  "with subsT and subsU respectively " <>
  "and evaluates using the provided product and power functions.\n" <>
  "Example: TUEval[phi, Mat[mtT], Mat[mtU], Dot, MatrixPower]\n" <>
  "Options: QuotientFunction.";

TUWord::usage = 
  "TUWord[t] returns a symbolic group word representation " <>
  "of the ModularTransformation t in terms of the group generators T and U.\n" <>
  "Options: QuotientFunction";

(* Some frequently used ModularTransformations *)
mtId::usage = 
  "mtId is the identity element " <>
  "of the classes MoebiusTransformation and ModularTransformation.";

mtU::usage = "mtU is the ModularTransformation U: z \[RightTeeArrow] z+1.";
mtT::usage = "mtT is the ModularTransformation T: z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z\)]\).";
mtR::usage = "mtR is the ModularTransformation R = TU : z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z + 1\)]\).";

GeneralizedDisk::usage = 
  "New[GeneralizedDisk, {{a, b},{c, d}}] constructs the a generalized disk " <> 
  "whose points z satisfy a\[CenterDot]|z\!\(\*SuperscriptBox[\(|\), \(2\)]\) + b\[CenterDot]\!\(\*OverscriptBox[\(z\), \(_\)]\) + c\[CenterDot]z + d \[LessSlantEqual] 0. " <>
  "The matrix {{a, b}, {c, d}} must be Hermitian with negative determinant.";
NoncompactDisk::usage =
  "New[NoncompactDisk, c, r] constructs a GeneralizedDisk " <>
  "which contains the point \[Infinity] and whose complement in \[DoubleStruckCapitalC] is a disk " <>
  "with center c and radius r.";
Halfplane::usage = 
  "New[Halfplane, z, n] constructs a GeneralizedDisk " <>
  "containing all points of a halfplane " <> 
  "which is given by a point z on its border " <>
  "and a complex number n pointing inside the halfplane " <> 
  "in normal direction to its border.";
CompactDisk::usage =
  "New[CompactDisk, c, r] constructs a GeneralizedDisk " <>
  "with center c and radius r.";
InteriorQ::usage =
  "InteriorQ[d, z] tests if the GeneralizedDisk d contains the point z.";
GDiskTransform::usage =
  "GDiskTransform[d, t] transforms the GeneralizedDisk d " <>
  "by applying the moebius transformation t, " <>
  "which may be given either as MoebiusTransformation object " <>
  "or directly as matrix.";

ModularGroupExcerpt::usage =
  "ModularGroupExcerpt[p] " <> 
  "performs a depth-first-search and returns ModularTransformations " <>
  "satisfying the predicate p.\n" <>
  "Options: StartTransformation, MaxIterations";
ModularGroupExcerpt::maxit =
  "Maximum number of iterations exceeded. Current setting: MaxIterations \[RightArrow] `1`.";

ModularGroupIterator::usage = 
  "ModularGroupIterator[p] " <>
  "Returns an Enumerator for modular transformations " <>
  "using p as ordering prdicate for the internal PriorityQueue.";

Begin["`Private`"];

(* ---------------------------------------------- Class MoebiusTransformation *)

NewClass[MoebiusTransformation];

Inhom[{z1_,z2_}] := (
  If[TrueQ[z2 == 0], 
    ComplexInfinity, 
    z1 / z2
  ]
);

Init[MoebiusTransformation, obj_, mat_?MatrixQ] ^:= (
  Mat[obj] ^= mat;
  obj /: InstanceQ[ModularTransformation][obj] := (
  obj /: InstanceQ[ModularTransformation][obj] = 
    MatrixQ[mat, IntegerQ] && Det[mat] == 1
  );
  obj@t_?(InstanceQ[MoebiusTransformation]) := 
    New[MoebiusTransformation, mat.Mat[t]];
  obj@m_?MatrixQ := mat.m;
  m_?MatrixQ@obj ^:= m.mat;
  obj@h_?VectorQ := mat.h;
  obj@z_ := If[TrueQ[z === ComplexInfinity], Inhom[mat.{1,0}], Inhom[mat.{z, 1}]];
);

Inv[m_?MatrixQ] := {{m[[2,2]], -m[[1,2]]}, {-m[[2,1]], m[[1,1]]}};

Inv[t_?(InstanceQ[MoebiusTransformation])] := Inv[t] ^= 
  Module[{inverse},
    inverse = New[MoebiusTransformation, Inv[Mat[t]]];
    Inv[inverse] ^= t;
    inverse
  ];

(* ---------------------------------------------- Class ModularTransformation *)
NewClass[ModularTransformation];

Init[ModularTransformation, obj_, mat_?(MatrixQ[#,IntegerQ]&)] ^:= (
  Assert[Det[mat] == 1];
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
  Row@DeleteCases[
    TUEval[obj, "T", "U", List, Superscript, opts],
    Superscript["U",0]
  ]
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

GDiskTransform[disk_?(InstanceQ[GeneralizedDisk]), t_?(InstanceQ[MoebiusTransformation])] := 
Module[{transformed},
  transformed = (ConjugateTranspose@Mat@Inv@t).(Mat@disk).(Mat@Inv@t);
  New[GeneralizedDisk, transformed]
];

GDiskTransform[disk_?(InstanceQ[GeneralizedDisk]), t_?MatrixQ] := 
Module[{tInv},
  tInv = Inverse@t;
  New[GeneralizedDisk, ConjugateTranspose[tInv].Mat[disk].tInv]
];

(* --------------------------------------------------- Enumeration algorithms *)

Transition[ModularTransformation] = Function[top, 
Module[{current, steps},
  {current, steps} = top;
  Table[{
    New[ModularTransformation, Mat[current].Mat[step]],
    If[step === mtT, Inv@#&/@Rest[steps], {mtT, step}]
    }, {step, steps}]
]];

ModularGroupExcerpt[criteria_, OptionsPattern[]] := Reap[
Module[{i, max, mtStart, queue, top, next},
  i = 0;
  max = OptionValue[MaxIterations];
  mtStart = OptionValue[StartTransformation];
  queue = New[LifoQueue];
  If[criteria[mtStart], Enqueue[queue, {mtStart, {mtT, mtU, Inv@mtU}}]];
  While[i++ < max && !EmptyQ[queue],
    top = Dequeue[queue];
    Sow[top[[1]]];
    next = Transition[ModularTransformation][top]; 
    Do[Enqueue[queue, n], {n, Select[next, criteria[#[[1]]]&]}];
  ];
  If[i >= max, Message[Excerpt::maxit, max]];
]][[2,1]];
Options[ModularGroupExcerpt] = {MaxIterations -> 2^12, StartTransformation->mtId};

ModularGroupIterator[ordering_, OptionsPattern[]] := 
Module[{mtStart},
  mtStart = OptionValue[StartTransformation];
  New[PFSIterator, {mtStart, {mtT, mtU, Inv@mtU}}, 
    Transition[ModularTransformation], ordering]
];
Options[ModularGroupEnumerator] = {StartTransformation -> mtId};

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
