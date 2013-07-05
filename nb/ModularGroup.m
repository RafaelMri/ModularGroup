(* ::Package:: *)

BeginPackage["ModularGroup`", {"OOP`", "Queue`"}];

(* ---------------------------------------------------------- Public Elements *)

MoebiusTransformation::usage = StringJoin[
  "New[MoebiusTransformation, {{a, b}, {c, d}}] ",
  "creates the moebius transformation ",
  "z \[RightTeeArrow] \!\(\*FractionBox[\(\(a\[CenterDot]z\)\(\\\ \)\(+\)\(\\\ \)\(b\)\(\\\ \)\), \(c\[CenterDot]z\\\  + \\\ d\)]\)."
];
Mat::usage = 
  "Mat[t] returns the coefficients of the MoebiusTransformation t as matrix.";
Inv::usage = 
  "Inv[t] returns the inverse of the MoebiusTransformation t.";

Inhom::usage = 
  "Inhom[{z1,z2}] returns z1 / z2 or \[Infinity], if z2 == 0.";

ModularTransformation::usage = StringJoin[
  "New[ModularTransformation, {{a, b}, {c, d}}] ",
  "creates the modular transformation ",
  "z \[RightTeeArrow] \!\(\*FractionBox[\(\(a\[CenterDot]z\)\(\\\ \)\(+\)\(\\\ \)\(b\)\(\\\ \)\), \(c\[CenterDot]z\\\  + \\\ d\)]\)."
];

TUExponents::usage = StringJoin[
  "TUExponents[t] returns for a ModularTransformation t a list of exponents ",
  "{\!\(\*SubscriptBox[\(e\), \(0\)]\),...,\!\(\*SubscriptBox[\(e\), \(n\)]\)} ",
  "such that t = \!\(\*SuperscriptBox[\(U\), SubscriptBox[\(e\), \(0\)]]\)\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(1\)]]\)...\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(n\)]]\).",
  "\nOptions: QuotientFunction."
];
 
QuotientFunction::usage = StringJoin[
  "QuotientFunction is an option which defines the ",
  "integer quotient function to be used within the Euclidean algorithm. ",
  "Default is Mathematica's built-in function Quotient. ",
  "Predefined alternatives are CentralQuotient and CQuotient."
];

CentralQuotient::usage = StringJoin[
  "CentralQuotient[m,n] returns the integer quotient q of m = q n + r, ",
  "such that Abs[r] \[LessEqual] Abs[n/2]."
];

CQuotient::usage = StringJoin[
  "CQuotient[m,n] returns the integer quotient of m and n ",
  "with rounding towards 0 ",
  "(equivalent to integer division e.g. in the programming language C)."
];

TUEval::usage = StringJoin[
  "TUEval[t, subsT, subsU, product, power] ",
  "substitutes the symbols T and U ",
  "in the group word representation of the ModularTransformation t ",
  "with subsT and subsU respectively ",
  "and evaluates using the provided product and power functions.",
  "\nExample: TUEval[phi, Mat[mtT], Mat[mtU], Dot, MatrixPower]",
  "\nThe options QuotientFunction is supported."
];

TUWord::usage = StringJoin[
  "TUWord[t] returns a symbolic group word representation ",
  "of the ModularTransformation t in terms of the group generators T and U.",
  "\nOptions: QuotientFunction"
];

(* Some frequently used ModularTransformations *)
mtId::usage = StringJoin[
  "mtId is the identity element ",
  "of the classes MoebiusTransformation and ModularTransformation."
];

mtU::usage = "mtU is the ModularTransformation U: z \[RightTeeArrow] z+1.";
mtT::usage = "mtT is the ModularTransformation T: z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z\)]\).";
mtR::usage = "mtR is the ModularTransformation R = TU : z \[RightTeeArrow] -\!\(\*FractionBox[\(1\), \(z + 1\)]\).";

GeneralizedDisk::usage = StringJoin[
  "New[GeneralizedDisk, {{a, b},{c, d}}] constructs the a generalized disk ", 
  "whose points z satisfy a\[CenterDot]|z\!\(\*SuperscriptBox[\(|\), \(2\)]\) + b\[CenterDot]\!\(\*OverscriptBox[\(z\), \(_\)]\) + c\[CenterDot]z + d \[LessSlantEqual] 0. ",
  "The matrix {{a, b}, {c, d}} must be Hermitian with negative determinant."
];
NoncompactDisk::usage = StringJoin[
  "New[NoncompactDisk, c, r] constructs a GeneralizedDisk ",
  "which contains the point \[Infinity] and whose complement in \[DoubleStruckCapitalC] is a disk ",
  "with center c and radius r."
];
Halfplane::usage = StringJoin[
  "New[Halfplane, z, n] constructs a GeneralizedDisk ",
  "containing all points of a halfplane ",
  "which is given by a point z on its border ",
  "and a complex number n pointing inside the halfplane ",
  "in normal direction to its border."
];
CompactDisk::usage = StringJoin[
  "New[CompactDisk, c, r] constructs a GeneralizedDisk ",
  "with center c and radius r."
];
InteriorQ::usage = StringJoin[
  "InteriorQ[d, z] tests if the GeneralizedDisk d contains the point z, ",
  "which also may be given in homogeneous form."
];

(* Some predefined GeneralizedDisks *)
gdUnitDisk::usage =
  "gdUnitDisk is the set of points z satisfying |z| \[LessEqual] 1.";
gdUpperHalfplane::usage =
  "gdUpperHalfPlane is the set of points z satisfying Im[z] \[GreaterSlantEqual] 0.";
gdLowerHalfplane::usage =
  "gdLowerHalfPlane is the set of points z satisfying Im[z] \[LessSlantEqual] 0.";
gdRightHalfplane::usage =
  "gdRightHalfPlane is the set of points z satisfying Re[z] \[GreaterSlantEqual] 0.";
gdLeftHalfplane::usage =
  "gdLeftHalfPlane is the set of points z satisfying Re[z] \[LessSlantEqual] 0.";

GDiskRadius::usage = StringJoin[
  "GDiskRadius[d] returns the radius of the GeneralizedDisk d. ", 
  "It is defined for CompactDisks only."
];
GDiskCenter::usage = StringJoin[
  "GDiskCenter[d] returns the center of a GeneralizdeDisk d. ",
  "It is defined for CompactDisks only."
];
GDiskCoradius::usage = StringJoin[
  "GDiskCoradius[d] returns the co-radius of a GeneralizedDisk d. ",
  "It is defined for NoncompactDisks only."
];
GDiskCocenter::usage = StringJoin[
  "GDiskCocenter[d] returns the co-center of a GeneralizedDisk d. ",
  "It is defined for NoncompactDisks only."
];

GDisk::usage = StringJoin[
  "GDisk[d] gives a two-dimensional graphics object ",
  "representing the GeneralizedDisk d.",
  "\nGDisk[d, {\!\(\*SubscriptBox[\(t\), \(1\)]\),\!\(\*SubscriptBox[\(t\), \(2\)]\),...}] ",
  "draws all Moebius transformed GeneralizedDisks \!\(\*SubscriptBox[\(t\), \(1\)]\)[d], \!\(\*SubscriptBox[\(t\), \(2\)]\)[d], ...",
  "\nThe options GDiskClipRadius and GDiskNPoints are supported."
];
GCircle::usage = StringJoin[
  "GCircle[d] gives a two-dimensional grphics object ",
  "representing the boundary of the GeneralizedDisk d.",
  "\nGCircle[d, {\!\(\*SubscriptBox[\(t\), \(1\)]\),\!\(\*SubscriptBox[\(t\), \(2\)]\),...}] ",
  "draws all boundaries of the Moebius transformed GeneralizedDisks \!\(\*SubscriptBox[\(t\), \(1\)]\)[d], \!\(\*SubscriptBox[\(t\), \(2\)]\)[d], ...",
  "\nThe option GDiskClipRadius is supported."
];

GDiskClipRadius::usage = StringJoin[
  "GDiskClipRadius is an option of GDisk. ",
  "Set it to a value, which is greater or equal ",
  "to the maximum 2-norm of all points within the plot range."
];
GDiskNPoints::usage = StringJoin[
  "GDiskNPoints is an option of GDisk. It specifies, how many points are used ",
  "in the approximation of the boundary of a NoncompactDisk with a regular n-gon."
];

ModularGroupExcerpt::usage = StringJoin[
  "ModularGroupExcerpt[p] ",
  "performs a depth-first-search and returns ModularTransformations ",
  "satisfying the predicate p.",
  "\nThe options StartTransformation, MaxIterations are supported."
];
ModularGroupExcerpt::maxit =
  "Maximum number of iterations exceeded. Current setting: MaxIterations \[RightArrow] `1`.";

ModularGroupIterator::usage = StringJoin[
  "ModularGroupIterator[p] ",
  "Returns an Enumerator for modular transformations ",
  "using p as ordering prdicate for the internal PriorityQueue."
];

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
  obj@h_?VectorQ := mat.h;
  obj@z_ := (
    If[TrueQ[z === ComplexInfinity], 
      Inhom[mat.{1,0}], 
      Inhom[mat.{z, 1}]
    ]
  );
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
  Assert[HermitianMatrixQ@mat && Det[mat] < 0];
  Mat[obj] ^= mat;
  a = mat[[1,1]];
  obj /: InstanceQ[NoncompactDisk][obj] = (a < 0);
  obj /: InstanceQ[Halfplane][obj] = (a == 0);
  obj /: InstanceQ[CompactDisk][obj] = (a > 0); 
  
  t_?(InstanceQ[MoebiusTransformation])[obj] ^:=
  Module[{transformed},
    transformed = (ConjugateTranspose@Mat@Inv@t).mat.(Mat@Inv@t);
    New[GeneralizedDisk, transformed]
  ];
];

Init[NoncompactDisk, obj_, cocenter_, coradius_] ^:= Module[{mat},
  mat = {{-1, cocenter}, {Conjugate@cocenter, coradius^2 - cocenter*Conjugate@cocenter}};
  Super[GeneralizedDisk, obj, mat];
  GDiskCocenter[obj] ^= cocenter;
  GDiskCoradius[obj] ^= coradius;
];

Init[CompactDisk, obj_, center_, radius_] ^:= Module[{mat},
  mat = {{1, -center}, {-Conjugate@center, center*Conjugate@center - radius^2}};
  Super[GeneralizedDisk, obj, mat];
  GDiskCenter[obj] ^= center;
  GDiskRadius[obj] ^= radius;
];

Init[Halfplane, obj_, pt_, normal_] ^:= Module[{b, d, mat},
  b = -normal/2;
  d = Re[pt*Conjugate@normal];
  mat = {{0, b}, {Conjugate@b, d}};
  Super[GeneralizedDisk, obj, mat];
];

InteriorQ[disk_?(InstanceQ[GeneralizedDisk]), z_?VectorQ] := (
  Re[Conjugate[z].Mat[disk].z] <= 0
)

InteriorQ[disk_?(InstanceQ[GeneralizedDisk]), z_] := (
  If[TrueQ[z === ComplexInfinity], 
    !InstanceQ[CompactDisk][disk], 
    InteriorQ[disk,{z,1}]
  ]
);

gdUnitDisk = New[CompactDisk, 0, 1];
gdUpperHalfplane = New[Halfplane, 0, I];
gdLowerHalfplane = New[Halfplane, 0, -I];
gdRightHalfplane = New[Halfplane, 0, 1];
gdLeftHalfplane = New[Halfplane, 0, -1];
gdUnitCodisk = New[NoncompactDisk, 0, 1];

radius = Compile[{{m,_Complex,2}},
  Module[{a,b,d,aSqr},
   a = Re@m[[1,1]];
   b = m[[2,1]];
   d = Re@m[[2,2]];
   aSqr = a^2;   
   Sqrt[(Re[b]^2 + Im[b]^2 - a d) / aSqr]
  ]
];
center[m_] := -m[[1,2]] / m[[1,1]];
class = With[{maxRadiusSqr=N[2^30]},
  Compile[{{m, _Complex, 2}},
    Module[{a,b,d,aSqr},
      a = Re@m[[1,1]];
      b = m[[2,1]];
      d = Re@m[[2,2]];
      aSqr = a^2;
      If[aSqr == 0. || (Re[b]^2 + Im[b]^2 - a d) / aSqr > maxRadiusSqr,
        0,
        Sign@a
      ]
    ]
  ]
];

GDiskRadius[disk_?(InstanceQ[CompactDisk])] := 
GDiskRadius[disk] ^= (
  radius[Mat[disk]]
);

GDiskCenter[disk_?(InstanceQ[CompactDisk])] :=
GDiskCenter[disk] ^= (
  center[Mat[disk]]
);

GDiskCoradius[disk_?(InstanceQ[NoncompactDisk])] :=
GDiskCoradius[disk] ^= (
  radius[Mat[disk]]
);

GDiskCocenter[disk_?(InstanceQ[NoncompactDisk])] :=
GDiskCocenter[disk] ^= (
  center[Mat[disk]]
);

Options[GDisk] ^= {GDiskClipRadius->1024, GDiskNPoints->64};
Options[GCircle] ^= {GDiskClipRadius->1024};

GDisk[disk_?(InstanceQ[GeneralizedDisk]), opts:OptionsPattern[]] := (
  GDisk[Mat@disk, opts]
);

GDisk[m_?MatrixQ, opts:OptionsPattern[]] :=
Switch[class[m],
  1, (* CompactDisk *)
  Module[{c = center[m]},
    Disk[{Re[c],Im[c]}, radius[m]]
  ],
  -1, (* NoncompactDisk *)
  Module[{points, clip, c, r, n},
    clip = OptionValue[GDiskClipRadius];
    n = OptionValue[GDiskNPoints];
    c = center[m];
    r = radius[m];
    points = Join[
      Table[c+r Exp[2 Pi I t], {t, 1/2, -1/2, -1/n}],
        (-I-1) clip {1,I,-1,-I,1}
    ];
    Polygon[{Re@#,Im@#}&/@points]
  ],
  _, (* Halfplane *) 
  Module[{absb, b0, rot, clip},
    absb = Abs[m[[1,2]]];
    b0 = m[[1,2]] / absb;
    rot = {{Re@b0, -Im@b0}, {Im@b0, Re@b0}};
    clip = OptionValue[GDiskClipRadius];
    GeometricTransformation[
      Rectangle[{-clip, -clip}, {-m[[2,2]]/(2 absb), clip}],
      rot
    ]
  ]
];

udTransforms[mlist_] := Module[{c,r},
  Table[
    c = center[m];
    r = radius[m];
    {{{r,0},{0,r}}, {Re@c, Im@c}},
  {m, mlist}]
]

GDisk[disk_?(InstanceQ[GeneralizedDisk]), tlist_List, opts:OptionsPattern[]] :=
Module[{dm, mlists},
  dm = Mat@disk;
  mlists = SplitBy[ConjugateTranspose[Mat@Inv@#].dm.Mat[Inv@#]& /@ tlist, class];
  Table[
    If[class[mlist[[1]]] == 1,
      GeometricTransformation[Disk[], udTransforms[mlist]],
      Table[GDisk[m, opts], {m, mlist}]
    ],
  {mlist, mlists}]
];

GCircle[disk_?(InstanceQ[GeneralizedDisk]), OptionsPattern[]] :=
Module[{m},
  m=Mat@disk;
  Switch[class[m],
    1, (* CompactDisk *)
    Module[{c = GDiskCenter[disk]},
      Circle[{Re[c],Im[c]}, GDiskRadius[disk]]
    ],
    -1, (* NoncompactDisk *)
    Module[{c = GDiskCocenter[disk]},
      Circle[{Re[c],Im[c]}, GDiskCoradius[disk]]
    ],
    _, (* Halfplane *) 
    Module[{absb, b0, rot, clip, x},
      absb = Abs[m[[1,2]]];
      b0 = m[[1,2]] / absb;
      rot = {{Re@b0, -Im@b0}, {Im@b0, Re@b0}};
      clip = OptionValue[GDiskClipRadius];
      x = -m[[2,2]]/(2 absb);
      GeometricTransformation[
        Line[{{x, -clip}, {x,clip}}],
        rot
      ]
    ]
  ]
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
