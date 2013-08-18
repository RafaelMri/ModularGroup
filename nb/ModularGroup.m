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

TRList::usage = StringJoin[
  "TRList[t] returns the factors of unique R-T-product representation ",
  "of the modular transformation t as list ",
  "consisting of the ModularTransformations mtT, mtR and Inv@mtR. ", 
  "The transformation t may be given in matrix form ",
  "or as ModularTransformation object. "
];

TRRight::usage = StringJoin[
  "TRRight[t] returns the rightmost factor of the unique R-T-product representation ",
  "of the modular transformation t, which may be given in matrix form ",
  "or as ModularTransformation object. ",
  "The return value is one of the ModularTransformations mtT, mtR and Inv@mtR.\n",
  "TRRight[t] is equivalent to Last[TRList[t]]."
];

TRLeft::usage = StringJoin[
  "Returns the leftmost factor of the unique R-T-product representation ",
  "of the modular transformation t, which may be given in matrix form ",
  "or as ModularTransformation object. ",
  "The return value is one of the ModularTransformations mtT, mtR and Inv@mtR.\n", 
  "TRLeft[t] is equivalent to First[TRList[t]]."
];

TRWord::usage = StringJoin[
  "TRWord[t] returns the unique R-T-group word representation ",
  "of the modular transformation t which may be given in matrix form ",
  "or as ModularTransformation object."
];

TUExponents::usage = StringJoin[
  "TUExponents[t] returns for a ModularTransformation t a list of exponents ",
  "{\!\(\*SubscriptBox[\(e\), \(0\)]\),...,\!\(\*SubscriptBox[\(e\), \(n\)]\)} ",
  "such that t = \!\(\*SuperscriptBox[\(U\), SubscriptBox[\(e\), \(0\)]]\)\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(1\)]]\)...\!\(\*SuperscriptBox[\(TU\), SubscriptBox[\(e\), \(n\)]]\).",
  "\nThe option QuotientFunction is supported."
];
 
QuotientFunction::usage = StringJoin[
  "QuotientFunction is an option which defines the ",
  "integer quotient function to be used within the Euclidean algorithm. ",
  "Default is Mathematica's built-in function Quotient. ",
  "Predefined alternatives are CentralQuotient and CQuotient."
];

CentralQuotient::usage = StringJoin[
  "CentralQuotient[m,n] returns the integer quotient q of m = q n + r, ",
  "such that -n/2 \[LessEqual] r < n/2 (for positive q and n).\n",
  "CentralQuotient[m,n] is eqivalent to Quotient[m, n, -n/2]"
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
  "\nThe option QuotientFunction is supported."
];

TUWord::usage = StringJoin[
  "TUWord[t] returns a symbolic group word representation ",
  "of the ModularTransformation t in terms of the group generators T and U.",
  "\nThe option QuotientFunction is supported."
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

ModularGroupList::usage = StringJoin[
  "ModularGroupList[p] ",
  "returns a list of ModularTransformations satisfying the predicate p. ",
  "Starting with the identity transformation, ",
  "a duplicate-free list of transformations is generated ",
  "by successively applying T and U from the right, ",
  "using a depth-first-search algorithm ",
  "which continues as long as p returns True.",
  "\nThe options StartTransformation and MaxIterations are supported."
];
ModularGroupList::maxit =
  "Maximum number of iterations exceeded. Current setting: MaxIterations \[RightArrow] `1`.";

ModularGroupIterator::usage = StringJoin[
  "it = ModularGroupIterator[o] returns an Iterator for ModularTransformations ",
  "with ordering defined by the ordering predicate o.",
  "\nHasNext[it] returns True forever, as the ModularGroup has infinitely many elements.",
  "\nGetNext[it] returns the next ModularTransformation ",
  "determined by a priority-first-search algorithm, ",
  "which successively applies transformations T and U from the right ",
  "to already known transformations and uses the ordering predicate o."
];
StartTransformation::usage = StringJoin[
  "StartTransformation is an option ",
  "of ModularGroupList and ModularGroupIterator. ",
  "It defines the ModularTransformation which is used for starting the enumeration."
];

ModularSubgroupCosets::usage = StringJoin[
  "ModularSubgroupCosets[s, o] returns a list of coset representatives ",
  "of a subgroup of the modular group given by the membership predicate s. ",
  "The algorithm starts with the emtpy set ",
  "and successively adds applicable ModularTransformations ",
  "until a complete system of coset representatives is found. ",
  "The subgroup given by the membership predicate s ",
  "has to be of finite index within the modular group, ",
  "otherwise the algorithm will not terminate. ",
  "The order in which ModularTransformations are added to the result set ",
  "is given by the the ordering predicate o.\n",
  "The option StartTrasformation is supported."
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
    If[z === ComplexInfinity, 
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

(* ------------------------------------------- Special ModularTransformations *)
mtId = New[ModularTransformation, IdentityMatrix[2]];
Inv[mtId] ^= mtId; (* Identity is self-inverse *)

mtU = New[ModularTransformation, {{1,1},{0,1}}];

mtT = New[ModularTransformation, {{0,-1},{1,0}}];
Inv[mtT] ^= mtT; (* T is self-inverse *)

mtR = New[ModularTransformation, Mat[mtT].Mat[mtU]];

(* ---------------------------------------------------- Group word algorithms *)

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

TUWord[obj_?(InstanceQ[ModularTransformation]), opts:OptionsPattern[]] := 
Module[{factors},
  factors = DeleteCases[
    TUEval[obj, "T", "U", List, Superscript, opts],
    Superscript["U",0]
  ];
  If[factors === {}, "1", Row@factors]
];

TRLeft[obj_?(InstanceQ[ModularTransformation])] :=
TRLeft[obj] ^= TRLeft[Mat@obj];
TRLeft[mat_?MatrixQ] := Module[{a,b,c,d, ac, bd},
  {{a,b},{c,d}} = mat;
  ac = a c; bd = b d;
  Which[
    ac >= 0 && bd >= 0, mtT,
    a^2 + ac <= 0 && b^2 + bd <= 0, mtR,
    True, Inv@mtR
  ]
];
TRRight[obj_?(InstanceQ[ModularTransformation])] := 
TRRight[obj] ^= Inv@TRLeft[Inv@obj];
TRRight[mat_?MatrixQ] := Inv@TRLeft[Inv@mat];

TRList[obj_?(InstanceQ[ModularTransformation])] :=
TRList[obj] ^= TRList[Mat@obj];
TRList[obj_?MatrixQ] := Module[{mat = obj}, 
  Assert[MatrixQ[mat,IntegerQ] && Det[mat] == 1];
  Flatten[Reap[
    While[!MatchQ[mat, {{_,0},{0,_}}],
      mat = Mat[Inv[Sow@TRLeft@mat]] . mat;
    ];
  ][[2]]]
];

TRWord[obj_?(InstanceQ[ModularTransformation])] :=
TRWord[obj] ^= TRWord[Mat@obj];
TRWord[mat_?MatrixQ] := Module[{factors},
  factors = TRList[mat];
  If[factors === {}, 
    "1",
    Row@(factors /. {
      mtT -> "T", 
      mtR -> Superscript["R",1], 
      Inv@mtR -> Superscript["R",2]
    })
  ]
];


(* --------------------------------------------------- Enumeration algorithms *)

Transition[ModularGroup] = Function[cur, 
Module[{steps, next},
  steps = NeighborSteps[cur];
  Table[
    next = cur@step;
    Parent[next] ^= cur;
    NeighborSteps[next] ^= If[step === mtT, Inv@#&/@Rest[steps], {mtT, step}];
    next, {step, steps}]
]];

ModularGroupList[criteria_, OptionsPattern[]] := Reap[
Module[{i, max, mtStart, it},
  i = 0;
  max = OptionValue[MaxIterations];
  mtStart = OptionValue[StartTransformation];
  NeighborSteps[mtStart] ^= {mtT, mtU, Inv@mtU};
  it = New[DFSIterator, mtStart, Transition[ModularGroup], criteria];
  While[i++ < max && HasNext@it,
    Sow@GetNext@it
  ];
  If[i >= max, Message[ModularGroupList::maxit, max]];
]][[2,1]];
Options[ModularGroupList] = {MaxIterations -> 2^12, StartTransformation->mtId};

ModularGroupIterator[ordering_, OptionsPattern[]] := 
Module[{mtStart},
  mtStart = OptionValue[StartTransformation];
  NeighborSteps[mtStart] ^= {mtT, mtU, Inv@mtU};
  New[PFSIterator, mtStart, Transition[ModularGroup], ordering]
];
Options[ModularGroupIterator] = {StartTransformation -> mtId};

ModularSubgroupCosets[subgroupMemberQ_, ordering_, OptionsPattern[]] := 
Module[{i, max, cosets, mtStart, Exclude, Neighbors, NotYetSeen, n, it},
  i = 0;
  max = OptionValue[MaxIterations];
  cosets = {};
 
  mtStart = OptionValue[StartTransformation];
  Exclude@mtStart = Null;
 
  NotYetSeen[m_] := !MemberQ[cosets, _?(subgroupMemberQ[#@Inv[m]]&)];
  Neighbors[m_] := (
    n = m@#;
    Exclude@n = Inv@#;
    n
  )& /@ DeleteCases[{mtT, mtU, Inv@mtU}, Exclude@m];

  it = New[PFSIterator, mtStart, Neighbors, ordering, NotYetSeen];
  While[i++ < max && HasNext[it],
    AppendTo[cosets, GetNext[it]];
  ];
  If[i >= max, Message[ModularGroupList::maxit, max]];
  cosets
];
Options[ModularSubgroupCosets] = {MaxIterations -> 2^10, StartTransformation -> mtId};


(* -------------------------------------------------- Class GeneralizedDisk *)
NewClass[GeneralizedDisk];
NewClass[NoncompactDisk];
NewClass[Halfplane];
NewClass[CompactDisk];

Init[GeneralizedDisk, obj_, pmat_?MatrixQ] ^:= Module[{a, mat},
  mat=FullSimplify[pmat];
  Assert[HermitianMatrixQ@mat && Simplify[Det[mat] < 0]];
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

(* ----------------------------------------------- Special GeneralizedDisks *)

gdUnitDisk = New[CompactDisk, 0, 1];
gdUpperHalfplane = New[Halfplane, 0, I];
gdLowerHalfplane = New[Halfplane, 0, -I];
gdRightHalfplane = New[Halfplane, 0, 1];
gdLeftHalfplane = New[Halfplane, 0, -1];
gdUnitCodisk = New[NoncompactDisk, 0, 1];

(* ------------------------------------------- Methods for GeneralizedDisks *)

InteriorQ[disk_?(InstanceQ[GeneralizedDisk]), z_?VectorQ] := (
  Re[Conjugate[z].Mat[disk].z] <= 0
)

InteriorQ[disk_?(InstanceQ[GeneralizedDisk]), z_] := (
  If[TrueQ[z === ComplexInfinity], 
    !InstanceQ[CompactDisk][disk], 
    InteriorQ[disk,{z,1}]
  ]
);

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

(* ----------------------------------------------------- Drawing algorithms *)
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

GCircle[disk_?(InstanceQ[GeneralizedDisk]), opts:OptionsPattern[]] := (
  GCircle[Mat@disk, opts]
);

GCircle[m_?MatrixQ, opts:OptionsPattern[]] :=
If[class[m] == 0,
  (* Halfplane *) 
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
  ],
  (* CompactDisk or NoncompactDisk *)
  Module[{c = center[m]},
    Circle[{Re[c],Im[c]}, radius[m]]
  ] 
];

GCircle[disk_?(InstanceQ[GeneralizedDisk]), tlist_List, opts:OptionsPattern[]] :=
Module[{dm, mlists},
  dm = Mat@disk;
  mlists = SplitBy[ConjugateTranspose[Mat@Inv@#].dm.Mat[Inv@#]& /@ tlist, class];
  Table[
    If[class[mlist[[1]]] == 0,
      Table[GCircle[m, opts], {m, mlist}],      
      GeometricTransformation[Circle[], udTransforms[mlist]]
    ],
  {mlist, mlists}]
];



End[];

(*Protect[MoebiusTransformation, Mat, Inv];
Protect[ModularTransformation, QuotientFunction];
Protect[TUExponents, TUEval, TUWord];*)

EndPackage[];
