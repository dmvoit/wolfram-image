(* ::Package:: *)

BeginPackage[ "Package`"]


Bisection::usage =
    "Bisection[ {a,b},test,condition] bisection search."


Begin[ "`Private`"]
Options[Bisection] = {
  "Max" -> 50, "Tol" -> 0.01, "log" -> True};

Bisection[{a_, b_}, test_, condition_,
  OptionsPattern[]] := Module[
  {n = 1, max, mid, tol, aa = a, bb = b},
  (**)
  max = OptionValue["Max"];
  tol = OptionValue["Tol"];

  While[n < max,
    mid = (aa + bb) / 2;
    (*test*)
    If[
      test[aa, bb, mid],
      Return[N@mid]
    ];
    If[OptionValue["log"],
      Print[n, "\[Rule]", N@mid, " cond=", condition[aa, bb, mid], " test=", test[aa, bb, mid]]];
    (*condition*)
    If[condition[aa, bb, mid],
      aa = mid,
      bb = mid
    ];
    n++];
  Message["max number of steps exceeded"];
]


End[]
EndPackage[]
