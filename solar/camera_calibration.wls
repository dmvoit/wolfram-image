#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Direct Linear Transform*)


(* ::Subsection:: *)
(*Equation*)


Subscript[\[DoubleStruckCapitalZ], n_]:=ConstantArray[{0},{n}]
Subscript[\[DoubleStruckCapitalZ], n_,m_]:=ConstantArray[0,{n,m}]
Subscript[\[DoubleStruckCapitalO], n_]:=ConstantArray[{1},{n}]
Subscript[\[DoubleStruckCapitalO], n_,m_]:=ConstantArray[1,{n,m}]
Subscript[P, n_,m_]:=Array[Subscript[p, ##]&,{n,m}]
Subscript[\[CapitalIota], n_]:=IdentityMatrix[n]

AngleBracket[mat_]:=ArrayFlatten[mat]
Subscript[v_List, H]:=v//Append@{1}
Subscript[v_List, e]:=Most@v/First@Last@v
Unlist[ls_]:=Replace[ls,{x_}->x,2];
vec[v_]:=v[[All,1]]


(* ::Subsubsection:: *)
(*Definitions*)


(* ::Text:: *)
(* Intrinsic matrix K:*)
(*K=({*)
(* {Subscript[\[ScriptF], x], \[ScriptS], Subscript[\[ScriptX], 0]},*)
(* {0, Subscript[\[ScriptF], y], Subscript[\[ScriptY], 0]},*)
(* {0, 0, 1}*)
(*})*)
(*where:*)
(*\[ScriptF] - focal lengths of the camera in terms of pixel*)
(*dimensions*)
(*(Subscript[\[ScriptX], 0], Subscript[\[ScriptY], 0]) - principal point on the image plane (image center)*)
(*\[ScriptS] - skew parameter*)
(*Links:*)
(*principal point*)
(**)
(*R=({*)
(* {Subscript[r, 1,1], Subscript[r, 1,2], Subscript[r, 1,3]},*)
(* {Subscript[r, 2,1], Subscript[r, 2,2], Subscript[r, 2,3]},*)
(* {Subscript[r, 3,1], Subscript[r, 3,2], Subscript[r, 3,3]}*)
(*})*)
(*rotations R = Subscript[R, x] Subscript[R, y] Subscript[R, z]*)
(*Subscript[X, 0]=({*)
(* {Subscript[t, 1]},*)
(* {Subscript[t, 2]},*)
(* {Subscript[t, 3]}*)
(*})*)
(*translation and also camera location*)


(* ::Text:: *)
(*Full transformation*)


(* ::Text:: *)
(*Subscript[x, image]=K . R . [Subscript[I, 3] Subscript[X, 0]] . Subscript[X, world]*)
(*\!\( *)
(*TagBox[*)
(*RowBox[{*)
(*RowBox[{"(", "", GridBox[{*)
(*{"x"},*)
(*{"y"},*)
(*{"1"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}], "==", *)
(*RowBox[{*)
(*RowBox[{"(", "", GridBox[{*)
(*{*)
(*SubscriptBox["\[ScriptF]", "x"], "\[ScriptS]", *)
(*SubscriptBox["\[ScriptX]", "0"]},*)
(*{"0", *)
(*SubscriptBox["\[ScriptF]", "y"], *)
(*SubscriptBox["\[ScriptY]", "0"]},*)
(*{"0", "0", "1"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}], ".", *)
(*RowBox[{"(", "", GridBox[{*)
(*{*)
(*SubscriptBox["r", *)
(*RowBox[{"1", ",", "1"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"1", ",", "2"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"1", ",", "3"}]]},*)
(*{*)
(*SubscriptBox["r", *)
(*RowBox[{"2", ",", "1"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"2", ",", "2"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"2", ",", "3"}]]},*)
(*{*)
(*SubscriptBox["r", *)
(*RowBox[{"3", ",", "1"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"3", ",", "2"}]], *)
(*SubscriptBox["r", *)
(*RowBox[{"3", ",", "3"}]]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}], ".", *)
(*RowBox[{"(", "", GridBox[{*)
(*{"1", "0", "0", *)
(*RowBox[{"-", *)
(*SubscriptBox["t", "1"]}]},*)
(*{"0", "1", "0", *)
(*RowBox[{"-", *)
(*SubscriptBox["t", "2"]}]},*)
(*{"0", "0", "1", *)
(*RowBox[{"-", *)
(*SubscriptBox["t", "3"]}]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}], ".", *)
(*RowBox[{"(", "", GridBox[{*)
(*{"X"},*)
(*{"Y"},*)
(*{"Z"},*)
(*{"1"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}]}]}],*)
(*HoldForm]\)*)


(* ::Text:: *)
(*set all unknowns to P*)


(* ::Text:: *)
(*P==({*)
(* {Subscript[\[ScriptF], x], \[ScriptS], Subscript[\[ScriptX], 0]},*)
(* {0, Subscript[\[ScriptF], y], Subscript[\[ScriptY], 0]},*)
(* {0, 0, 1}*)
(*}) . ({*)
(* {Subscript[r, 1,1], Subscript[r, 1,2], Subscript[r, 1,3]},*)
(* {Subscript[r, 2,1], Subscript[r, 2,2], Subscript[r, 2,3]},*)
(* {Subscript[r, 3,1], Subscript[r, 3,2], Subscript[r, 3,3]}*)
(*}) . ({*)
(* {1, 0, 0, -Subscript[t, 1]},*)
(* {0, 1, 0, -Subscript[t, 2]},*)
(* {0, 0, 1, -Subscript[t, 3]}*)
(*})*)
(**)
(*P==({*)
(* {Subscript[p, 1,1], Subscript[p, 1,2], Subscript[p, 1,3], Subscript[p, 1,4]},*)
(* {Subscript[p, 2,1], Subscript[p, 2,2], Subscript[p, 2,3], Subscript[p, 2,4]},*)
(* {Subscript[p, 3,1], Subscript[p, 3,2], Subscript[p, 3,3], Subscript[p, 3,4]}*)
(*})*)


(* ::Subsubsection:: *)
(*Solution*)


\[DoubleStruckX]=({
 {x},
 {y}
});\[DoubleStruckCapitalX]=({
 {X},
 {Y},
 {Z}
});Subscript[\[DoubleStruckCapitalT], 0]=({
 {Subscript[t, 1]},
 {Subscript[t, 2]},
 {Subscript[t, 3]}
});
\[LeftAngleBracket]{
 {Subscript[\[CapitalIota], 3], -Subscript[\[DoubleStruckCapitalT], 0]}
}\[RightAngleBracket]


\[DoubleStruckX]==Subscript[(Subscript[P, 3,4] . Subscript[\[DoubleStruckCapitalX], H]), e]//Thread//Unlist//Column


ClearAll[\[DoubleStruckCapitalA],\[DoubleStruckCapitalB],\[DoubleStruckCapitalC]]
\[DoubleStruckCapitalA]\[DoubleStruckCapitalB]\[DoubleStruckCapitalC]={\[DoubleStruckCapitalA],\[DoubleStruckCapitalB],\[DoubleStruckCapitalC]};
\[DoubleStruckCapitalA]\[DoubleStruckCapitalB]\[DoubleStruckCapitalC]=\!\(\*SubscriptBox[\(\[DoubleStruckCapitalA]\[DoubleStruckCapitalB]\[DoubleStruckCapitalC]\), \(\(\[LeftDoubleBracket]\)\(#\)\(\[RightDoubleBracket]\)\)]\)->List/@Subscript[P, 3,4][[#,All]]&/@Range@3


ClearAll[\[DoubleStruckCapitalA],\[DoubleStruckCapitalB],\[DoubleStruckCapitalC]];
({
 {x},
 {y},
 {w}
})==({
 {\[DoubleStruckCapitalA]\[Transpose]},
 {\[DoubleStruckCapitalB]\[Transpose]},
 {\[DoubleStruckCapitalC]\[Transpose]}
})*({
 {Subscript[X, 1]},
 {Subscript[X, 2]},
 {Subscript[X, 3]}
})


\[DoubleStruckX]==Subscript[(({
 {\[DoubleStruckCapitalA]\[Transpose]},
 {\[DoubleStruckCapitalB]\[Transpose]},
 {\[DoubleStruckCapitalC]\[Transpose]}
})*({
 {Subscript[X, 1]},
 {Subscript[X, 2]},
 {Subscript[X, 3]}
})), e]//Thread//Unlist//Column


BalanceSides[eq_Equal]:=Module[{den,res},
den=Denominator/@List@@eq;
den=SelectFirst[#=!=1&]@den;
res=MultiplySides[eq,den,Assumptions -> den != 0];
SubtractSides@res
]


\[DoubleStruckX]==Subscript[(({
 {\[DoubleStruckCapitalA]\[Transpose]},
 {\[DoubleStruckCapitalB]\[Transpose]},
 {\[DoubleStruckCapitalC]\[Transpose]}
})*({
 {Subscript[X, 1]},
 {Subscript[X, 2]},
 {Subscript[X, 3]}
})), e]//Thread//Unlist//Map[BalanceSides,#]&//Column


\[DoubleStruckP]=vec[\[LeftAngleBracket]{
 {Subscript[\[DoubleStruckCapitalA], 1]},
 {Subscript[\[DoubleStruckCapitalB], 1]},
 {Subscript[\[DoubleStruckCapitalC], 1]}
}\[RightAngleBracket]];
Subscript[a, x]=\[LeftAngleBracket]{
 {-Subscript[\[DoubleStruckCapitalX], H]},
 {Subscript[\[DoubleStruckCapitalZ], 4]},
 {x Subscript[\[DoubleStruckCapitalX], H]}
}\[RightAngleBracket];Subscript[a, y]=\[LeftAngleBracket]{
 {Subscript[\[DoubleStruckCapitalZ], 4]},
 {-Subscript[\[DoubleStruckCapitalX], H]},
 {y Subscript[\[DoubleStruckCapitalX], H]}
}\[RightAngleBracket];
\[LeftAngleBracket]{
 {Subscript[a, x]\[Transpose]},
 {Subscript[a, y]\[Transpose]}
}\[RightAngleBracket] . \[DoubleStruckP]==0//Thread//Column


\[LeftAngleBracket]{
 {Subscript[a, x]\[Transpose]},
 {Subscript[a, y]\[Transpose]}
}\[RightAngleBracket]


(* ::Subsubsection:: *)
(*Function*)


(* ::Text:: *)
(* Normalizing transformations is needed for better results.*)
(*Sources:*)
(*Multiple View Geometry in Computer Vision (2nd Edition)  Hartley and Zisserman  -  4.4.4 Normalizing transformations - Page 107*)
(* Data normalization - Page 180*)
(*Homography Estimation by Elan Dubrofsky -  2.1.1 Normalization - Page 11*)


DLTNormalizationMatrix::usage="normalization transformation matrix for points near the camera
based on Multiple View Geometry in Computer Vision Page 107,180";
DLTNormalizationMatrix[points_List]:=Module[
 {mean, len,scale},
		mean = Mean@points;
		len = Dimensions[points][[2]];
		(* make mean centroid of all points *)
		(* scale average distance from the origin *)
		scale = Sqrt[len]/RootMeanSquare[Norm/@(#-mean&/@points)];
		(* transformation matrix *)
		mean=List/@mean;(*transform to vector*)
		ArrayFlatten[{
		scale*{IdentityMatrix[len],-mean},
		{0,1}}]
]


ToHom::usage="converts points to homogeneous";
ToHom[ls:{_?NumericQ,_}|{_?NumericQ,_,_}]:=ls//Append@1
ToHom[ls:{({_?NumericQ,_}|{_?NumericQ,_,_})..}]:=ToHom/@ls

FromHom::usage="converts points from homogeneous";
FromHom[ls:{_?NumericQ,_,_}|{_?NumericQ,_,_,_}]:=Most@ls/Last@ls
FromHom[ls:{({_?NumericQ,_,_}|{_?NumericQ,_,_,_})..}]:=FromHom/@ls


CameraCalibration[imageP_,wordP_,"DLT"]:=Module[
{world=wordP, image=imageP,imgT,worldT,
A,P,X,Y,Z,x,y,m},
(* Normalize the points. *)
imgT = DLTNormalizationMatrix@image;
worldT = DLTNormalizationMatrix@world;
image = imgT . ToHom[image]\[Transpose]//Transpose//FromHom;
world = worldT . ToHom[world]\[Transpose]//Transpose//FromHom;

(*build the homogeneous linear system*)
A={
Join[-{X,Y,Z,1},{0,0,0,0},x*{X,Y,Z,1}],
Join[{0,0,0,0},-{X,Y,Z,1},y*{X,Y,Z,1}]
}/.Inner[Rule,{X,Y,Z,x,y}, 
#,List]&/@ArrayFlatten[{{world,image}}];

A=Flatten[A,1];
(*{P,m}={Partition[#[[3,All,-1]],4],#[[2,-1,-1]]}&@SingularValueDecomposition[A]*);
P = Partition[SingularValueDecomposition[A][[3,All,-1]],4];

(* denormalize *)
-Inverse[imgT] . P . worldT
]


(* ::Text:: *)
(*P==({*)
(* {Subscript[p, 1,1], Subscript[p, 1,2], Subscript[p, 1,3], Subscript[p, 1,4]},*)
(* {Subscript[p, 2,1], Subscript[p, 2,2], Subscript[p, 2,3], Subscript[p, 2,4]},*)
(* {Subscript[p, 3,1], Subscript[p, 3,2], Subscript[p, 3,3], Subscript[p, 3,4]}*)
(*})==({*)
(* {\[ScriptCapitalH], \[ScriptH]}*)
(*})==({*)
(* {\[CapitalKappa] . R, -\[CapitalKappa] . R . ({*)
(*   {Subscript[t, 1]},*)
(*   {Subscript[t, 2]},*)
(*   {Subscript[t, 3]}*)
(*  })}*)
(*}) *)
(*  \[ScriptCapitalH]=\[CapitalKappa] . R and h=-\[CapitalKappa] . R . Subscript[X, 0]*)
(*\[ScriptCapitalH]^-1=(\[CapitalKappa] . R)^-1=R^-1 K^-1*)
(*R^-1=R\[Transpose] since R is a rotation matrix*)


DecomposeProjectionMatrix[mat_]:=Module[
{K,R,t,P=mat,H,h,r180},
	(*  H as the first three columns of P. *)
	H = P[[All,1;;3]];(*H=K.R *)
	h = P[[All,4]];(*h=-K.R.Subscript[X, 0] *)
	t=Inverse[-H] . h;
	{R,K}=QRDecomposition[Inverse[H]];
	(* rotate 180 deg to face the scene *)
	(*r180=RotationMatrix[180 Degree,{0,0,1}];*)
	K=Inverse[K];
	{K/K[[3,3]],-R,t}
]


(* ::Subsubsection:: *)
(*Example*)


(*data*)
worldPoints={
{0, 0, 0}, 
{0, 0, 1}, 
{0, 1, 1}, 
{1, 0, 0}, 
{1, 0, 1}, 
{1, 1, 0}, 
{1, 1, 1}};
imagePoints={
{29.600000000000023, 112.66666666666669}, 
{19.33333333333337, 297.4666666666666}, 
{175.20000000000005, 353.3080957242207}, 
{176.14562558795865, 24.230391114624922}, 
{178.03687676387585, 224.70301576184983}, 
{314.2069614299153, 118.79294991048579}, 
{332.1738476011287, 296.57056044670406}};


DLTNormalizationMatrix[imagePoints]


projectionMat=CameraCalibration[imagePoints,worldPoints,"DLT"]


DecomposeProjectionMatrix@projectionMat//Column


{K1,R1,FromHom@t1}//Column
