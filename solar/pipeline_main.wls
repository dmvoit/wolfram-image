#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Solar Pipeline *)


(* ::Subsection:: *)
(*Setup*)


SetDirectory[ParentDirectory@NotebookDirectory[]];


(* ::Input:: *)
(*RegisterExternalEvaluator["Python",*)
(*FileNameJoin[{$HomeDirectory,"miniconda3","python.exe"}]];*)


(* ::Subsection:: *)
(*Tensorflow*)


(* ::Input:: *)
(*session=StartExternalSession["Python"];*)
(*tensorflow=ExternalEvaluate[session,File["solar/tf.py"]];*)


(* ::Input:: *)
(*(*close session*)*)
(*(*DeleteObject[session]*)*)


(* ::Input:: *)
(*tf=Association[];*)
(**)
(*tfModel[imgPath_String]:=tensorflow[*)
(*FindFile[imgPath],FindFile["model/model_1.tflite"]]*)
(*tf["model"]=tfModel;*)
(**)
(*tf["result"]=tfModel["data/IMG_0043.JPG"];*)


(* ::Input:: *)
(*img=Import@FindFile["data/IMG_0043.JPG"];*)
(*tf["img"]=img;*)
(*img//Thumbnail*)


(* ::Input:: *)
(*ImageDimensions@img*)


(* ::Subsection:: *)
(*Recognition*)


(* ::Input:: *)
(*ListDensityPlot[tf["result"],*)
(*ColorFunction->"BlueGreenYellow",*)
(*ImageSize->Small,*)
(*ScalingFunctions->{None,"Reverse"},*)
(*FrameTicks->{None, Automatic}]*)


(* ::Input:: *)
(*tf["result"]//Image//Thumbnail*)


(* ::Input:: *)
(*heatmap=Image@ArrayPlot[*)
(*tf["result"],ColorFunction->"BlueGreenYellow",*)
(*Frame->False,PlotRangePadding->None];*)
(**)
(*Blend[{*)
(*img,*)
(*ImageResize[heatmap,ImageDimensions@img]},0.5]*)


(* ::Input:: *)
(*tf["threshold"]=Binarize@Threshold[*)
(*Image[tf["result"]],0.7]*)


(* ::Input:: *)
(*kernel=BoxMatrix[1];*)
(*tf["morp_open"]=Opening[tf["threshold"],kernel];*)
(*tf["morp_close"]=Closing[tf["morp_open"],kernel];*)
(*{tf["morp_open"],tf["morp_close"]}*)


(* ::Input:: *)
(*tf["mesh"]=ImageMesh@tf["morp_close"];*)
(*tf["mesh_bounds"]=RegionBounds@tf["mesh"];*)
(*tf["mesh_hull"]=ConvexHullMesh@tf["mesh"];*)
(*tf["mesh_hull_coord"]=MeshCoordinates@tf["mesh_hull"];*)
(*tf["mesh_hull_coord"]//Length*)


(* ::Input:: *)
(*Show[Image[tf["result"]],*)
(*Graphics[{Red,tf["mesh_hull_coord"]/.{x_,y_}->Point[{x,y}]}]*)
(*]*)


(* ::Input:: *)
(*bisectionImport=ExternalEvaluate[session,File["solar/bisection.py"]];*)
(*bisectionPy[ls_]:=Flatten[Normal@bisectionImport[ls],1]*)


(* ::Input:: *)
(*tf["corners_py"]=bisectionPy[tf["mesh_hull_coord"]]*)


(* ::Input:: *)
(*tf["corners_alg"]=Module[*)
(*{points=tf["mesh_hull_coord"],arc=Perimeter@tf["mesh_hull"],*)
(*func,found},*)
(*func[a_]:=RamerDouglasPeucker[points,a*arc,True];*)
(*found=Bisection[{0,1},*)
(*Length@func[#3]==4&,*)
(*Length@func[#3]>4&*)
(*,"Max"->10,"log"->False];*)
(*func[found]*)
(*]*)


(* ::Input:: *)
(*tf["corners_fnd"]=ImageCorners[MorphologicalPerimeter@tf["morp_close"], 5,0,10,MaxFeatures->4]*)


(* ::Input:: *)
(*Manipulate[*)
(*HighlightImage[Image@tf["result"],opt,*)
(*ImageSize->Small],{opt,{*)
(*tf["corners_py"]->"Py",*)
(*tf["corners_alg"]->"Own",*)
(*tf["corners_fnd"]->"Find"}},*)
(*ControlPlacement->Top]*)


(* ::Input:: *)
(*SortedCorners[{{p0x_,_},{p1x_,_},{p2x_,_},{p3x_,_}}]:=And[p0x < p1x,p3x < p2x ,Abs[p1x - p2x] < Abs[p3x - p2x],Abs[p1x - p2x] < Abs[p0x - p1x]]*)
(**)
(*SortCorners[points_]:=First@Select[*)
(*NestList[RotateLeft,points,3],SortedCorners]*)


(* ::Input:: *)
(*SortCorners@tf["corners_alg"]//Round*)
