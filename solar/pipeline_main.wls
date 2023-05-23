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


heatmap=Image@ArrayPlot[
tf["result"],ColorFunction->"BlueGreenYellow",
Frame->False,PlotRangePadding->None];

Blend[{
img,
ImageResize[heatmap,ImageDimensions@img]},0.5]


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
