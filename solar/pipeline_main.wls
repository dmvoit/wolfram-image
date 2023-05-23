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
(*DeleteObject[session]*)


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
