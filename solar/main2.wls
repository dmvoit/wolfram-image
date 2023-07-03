#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@ParentDirectory@NotebookDirectory[];
img=Import@FindFile["data/solar/IMG_0043.JPG"];
img//Thumbnail


enc=NetEncoder[
{"Function",
ImageData[ImageResize[#,{224,224}],"Byte"]&,
{224,224,3},Pattern->_Image}];

dec=NetDecoder[{"Function",#[[All,All,2]]&}];

model=Import@FindFile["data/solar/model_2a.onnx"];


{inName,outName}=Information[model,
{"InputPortNames","OutputPortNames"}]//Flatten


net=NetChain[{model},
inName->enc, outName->dec]


ArrayPlot[
net@img,
ColorFunction->"BlueGreenYellow",
PlotRangePadding->None]
