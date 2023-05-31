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


(* ::Subsection:: *)
(*Experimental*)


(* ::Input:: *)
(*Options[Bisection]={*)
(*"Max"->50,"Tol"->0.01,"log"->True};*)
(*Bisection[{a_,b_},test_,condtion_,*)
(*OptionsPattern[]]:=Module[*)
(*{n=1,\[CapitalNu],mid,tol,aa=a,bb=b},*)
(*(**)*)
(*\!\(\*SubscriptBox[\(\[CapitalNu]\), \("\<max\>"\)]\)=OptionValue["Max"];*)
(*tol=OptionValue["Tol"];*)
(*While[n<\!\(\*SubscriptBox[\(\[CapitalNu]\), \("\<max\>"\)]\),*)
(*mid=(aa+bb)/2;*)
(*(*test*)*)
(*If[*)
(*test[aa,bb,mid],*)
(*Return[N@mid]*)
(*];*)
(*If[OptionValue["log"],*)
(*Print[n,"\[Rule]",N@mid," cond=",condtion[aa,bb,mid]," test=",test[aa,bb,mid]]];*)
(*(*condition*)*)
(*If[condtion[aa,bb,mid],*)
(*aa=mid,*)
(*bb=mid*)
(*];*)
(*n++];*)
(*Message["max number of steps exceeded"];*)
(*]*)


(* ::Code:: *)
(*(* transform between python opencv and wolfram*)*)
(*reverseYAxis[ySize_][xy_]:=TransformationFunction[{*)
(*{1,0,0},{0,-1,ySize},{0,0,1}}][xy]*)


(* ::Text:: *)
(*MeshRegion[*)
(*reverseYAxis[224]/@MeshCoordinates@tf["mesh_hull"]//Polygon]*)
(*convert from points back to mesh*)


(* ::Text:: *)
(*cv2.arcLength(points, closed=False)*)
(*is same as ArcLength@Line@points*)
(**)
(*cv2.arcLength(points, closed=True)*)
(*is same as Perimeter@tf["mesh_hull"]*)
(**)


(* ::Input:: *)
(*RamerDouglasPeucker[{p1_,p2_},_]:={p1,p2}*)
(*RamerDouglasPeucker[points_,th_,loop_:False]:=Module[*)
(*{p1,p2,perpendicular,dist,maxPos},*)
(*p1=First@points;*)
(*p2=Last@points;*)
(*perpendicular=Normalize[p2-p1] . RotationMatrix[90\[Degree]];*)
(*dist=perpendicular . (#-p1)&/@points//Abs;*)
(*maxPos=First@Ordering[dist,-1];*)
(**)
(*If[dist[[maxPos]]<th,*)
(*{p1,p2},*)
(*Join[*)
(*RamerDouglasPeucker[points[[;;maxPos]],th],Rest@RamerDouglasPeucker[points[[maxPos;;]],th]]*)
(*]*)
(*]*)
(*RamerDouglasPeucker[points_,th_,True]:=Module[{*)
(*p1,p2,maxSrt,maxMid,dm,maxDis,pointsL},*)
(*(*loop*)*)
(*(*find 2 points with highest distance*)*)
(*dm=DistanceMatrix@points;*)
(*maxDis=Max@dm;*)
(*{maxSrt,maxMid}=Sort[Position[dm,maxDis][[1]]];*)
(**)
(*(*make first point begining of the list*)*)
(*pointsL=RotateLeft[points,maxSrt-1];*)
(*{maxSrt,maxMid}={1,maxMid-maxSrt+1};*)
(*{p1,p2}=pointsL[[{maxSrt,maxMid}]];*)
(**)
(*If[maxDis<th,*)
(*{p1,p2},*)
(*Join[*)
(*(*make first point begining of the list*)*)
(*RamerDouglasPeucker[pointsL[[;;maxMid]],th],*)
(*Extract[{2;;-2}]@RamerDouglasPeucker[pointsL[[maxMid;;]]//Append@First@pointsL,th]*)
(*]*)
(*]]*)
(*RamerDouglasPeucker[region_BoundaryMeshRegion,th_]:=RamerDouglasPeucker[MeshCoordinates@region,th,True]//Polygon//BoundaryMesh*)


(* ::Input:: *)
(**)
(*pointsCompare[img_,data_:{{ls_List->name_String}..}]:=Manipulate[Show[img,Graphics[{Red,opt/.{x_,y_}->Point[{x,y}]*)
(*}]*)
(*],{opt,data},*)
(*ControlPlacement->Top]*)


(* ::Input:: *)
(*CameraMatrix[img_]:=Module[{height, width,center},*)
(*{height, width}=ImageDimensions@img;*)
(*center={height, width}/2;*)
(*({*)
(* {height, 0, center[[1]]},*)
(* {0, height, center[[2]]},*)
(* {0, 0, 1}*)
(*})]*)
