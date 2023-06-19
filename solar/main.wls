#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Pipeline *)


(* ::Subsection:: *)
(*Setup*)


SetDirectory@ParentDirectory@NotebookDirectory[];

Get@*FindFile /@ {
   "solar/functions.m",
   "solar/bisection.wl", 
   "solar/ramerdouglaspeucker.wl",
   "solar/CameraCalibration.wl"
   };
(* python location *)
RegisterExternalEvaluator["Python",
FileNameJoin[{$HomeDirectory,"miniconda3","python.exe"}]];

img=Import@FindFile["data/solar/IMG_0043.JPG"];
img//Thumbnail


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
(*FindFile[imgPath],FindFile["data/solar/model_1.tflite"]]*)
(*tf["model"]=tfModel;*)
(**)
(*tf["result"]=tfModel["data/solar/IMG_0043.JPG"];*)


(* ::Input:: *)
(*tf["img"]=img;*)
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
(*Graphics[{Red,Point/@tf["mesh_hull_coord"]*)
(*}]]*)


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


(* ::Code:: *)
(*HighlightImage[Image@tf["result"],*)
(*tf[#[[1]]],*)
(*ImageSize->Small,*)
(*PlotLabel->#[[2]]*)
(*]&/@{*)
(*"corners_py"->"py",*)
(*"corners_alg"->"own",*)
(*"corners_fnd"->"find"}//FlipView	*)


(* ::Input:: *)
(*SortCorners[points_]:=Module[{cond},*)
(*cond[{{p0x_,_},{p1x_,_},{p2x_,_},{p3x_,_}}]:=And[*)
(*p0x < p1x,*)
(*p3x < p2x ,*)
(*Abs[p1x - p2x] < Abs[p3x - p2x],Abs[p1x - p2x] < Abs[p0x - p1x]];*)
(*First@Select[NestList[RotateLeft,points,3],cond]]*)
(*SortCorners@tf["corners_alg"]//Round*)


(* ::Subsection:: *)
(*Projection*)


pointsWorld={
{0,0,0},
{0,1,0},
{1,1,0},
{1,0,0}};
pointsAxis={
{0,0,0},
{1,0,0},
{0,1,0},
{0,0,1}};
Kmat={
{756,0,504},
{0,756,378},
{0,0,1}};
(* same as Kmat=CameraMatrix[targetSize]*)
targetSize=ImageDimensions@img/4


(* ::Code:: *)
(*(**)*)
(*tf["alg_scaled"]=PointsScale[targetSize, 224][*)
(*SortCorners@tf["corners_alg"]]*)


(* ::Code:: *)
(*tf["proj_alg"]=FindHomography[*)
(*tf["alg_scaled"],*)
(*pointsWorld,*)
(*Kmat,"Solve"];*)
(*tf["proj_alg"]@pointsWorld*)


(* ::Code:: *)
(*Module[{p1,p2,p3,p4},*)
(*{p1,p2,p3,p4}=tf["proj_alg"]@pointsWorld;*)
(*HighlightImage[ImageResize[img,targetSize],*)
(*{*)
(*Green,Line[{p1,p2}],Line[{p3,p4}],*)
(*Red,Line[{p2,p3}],Line[{p1,p4}]*)
(*}]]*)


(* ::Code:: *)
(*DrawAxis[proj_]:=Module[{or,p1,p2,p3},*)
(*{or,p1,p2,p3}=proj@pointsAxis;*)
(*HighlightImage[ImageResize[img,targetSize],*)
(*{*)
(*Green,Line[{or,p1}],*)
(*Red,Line[{or,p2}],*)
(*Blue,Line[{or,p3}]*)
(*}]]*)


(* ::Code:: *)
(*DrawAxis[tf["proj_alg"]]*)


(* ::Subsection:: *)
(*3D Camera*)


(* ::Code:: *)
(*Module[{P,R,t,cx,cy,K=Kmat,fx,fy},*)
(*P=Inverse[K] . tf["proj_alg"]["TransformationMatrix"];*)
(*{R,t} = SplitMatrix[P,{1;;3,4}];*)
(*{cx,cy} = {K[[1,3]],K[[2,3]]};*)
(*{fx,fy}={K[[1,1]],K[[2,2]]};*)
(*Show[*)
(*Graphics3D[{Blue,Line[{First@#,Last@#}&@pointsAxis]}],*)
(*Graphics3D[{Orange,Polygon[pointsWorld]}],*)
(*Graphics3D[CreatePyramid[t,{cx,cy}/{fx,fy},1]],*)
(*PlotRange->{{-2,2},{-2,2},{0,3}},*)
(*RotationAction->"Clip",Axes->True*)
(*]*)
(*]*)


Module[{P,R,t,cx,cy,K=Kmat,fx,fy},
P=Inverse[K] . tf["proj_alg"]["TransformationMatrix"];
{R,t} = SplitMatrix[P,{1;;3,4}];
(*R=RotationMatrix[90 Degree,{0,0,1}].R;*)
{cx,cy} = {K[[1,3]],K[[2,3]]};
{fx,fy}={K[[1,1]],K[[2,2]]};
Show[
Graphics3D[{Blue,Line[{First@#,Last@#}&@pointsAxis]}],
Graphics3D[{Orange,Polygon[pointsWorld]}],
Graphics3D[{Opacity[0.5],GeometricTransformation[
CreatePyramid[{0,0,0},{cy,cx}/{fy,fx},1],
TransformationFunction[
	ArrayFlatten[{{-R,t},{0,1}}]]]}
	],
PlotRange->{{-2,2},{-2,2},{0,3}},Axes->True,
RotationAction->"Clip"
]]


(* ::Code:: *)
(*Camera[K_,P_]:=Module[{cx,cy,fx,fy,R,t},*)
(*{cx,cy} = {K[[1,3]],K[[2,3]]};*)
(*{fx,fy}={K[[1,1]],K[[2,2]]};*)
(*{R,t} = SplitMatrix[P,{1;;3,4}];*)
(*CreatePyramid[t,{cx,cy}/{fx,fy},1]]*)


Module[{P,R,t,cx,cy,K=Kmat,fx,fy,trmat,res},
P=Inverse[K] . tf["proj_alg"]["TransformationMatrix"];
{R,t} = SplitMatrix[P,{1;;3,4}];
(*R=RotationMatrix[90 Degree,{0,0,1}].R;*)
{cx,cy} = {K[[1,3]],K[[2,3]]};
{fx,fy}={K[[1,1]],K[[2,2]]};
res=TransformationMatrix@RescalingTransform[
{{0, 1}, {0, 1},{0,1}}];
trmat=TransformationMatrix@TransformationFunction[
	ArrayFlatten[{{-R,t},{0,1}}]];
Framed[Show[
Graphics3D[{Blue,Sphere[First@pointsWorld,0.05]}],
Graphics3D[{Orange,Polygon[pointsWorld]}],
PlotRange->{{-2,2},{-2,2},{0,3}},
Axes->False,
Background -> White,
RotationAction->"Clip",
ViewProjection->"Perspective",
Boxed -> False,
ImageSize->targetSize/4,
ViewMatrix->{trmat,Automatic}
],
FrameStyle -> None,
Background -> LightGray]]


(* ::Subsection:: *)
(*Some Data*)


(* ::Input:: *)
(*Import[FindFile["data/solar/IMG_0043.JPG"],"Exif"][[{"FocalLength","LensSpecification","PixelXDimension","PixelYDimension","FocalLengthIn35mmFilm"}]]*)


(* ::Code:: *)
(*P={*)
(* {-0.08861924, 0.99532623, 0.03837087, 0.15477214},*)
(* {0.9897754, 0.09231669, -0.1087303, -1.00098198},*)
(* {-0.11176439, 0.02834294, -0.99333046, 2.26264187}*)
(*};*)


(* ::Code:: *)
(*{#[[1]]/#[[3]],#[[2]]/#[[3]]}&@(CameraMatrix[targetSize] . P . {0,0,0,1})*)


(* ::Code:: *)
(*projFunc=FindHomography[*)
(*{{432.,168.},{760.,209},{747.,523},{396.,506}},*)
(*pointsWorld,*)
(*Kmat,"Solve"];*)
(*projFunc@{0,0,0}*)


(* ::Code:: *)
(*(* Projection Matrix*)*)
(*Inverse[Kmat] . projFunc["TransformationMatrix"]*)


(* ::Code:: *)
(*P*)


(* ::Subsection:: *)
(*Camera*)


Module[{TrMat,P,K,Rmat,t},
TrMat=projFunc["TransformationMatrix"];
K=CameraMatrix[{1008,756}];
P=Inverse[K] . TrMat;
{Rmat,t}=SplitMatrix[P,{1;;3,4}];
t=t//Flatten;
Show[Graphics3D[{
Polygon[{{0,0,0},{0,1,0},{1,1,0},{1,0,0}}],
(*camera position*)
Red,
Sphere[t*{1,1,-1},0.1]
}],
PlotRange->{{-2,2},{-2,2},{0,3}}]
]


Module[{TrMat,K,Rmat,t,P},
TrMat=projFunc["TransformationMatrix"];
K=CameraMatrix[{1008,756}];
P=Inverse[K] . TrMat;
{Rmat,t}=SplitMatrix[P,{1;;3,4}];
t=t//Flatten;
Show[Graphics3D[{
Polygon[{{0,0,0},{0,1,0},{1,1,0},{1,0,0}}],
(*camera position*)
Red,
Sphere[t*{1,1,-1},0.1]
}],
PlotRange->{{-2,2},{-2,2},{0,3}}]
]
