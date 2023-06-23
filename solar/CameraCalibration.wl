(* ::Package:: *)
(* :Title: CameraCalibration *)
(* :Context: CameraCalibration` *)
(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.2 *)


BeginPackage["CameraCalibration`"];


DLTNormalizationMatrix::usage =
  "normalization transformation matrix function for points near the camera,
based on 'Multiple View Geometry in Computer Vision' Page 107,180";

ToHom::usage = "converts points to homogeneous";
FromHom::usage = "converts points from homogeneous";

CameraCalibration::usage = "find projection matrix based on image points";
DecomposeProjectionMatrix::usage = "splits projection matrix into K, R, t";
DecomposeHomography::usage = "decompose H into R, t";

PointsProjection::usage = "project 3D points on 2D image";
PointsScale::usage = "scale pixels from image size to another image size";

HomographyNormalization::usage = "normalization transformation matrix function for homography";
FindHomography::usage = "find projection function for homography based on image points and camera matrix";

SplitMatrix::usage = "split matrix into columns/rows";

CreateCube::usage = "create projected cube graphics primitives
based on provided projection function";

CreatePyramid::usage = "creates a pyramid in 3D space";

CreateCamera::usage = "position a camera 3d space based on
intrinsic and extrinsic parameters,";

GetViewDirection::usage = "get direction for ViewVector";

Begin["`Private`"];

DLTNormalizationMatrix[points_List] := Module[
  {mean, len, scale},
  len = Dimensions[points][[2]];
  mean = Mean@points;
  (* make mean centroid of all points *)
  (* scale average distance from the origin *)

  scale = Sqrt[len] / RootMeanSquare[ Norm /@ (points - Threaded@mean)];

  Composition[
    ScalingTransform[scale * Table[1, len]],
    TranslationTransform[-mean]
  ]
];

ToHom[ls : {Repeated[_?NumericQ, {2, 3}]}] := ls // Append@1;
ToHom[ls : {Repeated[{_?NumericQ}, {2, 3}]}] := ls // Append@{1};
ToHom[ls : {({_?NumericQ, _} | {_?NumericQ, _, _}) ..}] := ToHom /@ ls;

FromHom[ls : {Repeated[_?NumericQ, {2, 3}]}] := Most@ls / Last@ls;
FromHom[ls : {Repeated[{_?NumericQ}, {2, 3}]}] := Most@ls / First@Last@ls;
FromHom[ls : {({_?NumericQ, _, _} | {_?NumericQ, _, _, _}) ..}] := FromHom /@ ls;

CameraCalibration[imageP_, wordP_, "DLT"] := Module[
  {world = wordP, image = imageP, imgT, worldT,
    A, P, M},
  (* Normalize the points. *)
  imgT = DLTNormalizationMatrix@image;
  worldT = DLTNormalizationMatrix@world;
  image = imgT@image;
  world = worldT@world;

  (*build the homogeneous linear system*)
  M[{X_, Y_, Z_, x_, y_}] := Join @@@ {
    {-{X, Y, Z, 1}, {0, 0, 0, 0}, x * {X, Y, Z, 1}},
    {{0, 0, 0, 0}, -{X, Y, Z, 1}, y * {X, Y, Z, 1}}
  };
  A = M /@ ArrayFlatten[{{world, image}}];
  A = Flatten[A, 1];

  P = Partition[SingularValueDecomposition[A][[3, All, -1]], 4];
  (* denormalize *)
  -Inverse[imgT["TransformationMatrix"]].P.worldT["TransformationMatrix"]
];

SplitMatrix[mat_, span : {(_Span | _Integer) ..}] :=
    SplitMatrix[mat, span, "row"];

SplitMatrix[mat_, span : {(_Span | _Integer) ..}, "row"] := Switch[#,
  _Span, mat[[All, #]],
  _Integer, List /@ mat[[All, #]]
] & /@ span;
SplitMatrix[mat_, span : {(_Span | _Integer) ..}, "col"] := Switch[#,
  _Span, mat[[#, All]],
  _Integer, List@mat[[#, All]]
] & /@ span;

SplitMatrix[mat_] := SplitMatrix[mat, Dimensions[mat][[2]] // Range];
SplitMatrix[mat_, "col"] :=
    SplitMatrix[mat, Dimensions[mat][[1]] // Range, "col"];


DecomposeProjectionMatrix[mat_] := Module[
  {K, R, t, P = mat, H, h, r180},

  (* H=K.R  , h=-K.R.T *)
  {H, h} = SplitMatrix[P, {1 ;; 3, 4}];
  t = Inverse[-H] . h;

  (*also t=FromHom@NullSpace[P] *)
  {R, K} = QRDecomposition[Inverse[H]];
  (* rotate 180 deg to face the scene *)
  (*r180=RotationMatrix[180 Degree,{0,0,1}];*)
  K = Inverse[K];
  {K / K[[3, 3]], -R, t}
];

DecomposeHomography[H_] := Module[
  {h1, h2, h3, norm1, norm2, tnorm},

  {h1, h2, h3} = SplitMatrix@H;
  norm1 = Norm@h1;
  norm2 = Norm@h2;
  h1 = h1 / norm1 // Flatten;
  h2 = h2 / norm2 // Flatten;
  tnorm = (norm1 + norm2) / 2;

  {{h1, h2, Cross[h1, h2]} // Transpose, h3 / tnorm}
];

DecomposeHomography[H_, Hnorm_?MatrixQ] :=
 DecomposeHomography[Inverse[Hnorm] . H];
DecomposeHomography[H_, Hnorm_TransformationFunction] :=
 DecomposeHomography[Inverse@Hnorm["TransformationMatrix"] . H];

PointsProjection[K_, R_, t_] := Composition[
  TransformationFunction@K,
  TransformationFunction@ArrayFlatten@{{R, t}}];
PointsProjection[K_, {R_, t_}] := Composition[
  TransformationFunction@K,
  TransformationFunction@ArrayFlatten@{{R, t}}];


PointsScale[img_Image, target_ : (_Integer | {_Integer, _Integer})] :=
  ScalingTransform[img/target];
PointsScale[img_Image, target : (_Real | {_Real, _Real})] :=
 ScalingTransform[img*target];
PointsScale[{x_Integer, y_Integer},
  target_ : (_Integer | {_Integer, _Integer})] :=
 ScalingTransform[{x, y}/target];
PointsScale[{x_Integer, y_Integer},
  target : (_Real | {_Real, _Real})] :=
 ScalingTransform[{x, y}*target];


CreateCube[fn_, flip_ : False] := Module[{cubeList, dn, up},
  cubeList = {
    {0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {1, 1, 0},
    {0, 0, 1}, {1, 0, 1}, {0, 1, 1}, {1, 1, 1}(*vertical*)
    };
  If[flip, cubeList = cubeList*Threaded@{1, 1, -1}];
  {
    Subscript[dn, 1], Subscript[dn, 2], Subscript[dn, 3], Subscript[
    dn, 4],
    Subscript[up, 1], Subscript[up, 2], Subscript[up, 3], Subscript[
    up, 4]
    } = Flatten /@ SplitMatrix[fn@cubeList, "col"];

  {
   Green,
   Line[{Subscript[dn, 1], Subscript[dn, 2]}],
   Line[{Subscript[dn, 3], Subscript[dn, 4]}],
   Red,
   Line[{Subscript[dn, 1], Subscript[dn, 3]}],
   Line[{Subscript[dn, 2], Subscript[dn, 4]}],
   Blue,
   Line[{Subscript[dn, 1], Subscript[up, 1]}],
   Line[{Subscript[dn, 2], Subscript[up, 2]}],
   Line[{Subscript[dn, 3], Subscript[up, 3]}],
   Line[{Subscript[dn, 4], Subscript[up, 4]}],
   Yellow, Thin,
   Line[{Subscript[up, 1], Subscript[up, 2]}],
   Line[{Subscript[up, 3], Subscript[up, 4]}],
   Line[{Subscript[up, 1], Subscript[up, 3]}],
   Line[{Subscript[up, 2], Subscript[up, 4]}]
   }
  ];

HomographyNormalization[pointsImage_] := Composition[
   ScalingTransform[ 1/MeanDeviation@pointsImage],
   TranslationTransform[-Mean@pointsImage]
   ] // N;


FindHomography[pointsImage_, pointsWorld_, K_, "Solve"] :=
 Module[{image = pointsImage, world = pointsWorld, Knorm, A, H, Hnorm,
    Hsolve, h, hvec, M},

  Knorm = InverseFunction@TransformationFunction[K];
  image = Knorm@image;

  Hnorm = HomographyNormalization[image];
  image = Hnorm@image;

  M[{X_, Y_, Z_, x_, y_}] := Join @@@ {
     {-{X, Y, 1}, {0, 0, 0}, x*{X, Y, 1}},
     {{0, 0, 0}, -{X, Y, 1}, y*{X, Y, 1}}
     };

  hvec = Array[Subscript[h, ##] &, 9];

  A = M /@ ArrayFlatten[{{world, image}}] // Flatten[#, 1] &;
  Hsolve =
   Solve[And[A . # == 0, Norm[#] == 1], #] &@hvec // First // N;

  H = ArrayReshape[hvec, {3, 3}] /. Hsolve;

  PointsProjection[K, DecomposeHomography[H, Hnorm]]
  ];

FindHomography[pointsImage_, pointsWorld_, K_, "SVD"] :=
 Module[{image = pointsImage, world = pointsWorld,
   USV, H, M, A, Knorm, Hnorm},

  Knorm = InverseFunction@TransformationFunction[K];
  image = Knorm@image;

  Hnorm = HomographyNormalization[image];
  image = Hnorm@image;

  M[{X_, Y_, Z_, x_, y_}] := Join @@@ {
     {-{X, Y, 1}, {0, 0, 0}, x*{X, Y, 1}},
     {{0, 0, 0}, -{X, Y, 1}, y*{X, Y, 1}}
     };
  A = M /@ ArrayFlatten[{{world, image}}];
  A = Flatten[A, 1];

  USV = SingularValueDecomposition[A];
  H = Partition[USV[[3, All, -1]], 3];

  PointsProjection[K, DecomposeHomography[H, Hnorm]]
  ];

CreatePyramid[{{cx_} , {cy_} , {cz_}},
  {px_ : 1, py_ : 1}, h_ : 1]:=CreatePyramid[{cx, cy, cz}, {px, py}, h]
CreatePyramid[{cx_ : 0, cy_ : 0, cz_ : 0},
  {px_ : 1, py_ : 1}, h_ : 1] := Pyramid[{
  {cx - px, cy - py, cz + h},
  {cx - px, cy + py, cz + h},
  {cx + px, cy + py, cz + h},
  {cx + px, cy - py, cz + h},
  {cx, cy, cz}}
]


CreateCamera[{K_, R_, t_}, scale_ : 1] :=
    MapAt[GeometricTransformation[#, {Inverse@R, t}] &,
      {Opacity[0.6], CreatePyramid[{0, 0, 0},
        scale * {K[[1, 3]], K[[2, 3]]} / {K[[1, 1]], K[[2, 2]]}, scale],
        Red, Arrow@{{0, 0, 0}, {Det[R], 0, 0}},
        Green, Arrow@{{0, 0, 0}, {0, Det[R], 0}}
      },
      {{2}, {4}, {6}}
    ]

CreateCamera[img_Image, {K_, R_, t_}, scale_ : 1] :=
 Module[{cam, base, photo, texture},
  cam = CreateCamera[{K, R, t}, scale];
  base = cam[[2, 1, 1, 1 ;; 4]];
  texture = Texture@ImageReflect[
     Thumbnail[img, Tiny, Padding -> None], Left];
  photo = {
    texture,
    GeometricTransformation[
     Polygon[base,
      VertexTextureCoordinates -> RotateLeft@{{0, 0}, {1, 0}, {1, 1}, {0, 1}}],
     cam[[2, 2]]]};
  Append[cam, photo]
  ]


GetViewDirection[P_, {cx_, cy_}] := Module[{Rp, tp, x1, x2, x3},
  {Rp, tp} = SplitMatrix[P, {1 ;; 3, 4}];
  Solve[{cx, cy, 1} == Rp . {x1, x2, x3} + tp, {x1, x2, x3}][[1, All,
    2]]
  ]
GetViewDirection[P_, img_Image] :=
 GetViewDirection[P, ImageDimensions@img/2]


End[];

EndPackage[]