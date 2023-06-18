(* ::Package:: *)

Package["functions`"]

(* Export functions *)
PackageExport["reverseYAxis"]
reverseYAxis::usage =
    "transform between python opencv and wolfram"

reverseYAxis[ySize_][xy_] := TransformationFunction[{
  {1, 0, 0}, {0, -1, ySize}, {0, 0, 1}}][xy]

PackageExport["CameraMatrix"]
CameraMatrix::usage = "CameraMatrix approximation"
CameraMatrix[{width_, height_}, scale : 1] := {
  {scale * height, 0, width / 2},
  {0, scale * height, height / 2},
  {0, 0, 1}
}

CameraMatrix[img_Image, scale : 1] := CameraMatrix[ImageDimensions@img, scale]

CameraMatrixOrientation[K_] := If[
  K[[1, 3]]/K[[2, 3]] > 1,
  "Landscape", "Portrait"]

CameraMatrixFlip[{{fx_, s_, cx_}, {_, fy_, cy_}, {__}}] := {
  {fy, s, cy},
  {0, fx, cx},
  {0, 0, 1}}

PackageExport["pointsCompare"]
pointsCompare[img_, data_ : {{ls_List -> name_String}..}] :=
    Manipulate[
      Show[img,
        Graphics@{Red, opt /. {x_, y_} -> Point@{x, y}}
      ], {opt, data},
      ControlPlacement -> Top]
