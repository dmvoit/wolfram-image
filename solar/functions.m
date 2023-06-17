(* ::Package:: *)

Package["functions`"]

(* Export functions *)
PackageExport["reverseYAxis"]
reverseYAxis::usage =
    "transform between python opencv and wolfram"

reverseYAxis[ySize_][xy_] := TransformationFunction[{
  {1, 0, 0}, {0, -1, ySize}, {0, 0, 1}}][xy]

PackageExport["CameraMatrix"]
CameraMatrix[{width_, height_}] := {
  {height, 0, height / 2},
  {0, height, width / 2},
  {0, 0, 1}
}

CameraMatrix[img_Image] := CameraMatrix[ImageDimensions@img]

PackageExport["pointsCompare"]
pointsCompare[img_, data_ : {{ls_List -> name_String}..}] :=
    Manipulate[
      Show[img,
        Graphics@{Red, opt /. {x_, y_} -> Point@{x, y}}
      ], {opt, data},
      ControlPlacement -> Top]
