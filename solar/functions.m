(* ::Package:: *)

Package["functions`"]

(* Export functions *)
PackageExport["reverseYAxis"]
reverseYAxis::usage =
    "transform between python opencv and wolfram"

reverseYAxis[ySize_][xy_] := TransformationFunction[{
  {1, 0, 0}, {0, -1, ySize}, {0, 0, 1}}][xy]

PackageExport["CameraMatrix"]
CameraMatrix[img_] := Module[{height, width, center},
  {height, width} = ImageDimensions@img;
  center = {height, width} / 2;
  ({
    {height, 0, center[[1]]},
    {0, height, center[[2]]},
    {0, 0, 1}
  })]

PackageExport["pointsCompare"]
pointsCompare[img_, data_ : {{ls_List -> name_String}..}] :=
    Manipulate[
      Show[img,
        Graphics@{Red, opt /. {x_, y_} -> Point@{x, y}}
      ], {opt, data},
      ControlPlacement -> Top]
