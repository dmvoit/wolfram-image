(* ::Package:: *)

BeginPackage[ "Package`"]


RamerDouglasPeucker::usage =
    "RamerDouglasPeucker[ points,threshold] Ramer-Douglas-Peucker algorithm."


Begin["`Private`"]


RamerDouglasPeucker[{p1_, p2_}, _] := {p1, p2}


RamerDouglasPeucker[points_, th_, loop_ : False] := Module[
  {p1, p2, perpendicular, dist, maxPos},
  p1 = First@points;
  p2 = Last@points;
  perpendicular = Normalize[p2 - p1] . RotationMatrix[90\[Degree]];
  dist = perpendicular . (# - p1)& /@ points // Abs;
  maxPos = First@Ordering[dist, -1];

  If[dist[[maxPos]] < th,
    {p1, p2},
    Join[
      RamerDouglasPeucker[points[[;; maxPos]], th],
      Rest@RamerDouglasPeucker[points[[maxPos ;;]], th]
    ]]
]


RamerDouglasPeucker[points_, th_, True] := Module[{
  p1, p2, maxSrt, maxMid, dm, maxDis, pointsL},
  (*loop*)
  (*find 2 points with highest distance*)
  dm = DistanceMatrix@points;
  maxDis = Max@dm;
  {maxSrt, maxMid} = Sort[Position[dm, maxDis][[1]]];

  (*make first point beginning of the list*)
  pointsL = RotateLeft[points, maxSrt - 1];
  {maxSrt, maxMid} = {1, maxMid - maxSrt + 1};
  {p1, p2} = pointsL[[{maxSrt, maxMid}]];

  If[maxDis < th,
    {p1, p2},
    Join[
      (*make first point beginning of the list*)
      RamerDouglasPeucker[pointsL[[;; maxMid]], th],
      Extract[{2 ;; -2}]@RamerDouglasPeucker[pointsL[[maxMid ;;]] // Append@First@pointsL, th]
    ]
  ]]


RamerDouglasPeucker[region_BoundaryMeshRegion, th_] :=
    RamerDouglasPeucker[MeshCoordinates@region, th, True] // Polygon // BoundaryMesh


End[]
EndPackage[]