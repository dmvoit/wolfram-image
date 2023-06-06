convert from points back to mesh
```wl
MeshRegion[
reverseYAxis[224]/@MeshCoordinates@tf["mesh_hull"]//Polygon]
```
### Measure line

```python

cv2.arcLength(points, closed=False)
```
is same as
```wl
ArcLength@Line@points
```
### Measure closed loop

```python
cv2.arcLength(points, closed=True)
```
is same as

```wl
Perimeter@tf["mesh_hull"]
```