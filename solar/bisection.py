import cv2
import numpy as np


def bisection_points(hull):
    N = 4
    a = 0.
    b = 1.
    n = len(hull)
    hull = np.asarray(hull, dtype=np.int32)  # convert tuple

    mid = a
    itr = 0

    max_iter = 50

    arc_length = cv2.arcLength(hull, True)

    while True:
        if n < N:
            b = mid
        else:
            a = mid

        mid = 0.5 * (a + b)
        itr += 1

        approx_points = cv2.approxPolyDP(hull, mid * arc_length, closed=True)

        n = len(approx_points)

        if not (n != N and itr < max_iter):
            return approx_points.astype(int)
