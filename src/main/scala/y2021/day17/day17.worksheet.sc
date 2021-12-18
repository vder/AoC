val (xmin, xmax, ymin, ymax) = (70, 96, -179, -124)
val ty = ymin.toInt + 1
val vyo = -ty;
vyo * (vyo + 1) / 2

val maxt = List(-2 * ymin + 1, xmax).max

var cnt = 0
val all = for {
  vy0 <- ymin to -ymin
  vx0 <- 0 to xmax
  t <- 1 to maxt
  y = vy0 * t - t * (t - 1) / 2
  x =
    if (t < vx0) vx0 * t - t * (t - 1) / 2
    else vx0 * (vx0 + 1) / 2
  if (ymin <= y && y <= ymax && xmin <= x && x <= xmax)
} yield (vx0, vy0)


all.distinct.size