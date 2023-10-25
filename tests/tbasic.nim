import fungus

type Shape* {.variant.} = object
  case _: ShapeKind
  of None: discard
  of Circle:
    x, y, r: int
  of Rectangle:
    x, y, w, h: int
  of Line:
    x1, y1, x2, y2: int

var a = Shape Circle.init(10, 10, 100)
a.to(Circle).r = 300
echo $a.to(Circle)
a = Shape Line.init(0, 0, 1, 1)

if (var myVar: Line) from a:
  inc myVar.x1
  echo myVar
elif (myOtherVar: Circle) from a:
  echo myOtherVar

match a:
of Circle as mut circ:
  circ.r = 1000
  echo circ
of Rectangle as rect:
  echo rect
of Line as (mut x1, _, x2, _):
  inc x1
  echo a.to(Line)
else: discard


if (myVar: Line) from a:
  echo myVar
