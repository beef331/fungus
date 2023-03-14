import fungus
adtEnum(Shape):
  None
  Circle: tuple[x, y, r: int]
  Rectangle: tuple[x, y, w, h: int]
  Line: tuple[x1, y1, x2, y2: int]

var a = Shape Circle.init(10, 10, 100)
a.to(Circle).r = 300
echo a.to(Circle)
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
