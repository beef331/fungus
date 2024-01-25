import fungus
adtEnum(Shape):
  None
  Circle: tuple[x, y, r: int]
  Rectangle: tuple[x, y, w, h: int]
  Line: tuple[x1, y1, x2, y2: int]

proc `==`(a, b: Shape): bool = adtEqual(a, b)

var a = Shape Circle.init(10, 10, 100)
a.to(Circle).r = 300
assert a.to(Circle) == Circle.init(10, 10, 300)
a = Shape Line.init(0, 0, 1, 1)

if (var myVar: Line) from a:
  inc myVar.x1
  assert myVar == Line.init(1, 0, 1, 1)
elif (myOtherVar: Circle) from a:
  doAssert false

match a:
of Circle as mut circ:
  circ.r = 1000
  echo circ
of Rectangle as rect:
  echo rect
of Line as (mut x1, _, x2, _):
  inc x1
  assert x1 == 2
  assert a.to(Line) == Line.init(2, 0, 1, 1)
else: discard


if (myVar: Line) from a:
  assert myVar == Line.init(2, 0, 1, 1)


proc doThing(shp: Shape): string =
  match shp:
  of Circle as c, Rectangle as c:
    $c.x
  else:
    ""

a = Circle.init(10, 20, 30)
assert doThing(a) == "10"
a = Rectangle.init(10, 20, 30, 40)
assert doThing(a) == "10"
