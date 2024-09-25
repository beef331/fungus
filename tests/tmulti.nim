import fungus
adtEnum(Shape):
  None
  Circle: tuple[x, y, r: int]
  Rectangle: tuple[x, y, w, h: int]
  Line: tuple[x1, y1, x2, y2: int]

var a = Shape Circle.init(10, 10, 100)

match a:
of Circle as shape, Rectangle as shape:
  echo shape.x, " ", shape.y
of Line, None:
  echo "A different shape"

