import tbasic
import fungus

let a: Shape = Circle.init(0, 0, 1)

match a:
of Circle as a:
  echo $a
of Line:
  discard
else:
  discard


if (myVal: Circle) from a:
  discard
elif (var myVal: Line) from a:
  discard
