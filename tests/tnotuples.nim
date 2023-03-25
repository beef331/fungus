import fungus
adtEnum(Data):
  None
  String: string
  Int: int
  Float: float
  Arr: array[3, int]
  Vector2: tuple[x, y: float32]


var a = Data Float.init(30d)
echo a.to(Float)
a = Vector2.init(100, 200)
echo a.to(Vector2)

proc doThing(f: float) = discard

match a:
of Vector2 as (x, y):
  echo x, " ", y
of Float as mut x:
  x.toInternal += 0.3 # Need `.toInternal` for some cases
  doThing(x)
else: discard

a = Int.init(100)

if (myInt: Int) from a:
  echo myInt
