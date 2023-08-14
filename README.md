# fungus

Rust-like tuple enums.
Give me a macro and a fulcrum to rest it on and I'll likely make an obnoxious macro.

## What does it do?

Does what it says on the tin... generates Rust-like ADT enums, those weird ones with fields.
Consider the following idiomatic Nim code:
```nim
type
  ShapeKind = enum
    NoneKind, CircleKind, RectangleKind, LineKind

  Shape = object
    case kind: ShapeKind
    of LineKind:
      lx1, ly1, lx2, ly2: int
    of RectangleKind:
      rx, ry, rw, rh: int
    of CircleKind:
      cRadius, cx, cy: int
    of NoneKind:
      discard
```

This is a bit messy due to rightly not being able to share field names(Nim variants are great for specific things, for other things they are a bit in the way).
Other languages get around it doing something similar to what this library does, statically enforcing unpacking each kind.
What one writes with fungus instead is:
```nim
import fungus
adtEnum(Shape): # or adtEnum(ref Shape) generate ref object type
  None
  Circle: tuple[x, y, r: int]
  Rectangle: tuple[x, y, w, h: int]
  Line: tuple[x1, y1, x2, y2: int]
```

This generates a static type per each branch, and also a bunch of accessor procedures.
The type definitions look like:

```nim
type
  ShapeKind = enum
    NoneKind, CircleKind, RectangleKind, LineKind
  Shape = object
    case kind: ShapeKind
    of LineKind:
      LineData:
        tuple[x1, y1, x2, y2: int]
    of RectangleKind:
      RectangleData:
        tuple[x, y, w, h: int]
    of CircleKind:
      CircleData:
        tuple[x, y, r: int]
    of NoneKind:
      nil

  None = distinct Shape
  Circle = distinct Shape
  Rectangle = distinct Shape
  Line = distinct Shape
```

Which means the following is possible:
```nim
var a = Shape Circle.init(10, 10, 100)
a.to(Circle).r = 300
echo a.to(Circle)
a = Shape Line.init(0, 0, 1, 1)
```

This doesnt solve much altogether, which is what the `match` and `from` macros are for.

```nim
if (var myVar: Line) from a: # notice `var` this is a mutable match
  inc myVar.x1
  echo myVar
elif (myOtherVar: Circle) from a:
  echo myOtherVar
```

What this macro expands into is `a.kind == CircleKind and (let myOtherVar = Circle(a); true)`.
This allows you to work similarly to Rust's `if let` construct, but this composes better(it should not bug out as easily :P ).
Now to do some primitive pattern matching!

```nim
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
```

The `match` macro is very very basic pattern matching.
It is not too disimilar to the `from` macro.
As one can see though it allows a `as mut name` to introduce a mutable name to the matching variable.
There is also the `as name` which is an immutable variable match.
Finally there is a `tuple` match which is `(x, y, z, w)`, much like the other match `mut` can prefix a name to indicate it is a mutable match.
Tuple unpacking works just like normally for this matching, although one can match as few positional fields as they want.

### Non tuple types

```nim
import fungus
adtEnum(Data):
  None
  String: string
  Int: int
  Float: float
  Arr: array[3, int]
  Vector2: tuple[x, y: float32]
```

Is valid code, there exists `toInternal` converts to implicitly convert, but some operations will require explicit invocation.
For example:
```nim
proc doThing(f: float) = discard
match a:
of Vector2 as (x, y):
  echo x, " ", y
of Float as mut x:
  x.toInternal += 0.3 # Need `.toInternal` for some cases
  doThing(x)
else: discard
```

### Generics?!
Yes this also supports some subset of generics.

```nim
import fungus
adtEnum(LinkedList[T]):
  Nil
  Node: tuple[n: ref LinkedList[T], val: T]

proc newNode[T](val: T): Node[T] =
  result = Node[T].init((ref LinkedList[T])(), val)
  result.n[] = LinkedList[T] Nil[T].init()

proc prepend[T](node: LinkedList[T], val: T): LinkedList[T] =
  result = LinkedList[T] Node[T].init(new LinkedList[T], val)
  Node[T](result).n[] = node

proc len[T](list: LinkedList[T]): int =
  var theList = list
  while (node: Node) from theList:
    inc result
    theList = node.n[]

proc `$`[T](list: LinkedList[T]): string =
  var theList = list
  result = "["
  while (node: Node) from theList:
    result.add $node.val
    result.add ", "
    theList = node.n[]

  if result.len > 1:
    result.setLen(result.len - 2)
  result.add "]"

var myList = LinkedList[int] newNode(10)
myList = myList.prepend(11)
myList = myList.prepend(12)
myList = myList.prepend(13)
echo myList
echo myList.len

var myList2 = LinkedList[string] newNode("world")
myList2 = myList2.prepend "cruel"
myList2 = myList2.prepend "hello"
echo myList2
echo myList2.len
```

The above *just works*(thanks to the Rust book for a small example of linked lists using an ADT).


## Implementing custom `==`

Fungus has a `adtEqual` procedure which can be used to invoke default comparisons for a type.
This means kind is checked then the tuples are compared.
An example for thel linked list looks like:
```nim
proc `==`[T](a, b: LinkedList[T]): bool = adtEqual(a, b)
```

## Implementing custom `$`

Fungus has a `$` procedure defined as a generic, which means you can overload it for your own type.
Otherwise it should call this one.
```nim
proc `$`[T](nl: Nil[T]): string = "nil"
```
