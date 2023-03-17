import fungus
import std/unittest


adtEnum(Option[T]):
  None
  Some: tuple[val: T]

proc isSome[T](opt: Option[T]): bool =
  match opt:
  of Some:
    true
  else:
    false

proc isNone[T](opt: Option[T]): bool = not opt.isSome

proc unsafeGet*[T](opt: Option[T]): T = opt.ValueData.val

var a = Option[int] Some.init(100)
check a.isSome()

match a:
of Some as (val, ):
  check val == 100
else:
  check false

a = Option[int] None[int].init()
check a.isNone()
