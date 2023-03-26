import fungus
import std/unittest

adtEnum(Command):
  common:
    short: string
    long: string
    help: string
    required: bool
  StrCmd: string
  FloatCmd: float
  BoolCmd: bool
  IntCmd: int


proc init*(_: typedesc[StrCmd], short, long, help: string, required: bool): StrCmd =
  result = StrCmd.init("")
  result.short = short
  result.long = long
  result.help = help
  result.required = required

proc `==`(a, b: StrCmd): bool = adtEqual(a, b)

var a = Command StrCmd.init("h", "looong", "this does helpy things", true)
check StrCmd(a) == StrCmd.init("h", "looong", "this does helpy things", true)
check StrCmd(a) != StrCmd.init("h", "looong", "", true)
discard $StrCmd(a)

