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

type Commands = StrCmd or FloatCmd or BoolCmd or IntCmd


proc init*(T: typedesc[Commands], short, long, help: string, required: bool): T=
  result = T.init()
  result.short = short
  result.long = long
  result.help = help
  result.required = required

proc `==`(a, b: StrCmd): bool = adtEqual(a, b)
proc `==`(a, b: FloatCmd): bool = adtEqual(a, b)
proc `==`(a, b: BoolCmd): bool = adtEqual(a, b)
proc `==`(a, b: IntCmd): bool = adtEqual(a, b)

var a = Command StrCmd.init("h", "looong", "this does helpy things", true)
check StrCmd(a) == StrCmd.init("h", "looong", "this does helpy things", true)
check StrCmd(a) != StrCmd.init("h", "looong", "", true)
check IntCmd.init("hmm", "huh", "bleh", false) == IntCmd.init("hmm", "huh", "bleh", false)
discard $StrCmd(a)

