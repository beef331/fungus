import fungus
import std/macros

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

var a = Command StrCmd.init("", "h", "this does helpy things", true)
echo a

