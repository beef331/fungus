import fungus
adtEnum(Letter):
  A
  B
  C
  D

var invokes = 0

proc choose(): Letter =
  inc invokes
  Letter D.init()

match choose():
of A: discard
of B: discard
of C: discard
of D: discard

assert invokes == 1
