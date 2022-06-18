import std/[
  sugar,
  sequtils,
]
import ./option.nim
import ./intM

proc findOption[T](xs: seq[T], p: T -> bool): Option[T] =
  for x in xs:
    if p(x):
      return some(x)
  return none(T)

func partition*[T](xs: seq[T], cond: T -> bool): tuple[ifTrue: seq[T], ifFalse: seq[T]] =
  xs.foldl(
    if cond(b): (a[0] & b, a[1]) else: (a[0], a[1] & b),
    (newSeq[T](), newSeq[T]()),
  )

when isMainModule:
  block testPartition:
    assert @[1, 2, 3].partition((x: int) => x.isOdd()) == (@[1, 3], @[2])
  block testFindOption:
    assert @[1, 2, 3].findOption((x: int) => x == 1) == 1.some()
    assert @[1, 2, 3].findOption((x: int) => x == 4) == int.none()
