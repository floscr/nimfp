import std/[
  options,
  sugar,
]

proc fold*[A,B](v: Option[A], ifNone: () -> B, ifSome: A -> B): auto =
  ## Returns the result of applying `ifSome` to the  value if `v` is
  ## defined. Otherwise evaluates `ifNone`.
  if v.isSome():
    ifSome(v.unsafeGet())
  else:
    ifNone()

when isMainModule:
  echo "Some".some.fold(
    () => "None",
    x => x,
  )
