import options as stdOption
import std/[
  sugar,
  strutils,
]

export stdOption.Option
export stdOption.option
export stdOption.some, stdOption.none
export stdOption.isSome, stdOption.isNone
export stdOption.map, stdOption.filter, stdOption.flatMap
export stdOption.`==`
export stdOption.`$`

proc isEmpty*[T](x: Option[T]): bool =
  ## Checks if `x` is empty
  x.isNone()

proc isDefined*[T](x: Option[T]): bool =
  ## Checks if `x` contains value
  x.isSome()

proc notEmpty*(x: Option[string]): Option[string] =
  ## Maps empty string to none
  x.filter(y => y.strip() != "")

proc asSeq*[T](o: Option[T]): seq[T] =
  if o.isDefined():
    @[o.get()]
  else:
    @[]

proc fold*[A,B](x: Option[A], ifNone: () -> B, ifSome: A -> B): auto =
  ## Returns the result of applying `ifSome` to the  value if `x` is
  ## defined. Otherwise evaluates `ifNone`.
  if x.isSome():
    ifSome(x.unsafeGet())
  else:
    ifNone()

proc getOrElse*[T](x: Option[T], fallback: T): T =
  ## Returns maybe's value if defined, or `fallback`
  x.get(fallback)

proc getOrElse*[T](x: Option[T], fallback: () -> T): T =
  ## Returns maybe's value if defined, or `fallback`
  x.fold(fallback, y => y)

proc orElse*[T](x: Option[T], fallback: Option[T]): Option[T] =
  ## Returns `x` if defined, or `fallback`
  if x.isSome(): x else: fallback

proc orElse*[T](o: Option[T], fallback: () -> Option[T]): Option[T] =
  ## Returns `x` if defined, or the result of applying `fallback`
  if o.isSome(): o else: fallback()

proc zip*[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
  ## Returns the tuple of `a` and `b` values if they are both defined
  if a.isDefined() and b.isDefined():
    (a.get(), b.get()).some()
  else:
    none((A, B))

proc tap*[T](xs: Option[T], f: T -> void): void =
  ## Applies side-effect `f`
  if xs.isDefined():
    f(xs.unsafeGet())

proc traverse*[T, U](xs: seq[T], f: T -> Option[U]): Option[seq[U]] =
  ## Returns list of values of application of `f` to elements in `xs`
  ## if all the results are defined
  ##
  ## Example:
  ## .. code-block:: nim
  ##   traverse(@[1, 2, 3], (t: int) => (t - 1).just) == @[0, 1, 2].just
  ##
  ##   let f = (t: int) => (if (t < 3): t.just else: int.nothing)
  ##   traverse(@[1, 2, 3], f) == seq[int].nothing
  var acc = newSeq[U](xs.len)
  for i, t in xs:
    let mu = f(t)
    if mu.isDefined():
      acc[i] = mu.get()
    else:
      return none(seq[U])
  return acc.some()

when isMainModule:
  assert "Some".some.fold(() => "None", x => x) == "Some"
  # asSeq
  assert "Value".some().asSeq() == @["Value"]
  assert none(string).asSeq() == @[]
  # noTEmpty
  assert "   ".some.notEmpty() == none(string)
  # getOrElse
  assert some("Value").getOrElse("Fallback") == "Value"
  assert some("Value").getOrElse(() => "Fallback") == "Value"
  assert none(string).getOrElse("Fallback") == "Fallback"
  assert none(string).getOrElse(() => "Fallback") == "Fallback"
  # orElse
  assert none(string).orElse(some("Fallback")) == some("Fallback")
  assert none(string).orElse(() => some("Fallback")) == some("Fallback")
  # zip
  assert some("A").zip(some("B")) == some(("A", "B"))
  assert some("A").zip(none(string)) == none((string, string))
  # tap
  var mutVal: string
  some("Value").tap(proc (x: string): void = mutVal = x)
  assert mutVal == "Value"
  # traverse
  let a = @[1, 2, 3]
  let f1 = (t: int) => (t - 1).some()
  assert traverse(a, f1) == @[0, 1, 2].some()
  let f2 = (t: int) => (if (t < 3): t.some() else: int.none())
  assert traverse(a, f2) == seq[int].none()
