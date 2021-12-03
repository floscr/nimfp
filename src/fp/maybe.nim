import sugar,
       strutils,
       classy,
       ./kleisli,
       ./function

{.experimental.}

type
  MaybeKind = enum
    okNone, okJust
  Maybe*[T] = ref object
    ## Maybe ADT
    case kind: MaybeKind
    of okJust:
      value: T
    else:
      discard

proc Just*[T](value: T): Maybe[T] =
  ## Constructs maybe object with value
  Maybe[T](kind: okJust, value: value)

proc None*[T](): Maybe[T] =
  ## Constructs empty maybe object
  Maybe[T](kind: okNone)

# Just helpers
proc just*[T](value: T): Maybe[T] = Just(value)
proc none*[T](value: T): Maybe[T] = None[T]()
proc none*(T: typedesc): Maybe[T] = None[T]()

proc notNil*[T](o: Maybe[T]): Maybe[T] =
  ## Maps nil object to none
  if o.kind == okJust and o.value.isNil:
    none(T)
  else:
    o

proc notEmpty*(o: Maybe[string]): Maybe[string] =
  ## Maps empty string to none
  if o.kind == okJust and (o.value.strip == ""):
    string.none
  else:
    o

proc maybe*[T](p: bool, v: T): Maybe[T] =
  ## Returns the boxed value of `v` if ``p == true`` or None
  if p: v.just
  else: T.none

proc maybeF*[T](p: bool, f: () -> T): Maybe[T] =
  ## Returns the boxed value of ``f()`` if ``p == true`` or None
  if p: f().just
  else: T.none

proc `==`*[T](x, y: Maybe[T]): bool =
  if x.isDefined and y.isDefined:
    x.value == y.value
  elif x.isEmpty and y.isEmpty:
    true
  else:
    false

proc isEmpty*[T](o: Maybe[T]): bool =
  ## Checks if `o` is empty
  o.kind == okNone

proc isDefined*[T](o: Maybe[T]): bool =
  ## Checks if `o` contains value
  not o.isEmpty

proc `$`*[T](o: Maybe[T]): string =
  ## Returns string representation of `o`
  if o.isDefined:

   "Just(" & $o.value & ")"
  else:
    "None"

proc map*[T,U](o: Maybe[T], f: T -> U): Maybe[U] =
  ## Returns maybe with result of applying f to the value of `o` if it exists
  if o.isDefined:
    f(o.value).just
  else:
    none(U)

proc flatMap*[T,U](o: Maybe[T], f: T -> Maybe[U]): Maybe[U] =
  ## Returns the result of applying `f` if `o` is defined, or none
  if o.isDefined: f(o.value) else: none(U)

proc join*[T](mmt: Maybe[Maybe[T]]): Maybe[T] =
  ## Flattens the maybe
  mmt.flatMap(id)

proc get*[T](o: Maybe[T]): T =
  ## Returns maybe's value if defined, or fail
  doAssert(o.isDefined, "Can't get Maybe's value")
  o.value

proc getOrElse*[T](o: Maybe[T], d: T): T =
  ## Returns maybe's value if defined, or `d`
  if o.isDefined: o.value else: d

proc getOrElse*[T](o: Maybe[T], f: void -> T): T =
  ## Returns maybe's value if defined, or the result of applying `f`
  if o.isDefined: o.value else: f()

proc orElse*[T](o: Maybe[T], d: Maybe[T]): Maybe[T] =
  ## Returns `o` if defined, or `d`
  if o.isDefined: o else: d

proc orElse*[T](o: Maybe[T], f: void -> Maybe[T]): Maybe[T] =
  ## Returns `o` if defined, or the result of applying `f`
  if o.isDefined: o else: f()

proc filter*[T](o: Maybe[T], p: T -> bool): Maybe[T] =
  ## Returns `o` if it is defined and the result of applying `p`
  ## to it's value is true
  if o.isDefined and p(o.value): o else: none(T)

proc map2*[T,U,V](t: Maybe[T], u: Maybe[U], f: (T, U) -> V): Maybe[V] =
  ## Returns the result of applying f to `t` and `u` value if they are both defined
  if t.isDefined and u.isDefined: f(t.value, u.value).just else: none(V)

proc map2F*[A, B, C](
  ma: Maybe[A],
  mb: () -> Maybe[B],
  f: (A, B) -> C
): Maybe[C] =
  ## Maps 2 `Maybe` values via `f`. Lazy in second argument.
  ma.flatMap((a: A) => mb().map((b: B) => f(a, b)))

proc zip*[T, U](t: Maybe[T], u: Maybe[U]): Maybe[(T, U)] =
  ## Returns the tuple of `t` and `u` values if they are both defined
  if t.isDefined and u.isDefined:
    (t.get, u.get).just
  else:
    none((T, U))

proc liftO*[T,U](f: T -> U): proc(o: Maybe[T]): Maybe[U] =
  ## Turns the function `f` of type `T -> U` into the function
  ## of type `Maybe[T] -> Maybe[U]`
  (o: Maybe[T]) => o.map((x: T) => f(x))

proc forEach*[T](xs: Maybe[T], f: T -> void): void =
  ## Applies `f` to the maybes value if it's defined
  if xs.isDefined:
    f(xs.value)

proc forAll*[T](xs: Maybe[T], f: T -> bool): bool =
  ## Returns `f` applied to the maybe's value or true
  if xs.isDefined:
    f(xs.value)
  else:
    true

proc traverse*[T, U](ts: seq[T], f: T -> Maybe[U]): Maybe[seq[U]] =
  ## Returns list of values of application of `f` to elements in `ts`
  ## if all the results are defined
  ##
  ## Example:
  ## .. code-block:: nim
  ##   traverse(@[1, 2, 3], (t: int) => (t - 1).just) == @[0, 1, 2].just
  ##
  ##   let f = (t: int) => (if (t < 3): t.just else: int.none)
  ##   traverse(@[1, 2, 3], f) == seq[int].none
  var acc = newSeq[U](ts.len)
  for i, t in ts:
    let mu = f(t)
    if mu.isDefined:
      acc[i] = mu.get
    else:
      return none(seq[U])
  return acc.just

proc asSeq*[T](o: Maybe[T]): seq[T] =
  if o.isDefined:
    @[o.get]
  else:
    @[]

template elemType*(v: Maybe): typedesc =
  ## Part of ``do notation`` contract
  type(v.get)

proc point*[A](v: A, t: typedesc[Maybe[A]]): Maybe[A] =
  v.just

instance KleisliInst, Maybe[_], exporting(_)

proc fold*[A,B](v: Maybe[A], ifNone: () -> B, ifJust: A -> B): B =
  ## Returns the result of applying `ifJust` to the  value if `v` is
  ## defined. Otherwise evaluates `ifNone`.
  if v.isDefined:
    ifJust(v.value)
  else:
    ifNone()

proc foldLeft*[A,B](v: Maybe[A], b: B, f: (B, A) -> B): B =
  if v.isDefined:
    f(b, v.value)
  else:
    b

proc foldRight*[A,B](v: Maybe[A], b: B, f: (A, B) -> B): B =
  if v.isDefined:
    f(v.value, b)
  else:
    b
