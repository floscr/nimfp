import results
import std/[
  sugar,
]

export results

proc fold*[T0, E, T1](x: Result[T0, E], errFn: E -> T1, okFn: T0 -> T1): T1 =
  if x.isOk():
    okFn(x.value)
  else:
    errFn(x.error)

proc tap*[T, E](x: Result[T, E], okFn: T -> void): Result[T, E] =
  ## Applies side-effect `okFn` and returns orignal result `x`
  if x.isOk():
    okFn(x.value)
  x

proc bitap*[T, E](x: Result[T, E], errFn: E -> void, okFn: T -> void): Result[T, E] =
  ## Applies side-effect `errFn` or `okFn` and returns orignal result `x`
  if x.isOk():
    okFn(x.value)
  else:
    errFn(x.error)
  x

when isMainModule:
  type R = Result[int, string]

  assert R.ok(1).fold((x: string) => 0, (x: int) => x) == 1
  assert R.err("Error").fold((x: string) => x, (x: int) => $x) == "Error"

  var mutTap: int
  assert R.ok(1).tap((x: int) => (mutTap = x)) == R.ok(1)
  assert mutTap == 1

  var mutTapOk: int
  var mutTapErr: string
  assert R.ok(1).bitap((x: string) => (mutTapErr = x), (x: int) => (mutTapOk = x)) == R.ok(1)
  assert mutTapOk == 1 and mutTapErr == ""
  mutTapOk = 0
  assert R.err("Error").bitap((x: string) => (mutTapErr = x), (x: int) => (mutTapOk = x)) == R.err("Error")
  assert mutTapOk == 0 and mutTapErr == "Error"
