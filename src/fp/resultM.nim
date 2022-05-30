import results
import std/[
  sugar,
]

export results

proc fold*[T0, E, T1](x: Result[T0, E], okFn: T0 -> T1, errFn: E -> T1): T1 =
  if x.isOk():
    okFn(x.value)
  else:
    errFn(x.error)

when isMainModule:
  type R = Result[int, string]

  assert R.ok(1).fold((x: int) => x, (x: string) => 0) == 1
  assert R.err("Error").fold((x: int) => $x, (x: string) => x) == "Error"
