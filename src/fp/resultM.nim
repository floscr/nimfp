import results
import std/[
  sugar,
  strutils,
  osproc,
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

proc tapErr*[T, E](x: Result[T, E], errFn: E -> void): Result[T, E] =
  ## Applies side-effect `errFn` and returns orignal result `x`
  if x.isErr():
    errFn(x.error)
  x

proc bitap*[T, E](x: Result[T, E], errFn: E -> void, okFn: T -> void): Result[T, E] =
  ## Applies side-effect `errFn` or `okFn` and returns orignal result `x`
  if x.isOk():
    okFn(x.value)
  else:
    errFn(x.error)
  x

proc log*[T, E](x: Result[T, E], prefix = ""): Result[T, E] =
  discard x.tap((x: T) => echo(prefix & $x))
  x

proc sh*(cmd: string, opts = {poStdErrToStdOut}): Result[string, string] =
  ## Execute a shell command `cmd` and wrap it in an result
  let (res, exitCode) = execCmdEx(cmd, opts)
  if exitCode == 0:
    res
    .strip()
    .ok()
  else:
    res
    .strip()
    .err()

when isMainModule:
  type R = Result[int, string]
  type RS = Result[string, string]

  assert R.ok(1).fold((x: string) => 0, (x: int) => x) == 1
  assert R.err("Error").fold((x: string) => x, (x: int) => $x) == "Error"

  var mutTap: int
  assert R.ok(1).tap((x: int) => (mutTap = x)) == R.ok(1)
  assert mutTap == 1

  var mutTapErr: string
  assert R.err("Error").tapErr((x: string) => (mutTapErr = x)) == R.err("Error")
  assert mutTapErr == "Error"

  block testBiTap:
    var mutBitapOk: int
    var mutBitapErr: string
    assert R.ok(1).bitap((x: string) => (mutBitapErr = x), (x: int) => (mutBitapOk = x)) == R.ok(1)
    assert mutBitapOk == 1 and mutBitapErr == ""
    mutBitapOk = 0
    assert R.err("Error").bitap((x: string) => (mutBitapErr = x), (x: int) => (mutBitapOk = x)) == R.err("Error")
    assert mutBitapOk == 0 and mutBitapErr == "Error"

  block testSh:
    assert sh("true").isOk()
    assert sh("false").isErr()
    assert sh("ls --nonexistent").isErr() == true

  block testLog:
    assert R.ok(1).log("Logger Test (int): ") == R.ok(1)
    assert RS.ok("Log Test").log("Logger Test (string): ") == RS.ok("Log Test")
