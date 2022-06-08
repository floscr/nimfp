import ./either,
       sugar,
       macros

export either

type Try*[A] = EitherE[A]
  ## The type representing either exception or successfully
  ## computed value

proc success*[A](v: A): Try[A] =
  ## Create the successfully computed value
  v.rightE

proc failure*[A](e: ref Exception, t: typedesc[A]): Try[A] =
  ## Create the result of failed computation
  e.left(A)

proc failure*[A](msg: string, t: typedesc[A]): Try[A] {.inline.} =
  ## Create the result of failed computation
  try:
    raise newException(Exception, msg)
  except:
    result = getCurrentException().failure(A)

proc fromEither*[E,A](v: Either[E,A]): Try[A] {.inline.}=
  ## Conversion from ``Either[E,A]`` type
  when E is ref Exception:
    v
  elif compiles(v.getLeft().`$`):
    if v.isLeft:
      v.getLeft().`$`.failure(A)
    else:
      v.get.success
  else:
    {.error: "Can't cast Either's left value to string".}

template tryMImpl(body: typed): untyped =
  when compiles((block:
    tryE do() -> auto:
      body
  )):
    tryE do() -> auto:
      body
  else:
    tryE do() -> auto:
      body
      ()

macro tryM*(body: untyped): untyped =
  ## Executes `body` and places it's result in the ``Try`` container
  var b = if body.kind == nnkDo: body[^1] else: body
  result = quote do:
    tryMImpl((block:
      `b`
    ))

proc isSuccess*[A](v: Try[A]): bool =
  ## Returns true if `v` contains value
  v.isRight

proc isFailure*[A](v: Try[A]): bool =
  ## Returns true if `v` contains exception
  v.isLeft

proc getError*[A](v: Try[A]): ref Exception =
  ## Returns the exception object
  v.getLeft

proc getErrorMessage*[A](v: Try[A]): string =
  ## Returns the exception message
  v.getError.msg

proc filter*[A](v: Try[A], p: A -> bool, error: string): Try[A] =
  ## Returns `v` if it is defined and the result of applying `p`to it's value is true
  ## Otherwise apply the `error` to `v`
  if v.isSuccess() and p(v.get()):
    v
  else:
    failure(error, A)

proc fold*[A,B](v: Try[A], ifFailure: ref Exception -> B, ifSuccess: A -> B): B =
  ## Applies `ifFailure` if `v` is left, or `ifSuccess` if `v` is right
  if v.isFailure():
    ifFailure(v.getError())
  else:
    ifSuccess(v.get())

proc tap*[A](v: Try[A], ifSuccess: A -> void): Try[A] =
  ## Applies side-effect `ifSuccess` and returns orignal result `v`
  if v.isSuccess():
    ifSuccess(v.get())
  v

proc tapErr*[A](v: Try[A], ifFailure: ref Exception -> void): Try[A] =
  ## Applies side-effect `ifFailure` and returns orignal result `v`
  if v.isErr():
    ifFailure(v.getError())
  v

proc bitap*[A](v: Try[A], ifFailure: ref Exception -> void, ifSuccess: A -> void): Try[A] =
  ## Applies side-effect `ifFailure` or `ifSuccess` and returns orignal result `v`
  if v.isSuccess():
    ifSuccess(v.get())
  else:
    ifFailure(v.getError())
  v

proc getErrorStack*[A](v: Try[A]): string =
  ## Returns the exception stack trace
  v.getError.getStackTrace

proc recover*[A](v: Try[A], f: ref Exception -> A): Try[A] =
  ## Returns the result of calling `f` if `v` contains the exception.
  ## Otherwise returns `v`
  if v.isFailure:
    f(v.getError).success
  else:
    v

proc recoverWith*[A](v: Try[A], f: ref Exception -> Try[A]): Try[A] =
  ## Returns the result of calling `f` if `v` contains the exception.
  ## Otherwise returns `v`
  if v.isFailure:
    f(v.getError)
  else:
    v

proc mapErrorMessage*[A](v: Try[A], f: string -> string): Try[A] =
  ## Transforms the error message using `f`
  if v.isFailure:
    var errCopy: ref Exception
    deepCopy(errCopy, v.getError)
    errCopy.msg = f(errCopy.msg)
    errCopy.failure(A)
  else:
    v

when isMainModule:
  import strutils

  block fold:
    assert tryET("F".parseInt()).fold((x: ref Exception) => "Fail", (x: int) => "Suc") == "Fail"
    assert "Suc".success().fold((x: ref Exception) => x.msg, (x: string) => x) == "Suc"
    assert "Fail".failure(string).fold((x: ref Exception) => x.msg, (x: string) => x) == "Fail"

  block filter:
    assert success(1).filter((x: int) => x == 1, "Fail").get() == 1
    assert success(1).filter((x: int) => x == 0, "Fail").getErrorMessage() == "Fail"

  block tap:
    var mutBitapOk: int
    var mutBitapErr: string
    assert tryET("1".parseInt()).bitap((x: ref Exception) => (mutBitapErr = x.msg), (x: int) => (mutBitapOk = x)) == 1.success()
    assert mutBitapOk == 1 and mutBitapErr == ""
    mutBitapOk = 0
    assert tryET("Fail".parseInt()).bitap((x: ref Exception) => (mutBitapErr = "Error"), (x: int) => (mutBitapOk = x)).isFailure()
    assert mutBitapOk == 0 and mutBitapErr == "Error"
