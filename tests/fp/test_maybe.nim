import ../../src/fp/maybe, unittest, sugar

suite "Maybe ADT":

  test "Basic functions":
    let s = "test".just
    let n = int.none

    check: $s == "Just(test)"
    check: $n == "None"
    check: s.isEmpty == false
    check: s.isDefined == true
    check: n.isDefined == false
    check: n.isEmpty == true
    check: s == Just("test")
    check: s != string.none
    check: n == None[int]()
    check: nil.cstring.just.notNil == cstring.none
    check: " 123 ".just.notEmpty == " 123 ".just
    check: "  ".just.notEmpty == "".none
    check: "123".none.notEmpty == "".none

    check: (2 < 3).maybeF(() => true).getOrElse(false)
    check: (2 < 3).maybe(true).getOrElse(false)

    check: s.get == "test"
    expect(AssertionError): discard n.get == 10

    check: 1.point(Maybe) == 1.just

  test "Map":
    let f = (x: int) => $x
    check: 100500.just.map(f) == just("100500")
    check: 100500.none.map(f) == string.none

    check: 100.just.map2("Test".just, (x, y) => y & $x) == "Test100".just
    check: 100.just.map2("Test".none, (x, y) => y & $x) == "".none

    check: 100.just.map2F(() => "Test".just, (x, y) => y & $x) == "Test100".just
    check: 100.just.map2F(() => "Test".none, (x, y) => y & $x) == "".none

    proc badMaybe(): Maybe[int] = raise newException(Exception, "Not lazy!")
    check: int.none.map2F(badMaybe, (x, y) => $x) == string.none

    check: "x".just.map(v => "\"" & v & "\"").getOrElse("y") == "\"x\""
    check: "x".none.map(v => "\"" & v & "\"").getOrElse("y") == "y"

  test "Flat map":
    let f = (x: int) => just(x * 2)
    check: 2.just.flatMap(f) == just(4)
    check: 2.none.flatMap(f) == none(4)

  test "Join":
    check: 2.just.just.join == 2.just
    check: int.none.just.join == int.none
    check: Maybe[int].none.join == int.none

  test "Getters":
    check: 2.just.getOrElse(3) == 2
    check: 2.none.getOrElse(3) == 3

    check: 2.just.getOrElse(() => 4) == 2
    check: 2.none.getOrElse(() => 4) == 4

    check: 2.just.orElse(3.just) == 2.just
    check: 2.none.orElse(3.just) == 3.just
    
    check: 2.just.orElse(() => 4.just) == 2.just
    check: 2.none.orElse(() => 4.just) == 4.just

  test "Filter":
    let x = "123".just
    let y = "12345".just
    let n = "".none
    let p = (x: string) => x.len > 3
    proc `!`[T](f: T -> bool): T -> bool = (v: T) => not f(v)

    check: x.filter(p) == n
    check: x.filter(!p) == x
    check: y.filter(p) == y
    check: y.filter(!p) == n
    check: n.filter(p) == n
    check: n.filter(!p) == n

  test "Traverse":
    let a = @[1, 2, 3]

    let f1 = (t: int) => (t - 1).just
    check: traverse(a, f1) == @[0, 1, 2].just

    let f2 = (t: int) => (if (t < 3): t.just else: int.none)
    check: traverse(a, f2) == seq[int].none

  test "Misc":
    check: ((x: int) => "Value " & $x).liftO()(1.just) == "Value 1".just
    var b = true
    false.just.forEach((v: bool) => (b = v))
    check: b == false
    true.just.forEach((v: bool) => (b = v))
    check: b == true
    false.none.forEach((v: bool) => (b = v))
    check: b == true
    check: true.just.forAll(v => v) == true
    check: false.just.forAll(v => v) == false
    check: false.none.forAll(v => v) == true

    check: 1.just.zip("foo".just) == (1, "foo").just
    check: 1.just.zip(string.none) == (int, string).none

    check: 1.just.asSeq == @[1]
    check: int.none.asSeq == newSeq[int]()

    check: int.none.fold(() => "", v => $v) == ""
    check: 1.just.fold(() => "", v => $v) == "1"

  test "Kleisli":
    let f = (v: int) => (v + 1).just
    let g = (v: int) => (v * 100).just
    check: 4.just >>= (f >=> g) == 500.just

