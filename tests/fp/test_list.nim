import ../../src/fp/list, ../../src/fp/maybe, unittest, sugar, boost/types, sequtils, algorithm

suite "List ADT":

  test "Initialization and conversion":
    let lst = [1, 2, 3, 4, 5].asList

    check: lst.head == 1
    check: lst.headMaybe == just(1)
    check: lst.tail.asSeq == @[2, 3, 4, 5]
    check: Nil[int]().headMaybe == 1.nothing
    check: $lst == "List(1, 2, 3, 4, 5)"
    check: 1^^2^^3^^4^^5^^Nil[int]() == lst
    check: ["a", "b"].asList != ["a", "b", "c"].asList

    check: 1.point(List) == [1].asList

  test "Fold operations":
    let lst = toSeq(1..4).asList

    check: lst.foldLeft(0, (x, y) => x + y) == 10
    check: lst.foldLeft(1, (x, y) => x * y) == 24

    check: lst.foldRight(0, (x, y) => x + y) == 10
    check: lst.foldRight(1, (x, y) => x * y) == 24

    check: lst.foldRightF(() => 0, (x: int, y: () -> int) => x + y()) == 10
    check: lst.foldRightF(() => 1, (x: int, y: () -> int) => x * y()) == 24

    proc badAcc(): int = raise newException(Exception, "Not lazy!")
    check: lst.foldRightF(badAcc, (x: int, y: () -> int) => x) == 1

    check: Nil[int]().foldRight(100, (x, y) => x + y) == 100

  test "Unfold operations":
    proc divmod10(n: int): Maybe[(int, int)] =
      if n == 0: nothing((int,int))
      else: just(( (n mod 10).int, n div 10))

    check: unfoldLeft(divmod10,12301230) == [1,2,3,0,1,2,3,0].asList
    check: unfoldRight(divmod10,12301230) == [0,3,2,1,0,3,2,1].asList

    proc unconsString(s: string): Maybe[(char, string)] =
      if s == "": nothing((char, string))
      else: just((s[0], s[1..^1]))

    check: unfoldLeft(unconsString,"Success !") == ['!', ' ', 's', 's', 'e', 'c', 'c', 'u', 'S'].asList
    check: unfoldRight(unconsString,"Success !") == ['S', 'u', 'c', 'c', 'e', 's', 's', ' ', '!'].asList

    var global_count: int = 0
    proc divmod10_count(n: int): Maybe[(int, int)] =
      inc global_count
      if n == 0: nothing((int,int))
      else: just(( (n mod 10).int, n div 10))

    let _ = unfoldLeft(divmod10_count,12301230)
    check: global_count == 9
    let _ = unfoldRight(divmod10_count,12301230)
    check: global_count == 18

  test "Transformations":
    check: @[1, 2, 3].asList.traverse((x: int) => x.just) == @[1, 2, 3].asList.just
    check: @[1, 2, 3].asList.traverseU((x: int) => x.just) == ().just
    check: @[1, 2, 3].asList.traverse((x: int) => (if x > 2: x.nothing else: x.just)) == List[int].nothing
    check: @[1, 2, 3].asList.traverseU((x: int) => (if x > 2: x.nothing else: x.just)) == Unit.nothing

    # traverse should not call f after the first Nothing
    var cnt = 0
    let res = asList(1, 2, 3).traverse do (x: int) -> auto:
      inc cnt
      if x != 2: x.just
      else: int.nothing
    check: res == List[int].nothing
    check: cnt == 2

    check: @[1.just, 2.just, 3.just].asList.sequence == @[1, 2, 3].asList.just
    check: @[1.just, 2.just, 3.just].asList.sequenceU == ().just
    check: @[1.just, 2.nothing, 3.just].asList.sequence == List[int].nothing
    check: @[1.just, 2.nothing, 3.just].asList.sequenceU == Unit.nothing

  test "Drop operations":
    let lst = toSeq(1..100).asList

    check: lst.drop(99) == [100].asList
    check: lst.dropWhile((x: int) => x < 100) == [100].asList

  test "Misc functions":
    let lst = toSeq('a'..'z').asList
    check: lst.dup == lst
    check: lst.reverse == toSeq('a'..'z').reversed.asList
    check: asList(2, 4, 6, 8).forAll((x: int) => x mod 2 == 0) == true
    check: asList(2, 4, 6, 9).forAll((x: int) => x mod 2 == 0) == false
    check: asList(1, 2, 3).zip(asList('a', 'b', 'c')) == asList((1, 'a'), (2, 'b'), (3, 'c'))
    check: asList((1, 'a'), (2, 'b'), (3, 'c')).unzip == (asList(1, 2, 3), asList('a', 'b', 'c'))
    check: [1,2,3].asList.zipWithIndex(-1) == [(1, -1), (2, 0), (3, 1)].asList

    check: asList(1, 2, 3).contains(2)
    check: not asList(1, 2, 3).contains(4)

    check: asList((1, 'a'), (2, 'b'), (2, 'c')).lookup(1) == 'a'.just
    check: asList((1, 'a'), (2, 'b'), (2, 'c')).lookup(2) == 'b'.just
    check: asList((1, 'a'), (2, 'b'), (2, 'c')).lookup(3) == char.nothing

    check: asList(1, 2, 3).span((i: int) => i <= 2) == (asList(1, 2), asList(3))
    check: asList(1, 2, 3).span((i: int) => i mod 2 == 1) == (asList(1), asList(2, 3))
    check: asList(1, 2, 3).span((i: int) => true) == (asList(1, 2, 3), Nil[int]())

    check: asList(1, 2, 3).partition((i: int) => i > 2) == (asList(3), asList(1, 2))
    check: asList(1, 2, 3).partition((i: int) => i == 2) == (asList(2), asList(1, 3))
    check: asList(1, 2, 3).partition((i: int) => i > 4) == (Nil[int](), asList(1, 2, 3))
    check: asList(1, 2, 3).partition((i: int) => i > 0) == (asList(1, 2, 3), Nil[int]())

    check: asList(3, 5, 2, 4, 1).sort == asList(1, 2, 3, 4, 5)

  test "Iterators":
    let lst1 = [1, 2, 3, 4, 5].asList
    var lst2 = Nil[int]()
    for x in lst1:
      lst2 = Cons(x, lst2)
    check: lst2 == lst1.reverse

    let lst3 = [1, 2, 3, 4, 5].asList
    for i, x in lst3:
      check: i == x.pred

  test "List - traverse with Maybe should allow to properly infer gcsafe":
    proc f(i: int): auto = i.just

    proc g(): auto {.gcsafe.} =
      asList(1, 2, 3).traverse(f)

    discard g()

  test "Traversable":
    check: asList(asList(1)) == asList(1)
    check: asList(1.just) == asList(1)
    check: asList([1.just].asList) == [1.just].asList
    when compiles(asList(1.just) == [1.just].asList):
      check: false

  test "Kleisli":
    let f = (v: int) => asList(v, v + 1, v + 2)
    let g = (v: int) => asList(v, v * 2, v * 3)
    check: 1.point(List) >>= (f >=> g) == asList(1, 2, 3, 2, 4, 6, 3, 6, 9)
