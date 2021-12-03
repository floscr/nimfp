import unittest,
       sugar,
       fp/maybe,
       fp/either,
       fp/list,
       fp/forcomp,
       fp/mtransf

suite "Monad transformers":
  test "MaybeTMaybe":
    let v = maybeT(1.just.just)
    check: v.getOrElse(2) == 1.just
    check: v.map(v => $v).getOrElse("") == "1".just
    check: v.flatMapF((v: int) => ($v).just).getOrElse("") == "1".just
    check: v.flatMap((v: int) => maybeT(($v).just.just)).getOrElse("") == "1".just

    proc getArticle(id: int): Maybe[Maybe[string]] =
      if id == 0:
        nothing(Maybe[string])
      else:
        ($id).just.just
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT((a1, a2, a3).just.just)
    check: articles.run == ("1", "2", "3").just.just
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      (a[0], a[1], a[2], bad).just.just.maybeT
    check: badArticles.run == nothing(Maybe[(string, string, string, string)])

  test "MaybeTEither":
    let v = maybeT(1.just.rightS)
    check: v.getOrElse(2) == 1.rightS
    check: v.map(v => $v).getOrElse("") == "1".rightS
    check: v.flatMapF((v: int) => ($v).rightS).getOrElse("") == "1".rightS
    check: v.flatMap((v: int) => maybeT(($v).just.rightS)).getOrElse("") == "1".rightS

    proc getArticle(id: int): EitherS[Maybe[string]] =
      if id == 0:
        "Not found".left(Maybe[string])
      else:
        ($id).just.rightS
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT((a1, a2, a3).just.rightS)
    check: articles.run == ("1", "2", "3").just.rightS
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      (a[0], a[1], a[2], bad).just.rightS.maybeT
    check: badArticles.run == "Not found".left(Maybe[(string, string, string, string)])

  test "MaybeTList":
    let v = maybeT([1.just].asList)
    check: v.getOrElse(2) == [1].asList
    check: v.map(v => $v).getOrElse("") == ["1"].asList
    check: v.flatMapF((v: int) => [$v].asList).getOrElse("") == ["1"].asList
    check: v.flatMap((v: int) => maybeT([($v).just].asList)).getOrElse("") == ["1"].asList

    proc getArticle(id: int): List[Maybe[string]] =
      if id == 0:
        Nil[Maybe[string]]()
      else:
        ($id).just.point(List)
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT([(a1, a2, a3).just].asList)
    check: articles.run == [("1", "2", "3").just].asList
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      [[a[0], a[1], a[2], bad].asList.just].asList.maybeT
    check: badArticles.run == Nil[Maybe[List[string]]]()

  test "Misc functions":
    check: string.nothing.just.maybeT.getOrElse("1") == "1".just
    check: string.nothing.just.maybeT.getOrElse(() => "1") == "1".just

    check: string.nothing.rightS.maybeT.getOrElseF(() => "Error".left(string)) == "Error".left(string)
