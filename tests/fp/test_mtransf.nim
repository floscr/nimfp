import unittest,
       sugar,
       fp/maybe,
       fp/either,
       fp/list,
       fp/forcomp,
       fp/mtransf

suite "Monad transformers":
  test "MaybeTMaybe":
    let v = maybeT(1.some.some)
    check: v.getOrElse(2) == 1.some
    check: v.map(v => $v).getOrElse("") == "1".some
    check: v.flatMapF((v: int) => ($v).some).getOrElse("") == "1".some
    check: v.flatMap((v: int) => maybeT(($v).some.some)).getOrElse("") == "1".some

    proc getArticle(id: int): Maybe[Maybe[string]] =
      if id == 0:
        none(Maybe[string])
      else:
        ($id).some.some
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT((a1, a2, a3).some.some)
    check: articles.run == ("1", "2", "3").some.some
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      (a[0], a[1], a[2], bad).some.some.maybeT
    check: badArticles.run == none(Maybe[(string, string, string, string)])

  test "MaybeTEither":
    let v = maybeT(1.some.rightS)
    check: v.getOrElse(2) == 1.rightS
    check: v.map(v => $v).getOrElse("") == "1".rightS
    check: v.flatMapF((v: int) => ($v).rightS).getOrElse("") == "1".rightS
    check: v.flatMap((v: int) => maybeT(($v).some.rightS)).getOrElse("") == "1".rightS

    proc getArticle(id: int): EitherS[Maybe[string]] =
      if id == 0:
        "Not found".left(Maybe[string])
      else:
        ($id).some.rightS
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT((a1, a2, a3).some.rightS)
    check: articles.run == ("1", "2", "3").some.rightS
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      (a[0], a[1], a[2], bad).some.rightS.maybeT
    check: badArticles.run == "Not found".left(Maybe[(string, string, string, string)])

  test "MaybeTList":
    let v = maybeT([1.some].asList)
    check: v.getOrElse(2) == [1].asList
    check: v.map(v => $v).getOrElse("") == ["1"].asList
    check: v.flatMapF((v: int) => [$v].asList).getOrElse("") == ["1"].asList
    check: v.flatMap((v: int) => maybeT([($v).some].asList)).getOrElse("") == ["1"].asList

    proc getArticle(id: int): List[Maybe[string]] =
      if id == 0:
        Nil[Maybe[string]]()
      else:
        ($id).some.point(List)
    let articles = act:
      a1 <- maybeT(getArticle(1))
      a2 <- maybeT(getArticle(2))
      a3 <- maybeT(getArticle(3))
      maybeT([(a1, a2, a3).some].asList)
    check: articles.run == [("1", "2", "3").some].asList
    let badArticles = act:
      a <- articles
      bad <- maybeT(getArticle(0))
      [[a[0], a[1], a[2], bad].asList.some].asList.maybeT
    check: badArticles.run == Nil[Maybe[List[string]]]()

  test "Misc functions":
    check: string.none.some.maybeT.getOrElse("1") == "1".some
    check: string.none.some.maybeT.getOrElse(() => "1") == "1".some

    check: string.none.rightS.maybeT.getOrElseF(() => "Error".left(string)) == "Error".left(string)
