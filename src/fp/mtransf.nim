import sugar,
       classy,
       ./maybe,
       ./either,
       ./list

type
  MaybeTMaybe*[A] = ref object
    run*: Maybe[Maybe[A]]
  MaybeTEither*[E,A] = ref object
    run*: Either[E,Maybe[A]]
  MaybeTList*[A] = ref object
    run*: List[Maybe[A]]

typeclass MaybeTInst, [F[_], MaybeTF[_]], exported:
  proc maybeT[A](run: F[Maybe[A]]): MaybeTF[A] =
    MaybeTF[A](run: run)

  proc point[A](v: A, t: typedesc[MaybeTF[A]]): MaybeTF[A] =
    v.point(F[Maybe[A]]).maybeT

  proc getOrElse[A](o: MaybeTF[A], v: A): F[A] =
    o.run.map((o: Maybe[A]) => o.getOrElse(v))

  proc getOrElse[A](o: MaybeTF[A], f: () -> A): F[A] =
    o.run.map((o: Maybe[A]) => o.getOrElse(f))

  proc getOrElseF[A](o: MaybeTF[A], f: () -> F[A]): F[A] =
    o.run.flatMap((o: Maybe[A]) => o.map((v: A) => v.point(F[A])).getOrElse(f))

  proc map[A,B](o: MaybeTF[A], f: A -> B): MaybeTF[B] =
    maybeT(o.run.map((o: Maybe[A]) => o.map(f)))

  proc flatMap[A,B](o: MaybeTF[A], f: A -> MaybeTF[B]): MaybeTF[B] =
    o.run.flatMap(
      (opt: Maybe[A]) => (if opt.isDefined: opt.get.f.run else: B.none.point(F[Maybe[B]]))
    ).maybeT

  proc flatMapF[A,B](o: MaybeTF[A], f: A -> F[B]): MaybeTF[B] =
    o.flatMap((v: A) => f(v).map((v: B) => v.just).maybeT)

  template elemType[A](v: MaybeTF[A]): typedesc =
    A

instance MaybeTInst, [Maybe[_], MaybeTMaybe[_]], exporting(_)

instance MaybeTInst, E => [Either[E, _], MaybeTEither[E,_]], exporting(_)

instance MaybeTInst, [List[_], MaybeTList[_]], exporting(_)
