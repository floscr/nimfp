import sugar,
       classy,
       ./maybe,
       ./either,
       ./list

typeclass FlattenInst, F[_]:
  proc flatten[T](xs: List[F[T]]): List[T] =
    xs.map((v: F[T]) => v.asList).join

instance FlattenInst, Maybe[_], exporting(_)

instance FlattenInst, E => Either[E,_], exporting(_)

instance FlattenInst, List[_], exporting(_)
