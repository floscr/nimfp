import json,
       sugar,
       typetraits,
       ../either,
       ../maybe,
       ../list,
       ../map,
       boost/jsonserialize

proc mget*(n: JsonNode, key: string|int): EitherS[Maybe[JsonNode]] =
  ## Returns the child node if it exists, or none.
  ## Returns an error if `key` is int and `n` is not an array, or
  ## if `key` is string and `n` is not an object.
  case n.kind
  of JObject:
    when key is string:
      n.contains(key).maybeF(() => n[key]).rightS
    else:
      ("JsonNode.mget: can't use string key with node of type " & $n.kind).left(Maybe[JsonNode])
  of JArray:
    when key is int:
      (key >= 0 and key < n.len).maybeF(() => n[key]).rightS
    else:
      ("JsonNode.mget: can't use int key with node of type " & $n.kind).left(Maybe[JsonNode])
  else:
    ("JsonNode.mget: can't get the child node from node of type " & $n.kind).left(Maybe[JsonNode])

proc mget*(n: Maybe[JsonNode], key: string|int): EitherS[Maybe[JsonNode]] =
  ## Returns the child node if it exists, or none.
  ## Returns an error if `key` is int and `n` is not an array, or
  ## if `key` is string and `n` is not an object.
  if n.isDefined:
    n.get.mget(key)
  else:
    JsonNode.none.rightS

proc mget*(key: string|int): Maybe[JsonNode] -> EitherS[Maybe[JsonNode]] =
  (n: Maybe[JsonNode]) => n.mget(key)

proc value*[T](t: typedesc[T], n: JsonNode): EitherS[T] =
  ## Returns the value of the node `n` of type `t`
  template checkKind(nKind: JsonNodeKind): untyped =
    if n.kind != nKind:
      raise newException(ValueError, "Can't get Json node's value of kind " & $nKind & ", node's kind is " & $n.kind)
  when t is int or t is int64:
    tryS do() -> auto:
      JInt.checkKind
      n.getBiggestInt.T
  elif t is string:
    tryS do() -> auto:
      JString.checkKind
      n.getStr
  elif t is float:
    tryS do() -> auto:
      JFloat.checkKind
      n.getFloat
  elif t is bool:
    tryS do() -> auto:
      JBool.checkKind
      n.getBool
  else:
    proc `$`[T](just:typedesc[T]): string = name(T)
    {.fatal: "Can't get value of type " & $T}

proc mvalue*[T](t: typedesc[T]): Maybe[JsonNode] -> EitherS[Maybe[T]] =
  (n: Maybe[JsonNode]) => (if n.isDefined: value(T, n.get).map((v: T) => v.just) else: T.none.rightS)

type
  Jsonable* = concept t
    %t is JsonNode

proc mjson*[T: Jsonable](v: T): Maybe[JsonNode] =
  (%v).just

proc mjson*[T: Jsonable](v: Maybe[T]): Maybe[JsonNode] =
  v.map(v => %v)

proc toJsonObject*(xs: List[(string, Maybe[JsonNode])]): JsonNode =
  var res = newJObject()
  xs.forEach(
    (v: (string, Maybe[JsonNode])) => (if v[1].isDefined: res[v[0]] = v[1].get)
  )
  return res

proc toJson*[T](v: Maybe[T]): JsonNode =
  mixin toJson
  if v.isDefined:
    v.get.toJson
  else:
    nil

proc fromJson*[T](_: typedesc[Maybe[T]], n: JsonNode): Maybe[T] =
  mixin fromJson
  if n.isNil or n.kind == JNull:
    T.none
  else:
    T.fromJson(n).just

proc toJson*[T](v: List[T]): JsonNode =
  mixin toJson
  let res = newJArray()
  v.forEach do(v: T) -> void:
    let n = v.toJson
    if n.isNil:
      res.add(newJNull())
    else:
      res.add(n)
  return res

proc fromJson*[T](_: typedesc[List[T]], n: JsonNode): List[T] =
  if n.isNil or n.kind != JArray:
    raise newFieldException("Value of the node is not an array")
  result = Nil[T]()
  mixin fromJson
  for i in countdown(n.len-1, 0):
    result = Cons(T.fromJson(n[i]), result)

proc toJson*[T](v: Map[string,T]): JsonNode =
  mixin toJson
  let res = newJObject()
  v.forEach do(v: (string,T)) -> void:
    let val = v[1].toJson
    if not val.isNil:
      res[v[0]] = val
  return res

proc fromJson*[T](_: typedesc[Map[string,T]], n: JsonNode): Map[string,T] =
  if n.isNil or n.kind != JObject:
    raise newFieldException("Value of the node is not an object")
  result = newMap[string,T]()
  mixin fromJson
  for k, v in n:
    let val = T.fromJson(v)
    when compiles(val.isNil):
      if not val.isNil:
        result = result.add((k, val))
    else:
      result = result.add((k, val))

