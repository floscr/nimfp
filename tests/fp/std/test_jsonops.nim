import unittest,
       sugar,
       fp,
       json,
       boost/typeutils,
       boost/jsonserialize

suite "std.json":

  let doc = """
{
  "int": 123,
  "str": "Hello!",
  "bool": true,
  "float": 1.2,
  "obj": {
    "int": 20
  }
}
""".parseJson

  test "mget":
    check: doc.mget("int").get.isDefined
    check: doc.mget("int2").get.isEmpty
    let v = doc.just.rightS >>= (mget("obj") >=> mget("int"))
    check: v.get.isDefined

  test "value":
    check: value(int, doc["int"]) == 123.rightS
    check: value(string, doc["int"]).isLeft
    check: value(string, doc["str"]) == "Hello!".rightS
    check: value(int, doc["str"]).isLeft
    check: value(float, doc["float"]) == 1.2.rightS
    check: doc.just.rightS >>= (
      mget("obj") >=>
      mget("int") >=>
      mvalue(int)
    ) == 20.just.rightS
    check: value(bool, doc["bool"]) == true.rightS

  test "toJson":
    check: [
      ("a", 1.mjson),
      ("b", "a".mjson),
      ("c", int64.nothing.mjson),
    ].asList.toJsonObject == %*{ "a": 1, "b": "a" }

  test "boost serialization test":
    data Test, json, show:
      a = "test".just
      b = asList(1, 2)
      c = asMap({"a": 1, "b": 2})

    check: Test.fromJson(initTest().toJson()).a == "test".just
    check: Test.fromJson(initTest(a = string.nothing).toJson()).a == string.nothing
    check: Test.fromJson(initTest().toJson()).b == asList(1, 2)
    check: Test.fromJson(initTest().toJson()).c == asMap({"a": 1, "b": 2})
