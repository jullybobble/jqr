require(jsonlite, warn.conflicts = F)

test_dsl <- function(expected, dsl, skipped = F) {
  if(skipped) {
    skip(paste(expected, 'skipped'))
  } else {
    dsl <- substitute(dsl)
    test_that(expected, {
      expect_equivalent(
        jq_(dsl),
        as.jq(expected)
      )
    })
  }
}

test_dsl('.',
         .)

test_dsl('.["foo"]',
         .["foo"])

test_dsl('.["foo"]?',
         .['foo', opt = T])

test_dsl('[.["foo"]?]',
         arr(.['foo', opt = T]))

test_dsl('.[0]',
         .[1])

test_dsl('.[2]',
         .[3])

test_dsl('.[2:4]',
         .[from = 3, to = 5])

test_dsl('.[:3]',
         .[to = 4])

test_dsl('.[-2:]',
         .[from = -2])

test_dsl('.[]',
         .[])

test_dsl('.["foo"], .["bar"]',
         mux(.['foo'], .['bar']))

test_dsl('. | .["foo"]',
         . %|% .['foo'])

test_dsl('.["user"], .["projects"] | .[]',
         mux(.['user'], .['projects']) %|% .[])

test_dsl('.[4,2]',
         .[5,3])

test_dsl('.[] | .["name"]',
         .[] %|%  .['name'])

test_dsl('[.["user"], .["projects"][]]',
         arr(.["user"], .["projects"][]))

test_dsl('{"user": .["user"], "title": .["titles"][]}',
         obj("user", "title" %:% .["titles"][]))

test_dsl('{(.["user"]): .["titles"]}',
         obj(.["user"] %:% .["titles"]))

test_dsl('.["a"] + 1',
         .["a"] + 1)

test_dsl('.["a"] + .["b"]',
         .["a"] + .["b"])

test_dsl('.["a"] + null',
         .["a"] + NA)

test_dsl('.["a"] + 1',
         .["a"] + 1)

test_dsl('{"a": 1} + {"b": 2} + {"c": 3} + {"a": 42}',
         obj(a =  1) + obj(b =  2) + obj(c =  3) + obj(a =  42))

test_dsl('-.["a"] + 4',
         -.["a"] + 4)

# test_dsl(skip = T,
#          '4 - .a',
#          4 - .["a"])

test_dsl('. - ["xml", "yaml"]',
         . - arr("xml", "yaml"))

# test_dsl(skip = T,
#          '10 / . * 3',
#          10 / . * 3)

test_dsl('. / ", "',
         . / ", ")

test_dsl('{"k": {"a": 1, "b": 2}} * {"k": {"a": 0, "c": 3}}',
         obj("k" =  obj("a" =  1, "b" =  2)) * obj("k" =  obj("a" =  0,"c" =  3)))

test_dsl('.[] | length',
         .[] %|%  length)

test_dsl('keys',
         keys)

test_dsl('map(has("foo"))',
         map(has("foo")))

test_dsl('map(has(2))',
         map(has(2)))

test_dsl('del(.["foo"])',
         del(.["foo"]))

test_dsl('del(.[1,2])',
         del(.[2, 3]))

test_dsl('to_entries',
         to_entries)

test_dsl('from_entries',
         from_entries)

# test_dsl('with_entries(.key |= "KEY_" + .)',
#          with_entries(.["key"] %|=% "KEY_" + ex()))

test_dsl('map(select(. >= 2))',
         map(select(. >= 2)))

test_dsl('.[] | numbers',
         .[] %|% numbers)

test_dsl('1, empty, 2',
         seq(1, empty, 2))

test_dsl('[1, 2, empty, 3]',
         arr(1,2,empty,3))

test_dsl('map(. + 1)',
         map(. + 1))

test_dsl('[paths]',
         arr(paths()))

test_dsl('[paths(scalars)]',
         arr(paths(scalars)))

test_dsl('add',
         add)

test_dsl('any',
         any)

test_dsl('all',
         all)

test_dsl('flatten(1)',
         flatten(1))

test_dsl('flatten',
         flatten())

test_dsl('range(2; 4)',
         range(2,4))

test_dsl('[range(2; 4)]',
         arr(range(2,4)))

test_dsl('[range(0; 4)]',
         arr(range(4)))

test_dsl('floor',
         floor)

test_dsl('sqrt',
         sqrt)

test_dsl('.[] | tonumber',
         .[] %|%  tonumber)

test_dsl('.[] | tostring',
         .[] %|%  tostring)

test_dsl('map(type)',
         map(type))

test_dsl('sort',
         sort())

test_dsl('sort(.["foo"])',
         sort(.["foo"]))

test_dsl('group(.["foo"])',
         group(.["foo"]))

test_dsl('min',
         min())

test_dsl('max(.["foo"])',
         max(.["foo"]))

test_dsl('unique',
         unique())

test_dsl('unique(.["foo"])',
         unique(.["foo"]))

test_dsl('unique(length)',
         unique(length))

test_dsl('reverse',
         reverse)

test_dsl('contains("bar")',
         contains("bar"))

test_dsl('contains(["baz", "bar"])',
         contains(arr("baz", "bar")))

test_dsl('contains(["bazzzzz", "bar"])',
         contains(arr("bazzzzz", "bar")))

test_dsl('contains({"foo": 12, "bar": [{"barp": 12}]})',
         contains(obj(foo =  12, bar =  arr(obj(barp =  12)))))

test_dsl('contains({"foo": 12, "bar": [{"barp": 15}]})',
         contains(obj(foo =  12, bar =  arr(obj(barp =  15)))))

test_dsl('indices(", ")',
         indices(", "))

test_dsl('indices(1)',
         indices(1))

test_dsl('indices([1, 2])',
         indices(arr(1,2)))

test_dsl('index(", ")',
         index(", "))

test_dsl('[rindex(", ")]',
         arr(rindex(", ")))

test_dsl('[.[] | startswith("foo")]',
         arr(.[] %|% startswith("foo")))

test_dsl('[.[] | endswith("foo")]',
         arr(.[] %|% endswith("foo")))

test_dsl('match("(abc)+"; "g")',
         match("(abc)+", "g"))

test_dsl('match("foo")',
         match("foo"))

test_dsl('match(["foo", "ig"])',
         match(arr("foo", "ig")))

test_dsl('match("foo (?bar)? foo"; "ig")',
         match("foo (?bar)? foo", "ig"))

test_dsl('test("foo")',
         test("foo"))

test_dsl('test("foo"; "i")',
         test("foo", "i"))

test_dsl('test("foo")',
         test("foo"))

test_dsl('[.[] | ltrimstr("foo")]',
         arr(.[] %|% ltrimstr("foo")))

test_dsl('[.[] | rtrimstr("foo")]',
         arr(.[] %|% rtrimstr("foo")))

test_dsl('explode',
         explode)

test_dsl('implode',
         implode)

test_dsl('split(", ")',
         split(", "))

test_dsl('join(", ")',
         join(", "))

test_dsl('recurse(.["foo"][])',
         recurse(.["foo"][]))

test_dsl('recurse',
         recurse())
