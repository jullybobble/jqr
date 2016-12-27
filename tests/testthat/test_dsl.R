test_dsl <- function(expected, dsl, skipped = F) {
  if(skipped) {
    skip(paste(expected, 'skipped'))
  } else {
    test_that(expected, {
      expect_equivalent(
        dsl,
        as.jq(expected)
      )
    })
  }
}

test_dsl('.',
         jin)

test_dsl('.["foo"]',
         jin["foo"])

test_dsl('.["foo"]["bar"]["baz"]',
         jpath('foo', 'bar', 'baz'))

test_dsl('.["foo"]?',
         jin['foo', opt = T])

test_dsl('[.["foo"]?]',
         jarray(jin['foo', opt = T]))

test_dsl('.[0]',
         jin[1])

test_dsl('.[2]',
         jin[3])

test_dsl('.[2:4]',
         jin[from = 3, to = 5])

test_dsl('.[:3]',
         jin[to = 4])

test_dsl('.[-2:]',
         jin[from = -2])

test_dsl('.[2:4]',
         jslice(from = 3, to = 5))

test_dsl('.[:3]',
         jslice(to = 4))

test_dsl('.[-2:]',
         jslice(from = -2))

test_dsl('.[]',
         jin[])

test_dsl('.["foo"], .["bar"]',
         jmux(jin['foo'], jin['bar']))

test_dsl('. | .["foo"]',
         jin %|% jin['foo'])

test_dsl('.["user"], .["projects"][]',
         jmux(jin['user'], jin['projects'][]))

test_dsl('.[4,2]',
         jin[5,3])

test_dsl('.[] | .["name"]',
         jin[] %|%  jin['name'])

test_dsl('[.["user"], .["projects"][]]',
         jarray(jin["user"], jin["projects"][]))

test_dsl('{"user": .["user"], "title": .["titles"][]}',
         jobject("user", "title" %:% jin["titles"][]))

test_dsl('{(.["user"]): .["titles"]}',
         jobject(jin["user"] %:% jin["titles"]))

test_dsl('.["a"] + 1',
         jin["a"] + 1)

test_dsl('.["a"] + .["b"]',
         jin["a"] + jin["b"])

test_dsl('.["a"] + null',
         jin["a"] + NA)

test_dsl('.["a"] + 1',
         jin["a"] + 1)

test_dsl('{"a": 1} + {"b": 2} + {"c": 3} + {"a": 42}',
         jobject(a =  1) + jobject(b =  2) + jobject(c =  3) + jobject(a =  42))

test_dsl('-.["a"] + 4',
         -jin["a"] + 4)

test_dsl('4 - .["a"]',
         jq(4) - jin["a"])

test_dsl('. - ["xml", "yaml"]',
         jin - jarray("xml", "yaml"))

test_dsl('10 / . * 3',
         as.jq(10) / jin * 3)

test_dsl('. / ", "',
         jin / ", ")

test_dsl('{"k": {"a": 1, "b": 2}} * {"k": {"a": 0, "c": 3}}',
         jobject("k" =  jobject("a" =  1, "b" =  2)) * jobject("k" =  jobject("a" =  0,"c" =  3)))

test_dsl('.[] | length',
         jin[] %|%  jlength)

test_dsl('keys',
         jkeys)

test_dsl('map(has("foo"))',
         jmap(jhas("foo")))

test_dsl('map(has(2))',
         jmap(jhas(2)))

test_dsl('del(.["foo"])',
         jdel(jin["foo"]))

test_dsl('del(.[1,2])',
         jdel(jin[2, 3]))

test_dsl('to_entries',
         jto_entries)

test_dsl('from_entries',
         jfrom_entries)

test_dsl('with_entries(.["key"] |= "KEY_" + .)',
         jwith_entries(jin["key"] %|=% "KEY_" + jin))

test_dsl('map(select(. >= 2))',
         jmap(jselect(jin >= 2)))

test_dsl('.[] | numbers',
         jin[] %|% jnumbers)

test_dsl('1, empty, 2',
         jseq(1, jempty, 2))

test_dsl('[1, 2, empty, 3]',
         jarray(1,2,jempty,3))

test_dsl('map(. + 1)',
         jmap(jin + 1))

test_dsl('[paths]',
         jarray(jpaths()))

test_dsl('[paths(scalars)]',
         jarray(jpaths(jscalars)))

test_dsl('add',
         jadd)

test_dsl('any',
         jany)

test_dsl('all',
         jall)

test_dsl('flatten(1)',
         jflatten(1))

test_dsl('flatten',
         jflatten())

test_dsl('range(2; 4)',
         jrange(2,4))

test_dsl('[range(2; 4)]',
         jarray(jrange(2,4)))

test_dsl('[range(0; 4)]',
         jarray(jrange(4)))

test_dsl('floor',
         jfloor)

test_dsl('sqrt',
         jsqrt)

test_dsl('.[] | tonumber',
         jin[] %|%  jtonumber)

test_dsl('.[] | tostring',
         jin[] %|%  jtostring)

test_dsl('map(type)',
         jmap(jtype))

test_dsl('sort',
         jsort())

test_dsl('sort(.["foo"])',
         jsort(jin["foo"]))

test_dsl('group(.["foo"])',
         jgroup(jin["foo"]))

test_dsl('min',
         jmin())

test_dsl('max(.["foo"])',
         jmax(jin["foo"]))

test_dsl('unique',
         junique())

test_dsl('unique(.["foo"])',
         junique(jin["foo"]))

test_dsl('unique(length)',
         junique(jlength))

test_dsl('reverse',
         jreverse)

test_dsl('contains("bar")',
         jcontains("bar"))

test_dsl('contains(["baz", "bar"])',
         jcontains(jarray("baz", "bar")))

test_dsl('contains(["bazzzzz", "bar"])',
         jcontains(jarray("bazzzzz", "bar")))

test_dsl('contains({"foo": 12, "bar": [{"barp": 12}]})',
         jcontains(jobject(foo =  12, bar =  jarray(jobject(barp =  12)))))

test_dsl('contains({"foo": 12, "bar": [{"barp": 15}]})',
         jcontains(jobject(foo =  12, bar =  jarray(jobject(barp =  15)))))

test_dsl('indices(", ")',
         jindices(", "))

test_dsl('indices(1)',
         jindices(1))

test_dsl('indices([1, 2])',
         jindices(jarray(1,2)))

test_dsl('index(", ")',
         jindex(", "))

test_dsl('[rindex(", ")]',
         jarray(jrindex(", ")))

test_dsl('[.[] | startswith("foo")]',
         jarray(jin[] %|% jstartswith("foo")))

test_dsl('[.[] | endswith("foo")]',
         jarray(jin[] %|% jendswith("foo")))

test_dsl('match("(abc)+"; "g")',
         jmatch("(abc)+", "g"))

test_dsl('match("foo")',
         jmatch("foo"))

test_dsl('match(["foo", "ig"])',
         jmatch(jarray("foo", "ig")))

test_dsl('match("foo (?bar)? foo"; "ig")',
         jmatch("foo (?bar)? foo", "ig"))

test_dsl('test("foo")',
         jtest("foo"))

test_dsl('test("foo"; "i")',
         jtest("foo", "i"))

test_dsl('test("foo")',
         jtest("foo"))

test_dsl('[.[] | ltrimstr("foo")]',
         jarray(jin[] %|% jltrimstr("foo")))

test_dsl('[.[] | rtrimstr("foo")]',
         jarray(jin[] %|% jrtrimstr("foo")))

test_dsl('explode',
         jexplode)

test_dsl('implode',
         jimplode)

test_dsl('split(", ")',
         jsplit(", "))

test_dsl('join(", ")',
         jjoin(", "))

test_dsl('recurse(.["foo"][])',
         jrecurse(jin["foo"][]))

test_dsl('recurse',
         jrecurse())

test_dsl('.["a"] = .["b"]',
         jin['a'] %=% jin['b'])

test_dsl('.["a"] |= .["b"]',
         jin['a'] %|=% jin['b'])
