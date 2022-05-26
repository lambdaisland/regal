# Regal

_Royally reified regular expressions_


<!-- badges -->
[![CircleCI](https://circleci.com/gh/lambdaisland/regal.svg?style=svg)](https://circleci.com/gh/lambdaisland/regal) [![cljdoc badge](https://cljdoc.org/badge/lambdaisland/regal)](https://cljdoc.org/d/lambdaisland/regal) [![Clojars Project](https://img.shields.io/clojars/v/lambdaisland/regal.svg)](https://clojars.org/lambdaisland/regal)
<!-- /badges -->

## tl;dr

Regal lets you manipulate regular expressions as data, by providing a
Hiccup-like regex syntax, and ways to convert between this Hiccup syntax (Regal
syntax), compiled regex patterns, and test.check generators. It also helps with
writing cross-platform code by providing consistent semantics across JS/Java
runtimes, and it allows converting JavaScript regex to Java regex semantically
(useful for e.g. dealing with JSON Schema in Clojure)

## The slightly longer version

Regal provides a syntax for writing regular expressions using plain Clojure
data: vectors, keywords, strings. This is known as Regal notation.

Once you have a Regal form you can either compile it to a regex object
(`java.util.regex.Pattern` or JavaScript `RegExp`), or you can use it to create
a Generator (see [test.check](https://github.com/clojure/test.check)) for
generating values that conform to the given pattern.

It is also possible to parse regular expression patterns back to Regal forms.

Regal is Clojure and ClojureScript compatible, and has fixed semantics across
platforms. Write your forms once and run them anywhere! It also allows
manipulating multiple regex flavors regardless of the current platform, so you
can do things like converting a JavaScript regex pattern to one that is suitable
for Java's regex engine.

<!-- opencollective -->
## Support Lambda Island Open Source

Regal is part of a growing collection of quality Clojure libraries and
tools released on the Lambda Island label. If you find value in our work please
consider [becoming a backer on Open Collective](http://opencollective.com/lambda-island#section-contribute)
<!-- /opencollective -->

## Project status

Regal is alpha level software, this does not mean it is of low quality or not
fit for use, it does mean that future breakage of the API is still possible.

The following aspects of the library are generally well tested and developed,
and we intend to retain compatibility as much as practically possible.

- Regal syntax as described in this README
- Generating regex patterns from regal forms
- Parsing regex patterns to regal forms

The following aspects have known issues or are otherwise untested or incomplete,
and you can expect them to change significantly as we further develop them:

- Creating test.check generators from regal forms
- clojure.spec-alpha integration
- Malli integration

## Installation

deps.edn

``` clojure
lambdaisland/regal {:mvn/version "0.0.143"}
```

project.clj

``` clojure
[lambdaisland/regal "0.0.143"]
```

### An example

``` clojure
(require '[lambdaisland.regal :as regal]
         '[lambdaisland.regal.generator :as regal-gen])

;; Regal expression, like Hiccup but for Regex
(def r [:cat
        [:+ [:class [\a \z]]]
        "="
        [:+ [:not \=]]])

;; Convert to host-specific regex
(regal/regex r)
;;=> #"[a-z]+\Q=\E[^=]+"

;; Match strings
(re-matches (regal/regex r) "foo=bar")
;;=> "foo=bar"

;; ... And generate them
(regal-gen/gen r)
;;=> #clojure.test.check.generators.Generator{...}

(regal-gen/sample r)
;;=> ("t=" "d=5Ë" "zja=·" "uatt=ß¾" "lqyk=É" "xkj=q\f" "gxupw=æ" "pkadbgmc=¯²" "f=ÃJ" "d=ç")
```

### A swiss army knife

Regal can convert between three different represenations for regular
expressions, Regal **forms**, **patterns**(i.e. strings), and **regex** objects.
Here is an overview of how to get from one to the other.

| ↓From / To→ | Form                                   | Pattern                          | Regex                      |
|---------------|----------------------------------------|----------------------------------|----------------------------|
| Form          | identity                               | lambdaisland.regal/pattern       | lambdaisland.regal/regex   |
| Pattern       | lambdaisland.regal.parse/parse-pattern | identity                         | lambdaisland.regal/compile |
| Regex         | lambdaisland.regal.parse/parse         | lambdaisland.regal/regex-pattern | identity                   |

### Regal forms

Forms consist of vectors, keywords, strings, character literals, and in some
cases integers. For example:

``` clojure
[:cat [:alt [:char 11] [:char 13]] \J [:rep "hello" 2 3]]
```

Forms have platform-independent semantics. The same regal form will match the
same strings both in Clojure and ClojureScript, even though Java and JavaScript
(and even different versions of Java or JavaScript) have different regex
"flavors". In other words, we generate the regex that is right for the target
platform.

``` clojure
;; Clojure
(regal/regex :vertical-whitespace) ;;=> #"\v"

;; ClojureScript
(regal/regex :vertical-whitespace) ;;=> #"[\n\x0B\f\r\x85\u2028\u2029]"
```

Regal currently knows about three "flavors"

- `:java8` Java 1.8 (earlier versions are not supported)
- `:java9` Java 9 or later
- `:ecma` ECMAScript (JavaScript)

By default it takes the flavor that is best suited for the platform, but you can override that with `lambdaisland.regal/with-flavor`

``` clojure
(regal/with-flavor :ecma
  (regal/pattern ...))
```

Note that using `regal/regex` with a flavor that does not correspond with the
flavor of the platform may yield unexpected results, when dealing with "foreign"
regex flavors always stick to string representations (i.e. **patterns**).

### Pattern

The second regex representation regal knows about is the **pattern**, i.e. the
regex pattern in string form.

``` clojure
(regal/regex-pattern #"\u000B\v") ;; => "\\u000B\\v"
```

Depending on the situation there are several reasons why you might want to use
this pattern representation over the compiled regex object.

- simple strings, so easy to (de-)serialize
- value semantics (can be compared)
- allow manipulating regex pattern of regex flavors other than the one supported
  by the current runtime

Note that in Clojure the syntax available in regex patterns differs from the
syntax available in strings, in particluar when it comes to notations starting
with a backslash. e.g. `#"\xFF"` is a valid regex, while `"\xFF"` is not a valid
string. We encode regex patterns in strings, which practically speaking means
that backslashes are escaped (doubled).

``` clojure
(regal/regex-pattern #"\xFF") ;;=> "\\xFF"
(regal/compile "\\xFF")       ;;=> #"\xFF"
```

### Regex

To use the regex engine provided by the runtime (e.g. through `re-find` or
`re-seq`) you need a platform-specific regex object. This is what
`lambdaisland.regal/regex` gives you.

### Grammar

- Strings and characters match literally. They are escaped, so `.` matches a
  period, not any character, `^` matches a caret, etc.
- A few keywords have special meaning.
  - `:any` : match any character, like `.`. Does not match newlines.
  - `:start` match the start of the input
  - `:end` : match the end of the input
  - `:digit` : match any digit (`0-9`)
  - `:non-digit` : match non-digits (not `0-9`)
  - `:word` : match word characters (`A-Za-z0-9_`)
  - `:non-word` : match non-word characters (not `A-Za-z0-9_`)
  - `:newline` : Match `\n`
  - `:return` : Match `\r`
  - `:tab` : Match `\t`
  - `:form-feed` : Match `\f`
  - `:line-break` : Match `\n`, `\r`, `\r\n`, or other unicode newline characters
  - `:alert` : match `\a` (U+0007)
  - `:escape` : match `\e` (U+001B)
  - `:whitespace` : match any whitespace character. Uses `\s` on JavaScript, and
    a character range of whitespace characters on Java with equivalent semantics
    as JavaScript `\s`, since `\s` in Java only matches ASCII whitespace.
  - `:non-whitespace` : match non-whitespace
  - `:vertical-whitespace` : match vertical whitespace, including newlines and vertical tabs `#"\n\x0B\f\r\x85\u2028\u2029"`
  - `:vertical-tab` : match a vertical tab `\v` (U+000B)
  - `:null` : match a NULL byte/char
- All other forms are vectors, with the first element being a keyword
  - `[:cat forms...]` : concatenation, match the given Regal expressions in order
  - `[:alt forms...]` : alternatives, match one of the given options, like `(foo|bar|baz)`
  - `[:* form]` : match the given form zero or more times
  - `[:+ form]` : match the given form one or more times
  - `[:? form]` : match the given form zero or one time
  - `[:*? form]` : lazily match the given form zero or more times
  - `[:+? form]` : lazily match the given form one or more times
  - `[:?? form]` : lazily match the given form zero or one time
  - `[:class entries...]` : match any of the given characters or ranges, with ranges given as two element vectors. E.g. `[:class [\a \z] [\A \Z] "_" "-"]` is equivalent to `[a-zA-Z_-]`
  - `[:not entries...]` : like `:class`, but negates the result, equivalent to `[^...]`
  - `[:repeat form num]` : repeat a form fixed number of times, like `{5}`
  - `[:repeat form min max]` : repeat a form a number of times, like `{2,5}`
  - `[:lazy-repeat form num]` : lazily repeat a form fixed number of times, like `{5}?`
  - `[:lazy-repeat form min max]` : lazily repeat a form a number of times, like `{2,5}?`
  - `[:capture forms...]` : capturing group with implicit concatenation of the given forms
  - `[:char number]` : a single character, denoted by its unicode codepoint
  - `[:ctrl char]` : a control character, e.g. `[:ctrl \A]` => `^A` => `#"\cA"`
  - `[:lookahead ...]` : match if followed by pattern, without consuming input
  - `[:negative-lookahead ...]` : match if not followed by pattern
  - `[:lookbehind ...]` : match if preceded by pattern
  - `[:negative-lookbehind ...]` : match if not preceded by pattern
  - `[:atomic ...]` : match without backtracking ([atomic group](https://www.regular-expressions.info/atomic.html))
- A `clojure.spec.alpha` definition of the grammar can be made available as `:lambdaisland.regal/form` by explicitly requiring `lambdaisland.regal.spec-alpha`

You can add your own extensions (custom tokens) by providing a `:registry` option
mapping namespaced keywords to Regal expressions.

### Use with spec.alpha

``` clojure
(require '[lambdaisland.regal.spec-alpha :as regal-spec]
         '[clojure.spec.alpha :as s]
         '[clojure.spec.gen.alpha :as gen])

(s/def ::x-then-y (regal-spec/spec [:cat [:+ "x"] "-" [:+ "y"]]))

(s/def ::xy-with-stars (regal-spec/spec [:cat "*" ::x-then-y "*"]))

(s/valid? ::xy-with-stars "*xxx-yy*")
;; => true

(gen/sample (s/gen ::xy-with-stars))
;; => ("*x-y*"
;;     "*xx-y*"
;;     "*x-y*"
;;     "*xxxx-y*"
;;     "*xxx-yyyy*"
;;     "*xxxx-yyy*"
;;     "*xxxxxxx-yyyyy*"
;;     "*xx-yyy*"
;;     "*xxxxx-y*"
;;     "*xxx-yyyy*")
```

### Use with Malli

```clojure
(require '[malli.core :as m]
         '[malli.error :as me]
         '[malli.generator :as mg]
         '[lambdaisland.regal :as regal])

(def form [:+ "y"])

(def schema [:re (regal/regex form)])

(m/form schema)
;; => [:re #"y+"]

(m/type schema)
;; => :re

(m/validate schema "yyy")
;; => true

(me/humanize (m/explain schema "xxx"))
;; => ["should match regex"]

(me/humanize (m/explain schema "xxx") {:errors {:re {:error/message {:en "Pattern does not match"}}}})
;; => ["Pattern does not match"]
```

### BYO test.check / spec-alpha

Regal does not declare any dependencies. This lets people who only care about
using Regal Expressions to replace normal regexes to require
`lambdaisland.regal` without imposing extra dependencies upon them.

If you want to use `lambdaisland.regal.generator` you will require
`org.clojure/test.check`. For `lambdisland.regal.spec-alpha` you will
additionally need `org.clojure/spec-alpha`.

<!-- contributing -->
### Contributing

Everyone has a right to submit patches to this projects, and thus become a contributor.

Contributors MUST

- adhere to the [LambdaIsland Clojure Style Guide](https://nextjournal.com/lambdaisland/clojure-style-guide)
- write patches that solve a problem. Start by stating the problem, then supply a minimal solution. `*`
- agree to license their contributions as MPLv2.
- not break the contract with downstream consumers. `**`
- not break the tests.

Contributors SHOULD

- update the CHANGELOG and README.
- add tests for new functionality.

If you submit a pull request that adheres to these rules, then it will almost
certainly be merged immediately. However some things may require more
consideration. If you add new dependencies, or significantly increase the API
surface, then we need to decide if these changes are in line with the project's
goals. In this case you can start by [writing a
pitch](https://nextjournal.com/lambdaisland/pitch-template), and collecting
feedback on it.

`*` This goes for features too, a feature needs to solve a problem. State the problem it solves, then supply a minimal solution.

`**` As long as this project has not seen a public release (i.e. is not on Clojars)
we may still consider making breaking changes, if there is consensus that the
changes are justified.
<!-- /contributing -->

### Prior Art

- [irregex for Chicken Scheme](http://synthcode.com/scheme/irregex/)
- [CL-PPRE create-scanner (Common Lisp)](http://edicl.github.io/cl-ppcre/#create-scanner2)
- [test.chuck string-from-regex](https://github.com/gfredericks/test.chuck#string-from-regex)
- [rx for Emacs Lisp](https://www.emacswiki.org/emacs/rx) (and [source](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/rx.el))
- [cgrand/regex](https://github.com/cgrand/regex/)
- [ClojureVerbalExpressions](https://github.com/VerbalExpressions/ClojureVerbalExpressions)
- [FLIP](https://dl.acm.org/doi/10.1145/800005.807968), Bobrow and Teitelman, 1966

## License

Copyright &copy; 2020 Arne Brasseur

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
