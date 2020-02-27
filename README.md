# Regal

_Royally reified regular expressions_

<!-- badges -->
[![CircleCI](https://circleci.com/gh/lambdaisland/regal.svg?style=svg)](https://circleci.com/gh/lambdaisland/regal) [![cljdoc badge](https://cljdoc.org/badge/lambdaisland/regal)](https://cljdoc.org/d/lambdaisland/regal) [![Clojars Project](https://img.shields.io/clojars/v/lambdaisland/regal.svg)](https://clojars.org/lambdaisland/regal)
<!-- /badges -->

Regal provides a syntax for writing regular expressions using plain Clojure
data: vectors, keywords, strings. This is known as Regal notation.

Once you have a Regal form you can either compile it to a regex pattern
(`java.util.regex.Pattern` or JavaScript `RegExp`), or you can use it to create
a Generator (see [test.check](https://github.com/clojure/test.check)) for
generating values that conform to the given pattern.

Regal is Clojure and ClojureScript compatible, and glosses over some of the
differences in Java and JavaScript regex syntax (like `\A` / `\z` vs `^` / `$`).

<!-- opencollective -->
### Support Lambda Island Open Source

If you find value in our work please consider [becoming a backer on Open Collective](http://opencollective.com/lambda-island#section-contribute)
<!-- /opencollective -->

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

### Grammar

- Strings and characters match literally. They are escaped, so `.` matches a
  period, not any character, `^` matches a caret, etc.
- A few keywords have special meaning. These are `:any` (match any character,
  like `.`), `:start` (match the start of the input), `:end` (match the end of
  the input).
- All other forms are vectors, with the first element being a keyword
  - `[:cat forms...]` : concatenation, match the given Regal expressions in order
  - `[:alt forms...]` : alternatives, match one of the given options, like `(foo|bar|baz)`
  - `[:* form]` : match the given form zero or more times
  - `[:+ form]` : match the given form one or more times
  - `[:? form]` : match the given form zero or one time
  - `[:class entries...]` : match any of the given characters or ranges, with ranges given as two element vectors. E.g. `[:class [\a \z] [\A \Z] "_" "-"]` is equivalent to `[a-zA-Z_-]`
  - `[:not entries...]` : like `:class`, but negates the result, equivalent to `[^...]`
  - `[:repeat form min max]` : repeat a form a number of times, like `{2,5}`
  - `[:capture forms...]` : capturing group with implicit concatenation of the given forms
- A `clojure.spec.alpha` definition of the grammar can be made available as `:lambdaisland.regal/form` by explicitly requiring `lambdaisland.regal.spec-alpha`

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

## License

Copyright &copy; 2020 Arne Brasseur

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
