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

### Support Lambda Island Open Source

If you find value in our work please consider [becoming a backer on Open Collective](http://opencollective.com/lambda-island#section-contribute)

### An example

``` clojure
(require '[lambdaisland.regal :as regal]
         '[lambdaisland.regal.generator :as regal-gen])

;; Regal expression, like Hiccup but for Regex
(def r [:cat
        [:+ [:range \a \z]]
        "="
        [:+ [:not \=]]])

;; Convert to host-specific regex
(regal/regex r)
;;=> #"(?:[a-z]+)\Q=\E(?:[^=]+)"

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
  - `[:range start end]` : match a range of characters, like `[a-z]`. Takes one-character strings or characters.
  - `[:class entries...]` : match any of the given characters or ranges, with ranges given as two element vectors. E.g. `[:class [\a \z] [\A \Z] "_" "-"]` is equivalent to `[a-zA-Z_-]`
  - `[:not entries...]` : like `:class`, but negates the result, equivalent to `[^...]`
  - `[:repeat form min max]` : repeat a form a number of times, like `{2,5}`
  - `[:capture forms...]` : capturing group with implicit concatenation of the given forms
- A `clojure.spec.alpha` definition of the grammar can be made available as `:lambdaisland.regal/form` by explicitly requiring `lambdaisland.regal.spec-alpha`

### BYO test.check

Regal does not declare a dependency on `org.clojure/test.check`. If you want to
use the generators, you need to include this dependency yourself.

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
