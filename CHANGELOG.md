# Unreleased

## Added

## Fixed
- regal/malli integration didn't work with recent versions of Malli due to breaking changes in the malli API.   

## Changed

# 0.0.97 (2021-04-20 / 6f5f1cc)

## Added

- Add `lambdaisland.regal.generator/generate` as a shorthand for generating a
  single value from a regal expression

# 0.0.94 (2021-04-20 / e087c28)

## Added

- Added support for `[:repeat form num]` (so fixed number of repeats, instead of
  min/max) to the generator

# 0.0.89 (2020-07-20 / f46699b)

## Fixed

- Java flavor: parse `\s` and `\S` to to semantically equivalent forms, instead
  of incorrectly parsing to `:whitespace` / `:non-whitespace`
- All flavors: parse unkown escape codes to their respective characters
- Use the Malli error protocol so we get error messages automatically (thanks @ikitommi)
- Generators: `:any` should not generate newlines (now also for cljs)

# 0.0.80 (2020-07-17 / 3976988)

## Added

- `[:char ...]` for code point literal
- `[:ctrl ...]` for control character literals
- `:line-break`, `:alert`, `:escape`, `:vertical-whitespace`, `:vertical-tab`
- `[:(negative-)lookahead ... ]` positve/negative lookahead
- `[:(negative-)lookbehind ...]` positive/negative lookbehind
- `[:atomic ...]` atomic groups (prevent backtracking)
- Parsing of `\w \W \d \D \s \S`
- Parsing of suffixed expressions `+ * ? {1,2}`
- `lambdaisland.regal.normalize` for getting a canonicalized version of a regal form

## Fixed

- Make `:whitespace` behave consistently across platforms
- Drop the use of `java.runtime.version` (GraalVM compat)
- Make instaparse grammar work on ClojureScript
- Generator fixes
- Correctly parse a single `&` inside a bracketed character class

## Changed

- Drop `[:range from to]`, instead use `[:class [from to]]`
- Using `:whitespace` inside `[:class ...]` or `[:not ...]` will throw an
  AssertionError, since we can't support it across platforms
- The parser returns canonical forms, meaning single-character strings instead of characters

# 0.0.0 (2020-02-28 / 9a40397ba)

Initial version of Regal as presented at ClojureD. Not officially released.
