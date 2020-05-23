# Unreleased

## Added

- `[:char ...]` for code point literal
- `[:ctrl ...]` for control character literals
- `:line-break`, `:alert`, `:escape`, `:vertical-whitespace`, `:vertical-tab`
- `[:(negative-)lookahead ... ]` positve/negative lookahead
- `[:(negative-)lookbehind ...]` positive/negative lookbehind
- `[:atomic ...]` atomic groups (prevent backtracking)
- Parsing of `\w \W \d \D \s \S`
- Parsing of suffixed expressions `+ * ? {1,2}`

## Fixed

- Make `:whitespace` behave consistently across platforms
- Drop the use of `java.runtime.version` (GraalVM compat)
- Make instaparse grammar work on ClojureScript
- Generator fixes
- Correctly parse a single `&` inside a bracketed character class

## Changed

- Drop `[:range from to]`, instead use `[:class [from to]]`

# 2020-02-28 (9a40397ba)

Initial version of Regal as presented at ClojureD.
