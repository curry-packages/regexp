regexp: Regular expression matching
===================================

This package contains libraries dealing with regular expressions:

* `RegExpSem`: This library specifies the semantics of
  regular expressions and regular expression matching in a high-level
  manner via non-deterministic operations. It might be useful
  as an oracle to test more efficient regular expression matchers.
* `RegExp`:  This library defines a structure of regular expressions
  and some operations like match and capture for regular expression.
  Since the implementation is high-level but quite inefficient,
  it can only be used for smaller string matching.
* `RegExpEff`:  This library defines a structure of regular expressions
  and some efficient operations like `match` and `capture` for larger regular 
  expression matching.

Note that the libraries `RegExp` and `RegExpEff` are useful in combination
with the Curry preprocessor to translate integrated code in the form of
POSIX extended regular expressions into Curry programs.
See example program `TestRegExpCode.curry` in directory `test`
or the examples in the package of the Curry preprocessor.

--------------------------------------------------------------------------
