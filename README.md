# This toy, personal project, uses Parser Combinators and an NFA (Non-deterministic Finite Automatum) to parse and evaluate regular expressions against a given string.

# Please install Deno (https://docs.deno.com/runtime/manual/getting_started/installation) before using it.

# To run tests:

```
deno test regexp_nfa.ts

running 6 tests from ./regexp_nfa.ts
Basic behavior ... ok (66ms)
Multi-level (> 2) repetitions ... ok (7ms)
Anchors ... ok (15ms)
scan() ... ok (21ms)
jsMultiline on x off behavior ... ok (8ms)
greedy x lazy behavior ... ok (7ms)
```

# To use it in the REPL:

```
deno

> import * as re from 'https://raw.githubusercontent.com/cassiano/regexps/main/regexp_nfa.ts'


// Matching regexps against a string.
> re.buildNfaFromRegExpAndMatch('a*', 'aaaaaaaa', { arrows: true })
{ match: "->aaaaaaaa<-", start: 0, end: 7 }


// Matching regexps against a string, using the default "greedy" behavior.
> re.buildNfaFromRegExpAndMatch('a+', 'aaaaaaaaaaaaaaaaa', { arrows: true, greedy: true })
{ match: "->aaaaaaaaaaaaaaaaa<-", start: 0, end: 16 }


// Matching regexps against a string, using the alternative "lazy" behavior.
> re.buildNfaFromRegExpAndMatch('a+', 'aaaaaaaaaaaaaaaaa', { arrows: true, greedy: false })
{ match: "->a<-aaaaaaaaaaaaaaaa", start: 0, end: 0 }


// Scanning a string against a regexp and collect all results.
> re.scan('/d{2}', '1234567890')
[ "12", "34", "56", "78", "90" ]


// Scanning a string using default (JavaScript's like) multiline behavior.
> re.scan('^.', 'regexps\nare\nreally\ncool', { jsMultiline: true })
[ "r", "a", "r", "c" ]


// Scanning a string using alternative (JavaScript's like) singleline behavior.
> re.scan('^.', 'regexps\nare\nreally\ncool', { jsMultiline: false })
[ "r" ]


// Showing the generated NFA.
> const nfa = re.buildNfaFromRegExp('a*'); re.print(nfa)
RegExp: a*

AST:

[
  {
    type: "repetition",
    expr: { type: "singleChar", character: "a" },
    limits: { min: 0, max: Infinity }
  }
]

NFA:

<ref *1> {
  type: "CNode",
  id: 0,
  next: { type: "NNode", id: 1, character: "a", isLiteral: false, next: [Circular *1] },
  nextAlt: { type: "ENode", id: 0 }
}

NFA nodes generated:

nNodeCount: 2, cNodeCount: 1


// Showing the generated AST (Abstract Syntax Tree) only.
> const ast = re.buildRegExpAst('a*'); re.print(ast)
RegExp: a*
[
  {
    type: "repetition",
    expr: { type: "singleChar", character: "a" },
    limits: { min: 0, max: Infinity }
  }
]
```
