# Regular Expression Engine

## This personal project uses Parser Combinators and an NFA (Non-deterministic Finite Automaton) to parse and evaluate regular expressions against a given string. The NFA implementation is based on this article by Ken Thompson: https://dl.acm.org/doi/epdf/10.1145/363347.363387

## Pre-requisites:

Please install Deno (https://docs.deno.com/runtime/manual/getting_started/installation) before using it.

## Features include:

- Greeedy matches by default
- Lazy and possessive match modes fully supported
- Really fast backtracking, using an NFA
- All repetition quantifiers (e.g. \*, +, ?, {m,n}, {m,}, {,n}, {m})
- Alternations (e.g. 'a|b')
- Parenthesized expressions (e.g. '(a|(bc)+)\*')
- Character classes, both default (e.g. [a-z0-9]) and negated (e.g. [^a-z0-9])
- Character class abbreviations, both default (e.g. \d, \w, \h etc) and negated (e.g. \D, W, \H etc)
- Additional (non-standard) character class abbreviations (e.g. \o for Octal numbers, \y for binarY, \l for Lowercase characters, \u for Uppercase characters and \a for Alphabetic [lower or uppercase] characters)
- Anchors (\b, ^ and $)
- `scan()` function, for returning all matches of a regexp over a given string

## To run tests:

```
✗ deno test regexp_nfa.ts

Check file:///.../regexp_nfa.ts
running 14 tests from ./regexp_nfa.ts
Repetitions (v1) ... ok (35ms)
Repetitions which can potentially match an empty string (v1) ... ok (27ms)
Complex repetitions (v1) ... ok (334ms)
Anchors (v1) ... ok (27ms)
scan() (v1) ... ok (187ms)
jsMultiline on x off behavior (v1) ... ok (0ms)
Greedy (default) x lazy behavior (v1) ... ok (1ms)
Backtrackable (default) x possessive behavior (v1) ... ok (5ms)
Repetitions (v2) ... ok (9ms)
Repetitions which can potentially match an empty string (v2) ... ok (6ms)
Complex repetitions (v2) ... ok (8s)
Anchors (v2) ... ok (0ms)
scan() (v2) ... ok (38ms)
jsMultiline on x off behavior (v2) ... ok (0ms)

ok | 14 passed | 0 failed (9s)
```

### To use it in the REPL:

```ts
deno

// Importing locally.
> import * as re from './regexp_nfa.ts'

// Or, importing directly from Github.
> import * as re from 'https://raw.githubusercontent.com/cassiano/regexps/main/regexp_nfa.ts'


// Matching regexps against a string.
> re.buildNfaFromRegExpAndMatch('a*', 'aaaaaaaa', { arrows: true })
{ match: "->aaaaaaaa<-", start: 0, end: 7 }


// Matching regexps against a string, using the default "greedy" behavior.
> re.buildNfaFromRegExpAndMatch('a+', 'aaaaaaaaaaaaaaaaa', { arrows: true })
{ match: "->aaaaaaaaaaaaaaaaa<-", start: 0, end: 16 }


// Matching regexps against a string, using the alternative "lazy" behavior.
> re.buildNfaFromRegExpAndMatch('a+?', 'aaaaaaaaaaaaaaaaa', { arrows: true })
{ match: "->a<-aaaaaaaaaaaaaaaa", start: 0, end: 0 }


// Matching regexps against a string, using the default "greedy" behavior.
> re.buildNfaFromRegExpAndMatch('a+a', 'aaaaaaaaaaaaaaaaa', { arrows: true })
{ match: "->aaaaaaaaaaaaaaaaa<-", start: 0, end: 16 }


// Matching regexps against a string, using the alternative "possessive" behavior.
> re.buildNfaFromRegExpAndMatch('a++a', 'aaaaaaaaaaaaaaaaa', { arrows: true })
"(sorry, no match)"


// Scanning a string against a regexp.
> re.scan_v1('\\d{2}', '1234567890')
[ "12", "34", "56", "78", "90" ]


// Scanning a string using the default multiline behavior, similar to how it works by default
// in Ruby.
> re.scan_v1('^.', 'regexps\nare\nreally\ncool', { jsMultiline: true })
[ "r", "a", "r", "c" ]


// Scanning a string using the alternative singleline behavior, similar to how it works by
// default in JavaScript.
> re.scan_v1('^.', 'regexps\nare\nreally\ncool', { jsMultiline: false })
[ "r" ]


// Showing the generated AST (Abstract Syntax Tree).
> const ast = re.buildRegExpAst('a*'); re.print(ast)
[
  {
    type: "repetition",
    expr: { type: "singleChar", character: "a" },
    limits: { min: 0, max: Infinity }
  }
]


// Showing the generated NFA.
> const nfa = re.buildNfaFromRegExp('a*'); re.print(nfa)
<ref *1> {
  type: "CNode",
  id: 0,
  next: { type: "NNode", id: 1, character: "a", isLiteral: false, next: [Circular *1] },
  nextAlt: { type: "ENode", id: 0 }
}
```
