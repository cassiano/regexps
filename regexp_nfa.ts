import { assertEquals } from 'https://deno.land/std@0.201.0/testing/asserts.ts'

import {
  Parser,
  and,
  char,
  closeParens,
  comma,
  delimitedBy,
  digit,
  joinedBy,
  letter,
  many1,
  map,
  natural,
  openParens,
  optional,
  or,
  isError,
  or3,
  ParserResult,
  andN,
  charRange,
  orN,
  concat,
  PERIOD,
  EMPTY_STRING,
  SingleChar,
  many,
  succeededBy,
  allButCharSet,
  none1,
  or4,
  charSequence,
  precededBy,
  error,
  memoize,
  allButChar,
  manyN,
} from '../reactive-spreadsheet/src/parser_combinators.ts'

//////////////////
// Global state //
//////////////////

let debugMode = true

//////////////////////////
// Types and interfaces //
//////////////////////////

type CharacterClassRangeType = { from: SingleChar; to: SingleChar }
type CharacterClassOptionsType = (SingleChar | CharacterClassRangeType)[]

type RepetitionLimitsType = {
  min: number
  max: number
}

type SingleCharType = {
  type: 'singleChar'
  character: SingleChar
}

type CharacterClassType = {
  type: 'characterClass'
  negated: boolean
  options: CharacterClassOptionsType
}
type ParenthesizedType = {
  type: 'parenthesized'
  expr: RegExpType
}
type AlternationType = {
  type: 'alternation'
  left: RegExpType
  right: RegExpType
}
type RepetitionType = {
  type: 'repetition'
  expr: SingleCharType | CharacterClassType | ParenthesizedType
  limits: RepetitionLimitsType
}

type RegExpTokenType =
  | SingleCharType
  | CharacterClassType
  | ParenthesizedType
  | RepetitionType
  | AlternationType

type RegExpType = RegExpTokenType[]

const QUANTIFIERS: { [quantifier: SingleChar]: RepetitionLimitsType } = {
  '*': { min: 0, max: Infinity },
  '+': { min: 1, max: Infinity },
  '?': { min: 0, max: 1 },
}

const alternation = char('|')

const alternativeTerm: Parser<RegExpTokenType> = memoize(input =>
  or(
    map(and(succeededBy(many(factor), alternation), many(alternativeTerm)), ([left, right]) => ({
      type: 'alternation' as const,
      left,
      right,
    })),
    factor
  )(input)
)

const regExp: Parser<RegExpType> = many1(alternativeTerm)

const singleChar: Parser<SingleCharType> = memoize(
  map(
    allButCharSet('|{}[]()' + Object.keys(QUANTIFIERS).join(EMPTY_STRING)),
    character => ({ type: 'singleChar', character } as SingleCharType)
  )
)

const dash = char('-')
const characterClassChar = allButChar(']')

const characterClassOption: Parser<string | CharacterClassRangeType> = memoize(
  or(
    map(or(joinedBy(letter, dash), joinedBy(digit, dash)), range => ({
      from: range[0].toString(),
      to: range[1].toString(),
    })),
    characterClassChar
  )
)

const CARET = '^'

const characterClass: Parser<CharacterClassType> = memoize(
  map(
    delimitedBy(char('['), and(optional(char(CARET)), many1(characterClassOption)), char(']')),
    ([caret, options]) => ({
      type: 'characterClass',
      negated: caret === CARET,
      options,
    })
  )
)

const parenthesized: Parser<ParenthesizedType> = memoize(
  map(
    delimitedBy(openParens, precededBy(optional(charSequence('?:')), regExp), closeParens),
    expr => ({
      type: 'parenthesized' as const,
      expr,
    })
  )
)

const quantifier: Parser<RepetitionLimitsType> = memoize(
  map(
    or(
      orN(Object.keys(QUANTIFIERS).map(charSequence)),
      delimitedBy(char('{'), or(joinedBy(optional(natural), comma), natural), char('}'))
    ),
    result =>
      typeof result === 'string'
        ? QUANTIFIERS[result as keyof typeof QUANTIFIERS]
        : typeof result === 'number'
        ? { min: result, max: result }
        : {
            min: result[0] === EMPTY_STRING ? 0 : result[0],
            max: result[1] === EMPTY_STRING ? Infinity : result[1],
          }
  )
)

const repetition: Parser<RepetitionType> = memoize(
  map(and(or3(singleChar, characterClass, parenthesized), quantifier), ([expr, limits]) => ({
    type: 'repetition',
    expr,
    limits,
  }))
)

const factor: Parser<RegExpTokenType> = memoize(
  or4(repetition, singleChar, characterClass, parenthesized)
)

const DOLLAR_SIGN = '$'
const NEW_LINE = '\n'

const endOfString: Parser<string> = memoize(input =>
  input.length === 0 ? ['', ''] : [error('Input not empty'), input]
)

export const evaluateRegExpToken =
  (token: RegExpTokenType): Parser<string> =>
  input => {
    switch (token.type) {
      case 'singleChar': {
        let parser: Parser<string>

        switch (token.character) {
          case PERIOD:
            parser = allButChar(NEW_LINE)
            break
          case DOLLAR_SIGN:
            parser = endOfString
            break
          default:
            parser = char(token.character)
        }

        const [result, rest] = parser(input)

        debug(() => `Trying to match '${token.character}' against '${input}'`)

        if (!isError(result)) debug(() => `Matched singleChar: '${result}'`)

        return [result, rest]
      }

      case 'parenthesized':
        return concat(andN(token.expr.map(evaluateRegExpToken)))(input)

      case 'characterClass': {
        const optionsParser = token.options.map(option =>
          typeof option === 'string' ? char(option) : charRange(option.from, option.to)
        )

        return (!token.negated ? orN(optionsParser) : none1(optionsParser))(input)
      }

      case 'repetition': {
        return concat(manyN(evaluateRegExpToken(token.expr), token.limits))(input)
      }

      case 'alternation':
        return concat(
          or(andN(token.left.map(evaluateRegExpToken)), andN(token.right.map(evaluateRegExpToken)))
        )(input)

      default: {
        const _exhaustiveCheck: never = token
        throw new Error('Invalid regular expression type')
      }
    }
  }

const CHARACTER_CLASS_ABBREVIATIONS: { [index: SingleChar]: string } = {
  d: '[0-9]', // d = Decimal digit
  h: '[0-9a-fA-F]', // h = Hexadecimal digit
  w: '[0-9a-zA-Z_]', // w = Word character
  s: '[ \t\r\n\f]', // s = Space
  r: '[\r\n]', // r = carriage Return
  y: '[0-1]', // y = binarY digit (non-standard)
  o: '[0-7]', // o = Octal digit (non-standard)
  l: '[a-z]', // l = Lowercase character (non-standard)
  u: '[A-Z]', // u = Uppercase character (non-standard)
  a: '[a-zA-Z]', // a = Alphabetic (lower or uppercase) character (non-standard)
}

const replaceCharacterClassAbbreviations = (regExpAsString: string): string => {
  Object.entries(CHARACTER_CLASS_ABBREVIATIONS).forEach(([abbrev, characterClass]) => {
    // Create regular abbreviations (\d, \h, \w etc, as well as /d, /h, /w etc).
    regExpAsString = regExpAsString
      .replaceAll(`\\${abbrev}`, characterClass)
      .replaceAll(`/${abbrev}`, characterClass)

    // Create "negated" upper-cased abbreviations (\D, \H, \W etc, as well as /D, /H, /W etc).
    abbrev = abbrev.toUpperCase()
    characterClass = characterClass.slice(0, 1) + CARET + characterClass.slice(1)
    regExpAsString = regExpAsString
      .replaceAll(`\\${abbrev}`, characterClass)
      .replaceAll(`/${abbrev}`, characterClass)
  })

  return regExpAsString
}

const replaceEscapedChars = (regExpAsString: string): string =>
  regExpAsString.replaceAll(/[/\\](.)/g, '[$1]')

export const buildRegExpAST = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(
    replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))
  )

  debug(() => `RegExp: ${replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))}`)

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  return result
}

export const regExpParserFromAST = memoize(
  (ast: RegExpType): Parser<string> => concat(andN(ast.map(evaluateRegExpToken)))
)

export const buildAndMatch = (
  regExpAsString: string,
  input: string,
  exactMatch = false
): ParserResult<string> => {
  let result: string | Error

  if (regExpAsString.startsWith(CARET)) {
    regExpAsString = regExpAsString.slice(1)
    exactMatch = true
  }

  const ast = buildRegExpAST(regExpAsString)
  const parser = regExpParserFromAST(ast)

  // Try to match the regular expression from left to right.
  for (let i = 0; i < (exactMatch ? 1 : input.length); i++) {
    let rest: string

    const slicedInput = input.slice(i)

    ;[result, rest] = parser(slicedInput)

    if (!isError(result)) return [result, rest]
  }

  return [result!, input]
}

export const scan = (regExpAsString: string, input: string): string[] => {
  let rest = input
  const matches = []

  while (true) {
    const [result, remaining] = buildAndMatch(regExpAsString, rest)

    if (!isError(result)) {
      matches.push(result)

      rest = remaining.length < rest.length ? remaining : remaining.slice(1)
    }

    if (isError(result) || remaining === EMPTY_STRING) break
  }

  return matches.flat()
}

declare const Deno: {
  inspect: (...args: unknown[]) => void
  stdin: { read: (...args: unknown[]) => void }
}

export const log = console.log

export const inspect = (value: any) =>
  Deno.inspect(value, { depth: 999, colors: true }) as unknown as string
export const print = (value: object) => log(inspect(value))

export const showRegExp = (regExpAsString: string) => print(buildRegExpAST(regExpAsString))

export const groupBy = <T>(collection: T[], fn: (prop: T) => string | number) =>
  collection.reduce((acc: { [key: string | number]: T[] }, obj: T) => {
    acc[fn(obj)] ??= []
    acc[fn(obj)].push(obj)
    return acc
  }, {})

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)

export const debug = (messageOrFalse: () => string | false): void => {
  if (debugMode) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

///////////
// Tests //
///////////

const assertMatches = (
  regExpAsString: string,
  input: string,
  matchedResult: ParserResult<string>,
  exactMatch = false
) => assertEquals(buildAndMatch(regExpAsString, input, exactMatch), matchedResult)

const previousDebugMode = debugMode
debugMode = false

log('Running tests...')

assertMatches('an+', 'banana', ['an', 'ana'])
assertMatches('(an)+', 'banana', ['anan', 'a'])
assertMatches('iss', 'mississipi', ['iss', 'issipi'])
assertMatches('(iss)+', 'mississipi', ['ississ', 'ipi'])
assertMatches('is+', 'mississipi', ['iss', 'issipi'])
assertMatches('(is+)+', 'mississipi', ['ississ', 'ipi'])
assertMatches('/d{2}/D/d{2}/s*([ap]m)', '"12:50 am', ['12:50 am', ''])
assertMatches('a*', '...aa', ['', '...aa'])
assertMatches('a*', 'aa', ['aa', ''])
assertMatches('a+', '...aa', ['aa', ''])
// assertMatches('(x+x+)+y', 'xxxxxxxxxxy', ['xxxxxxxxxxy', ''], true)

// assertIsError(buildAndMatch('(x+x+)+y', 'xxxxxxxxxx', true).match[0]) // 558 steps

// assertMatches('(a+)*ab', 'aaaaaaaaaaaab', ['aaaaaaaaaaaab', ''], true) // 2050 steps
// assertMatches('.*.*=.*', 'x=x', ['x=x', ''], true) // 6 steps

assertEquals(
  scan(
    '/w+(/./w+)*@/w+(/./w+)+',
    '| john.doe@gmail.com | john@gmail.com.us | john.doe@ | @gmail.com | john@gmail | jo.hn.do.e@g.mail.co.m |'
  ),
  ['john.doe@gmail.com', 'john@gmail.com.us', 'jo.hn.do.e@g.mail.co.m']
)

log('Done!')

debugMode = previousDebugMode

// import * as re from './regexp_nfa.ts'; import * as pc from '../reactive-spreadsheet/src/parser_combinators.ts'
//
// re.buildAndMatch('m(is+)+is', 'mississipi')   // DOES NOT WORK!
//
// re.debugRegExp('(x+x+)+y', 'xxxxxxxxxx')
//
// const ast = re.buildRegExpAST('(x+x+)+y')
// const parser = re.regExpParserFromAST(ast)
// re.print(ast)
// parser('xxxxxxxxxx')

type CNodeType = { type: 'CNode'; id: number; next?: NodeType | null; nextAlt?: NodeType | null }
type NNodeType = {
  type: 'NNode'
  id: number
  character: SingleChar
  escaped: boolean
  next?: NodeType | null
}
type NodeType = NNodeType | CNodeType
type previousNavigationPropType = 'next' | 'nextAlt'

type CreateNNodeOptionsType = {
  next?: NodeType | null
  previous?: NodeType
  previousProp?: previousNavigationPropType
  escaped?: boolean
}

type CreateCNodeOptionsType = {
  previous?: NodeType
  previousProp?: previousNavigationPropType
}

let nNodeCount = 0
let cNodeCount = 0

export const createNNode = (
  character: SingleChar,
  { next, previous, previousProp, escaped = false }: CreateNNodeOptionsType = {}
) => {
  const node: NNodeType = { type: 'NNode', id: nNodeCount++, character, escaped, next }

  if (previous)
    if (previous.type === 'NNode') previous.next = node
    else previous[previousProp ?? 'next'] = node

  return node
}

export const createCNode = (
  next: NodeType | null | undefined,
  nextAlt: NodeType | null | undefined,
  { previous, previousProp }: CreateCNodeOptionsType = {}
) => {
  const node: CNodeType = { type: 'CNode', id: cNodeCount++, next, nextAlt }

  if (previous)
    if (previous.type === 'NNode') previous.next = node
    else previous[previousProp ?? 'next'] = node

  return node
}

// Maps a character class range into an array of its individual constituint characters. E.g.:
// takes the 1st range in '[a-dxyz]' ('a-d'), and transforms it into [ "a", "b", "c", "d" ].
const mapCharacterClassRange = memoize((range: CharacterClassRangeType) =>
  times(range.to.charCodeAt(0) - range.from.charCodeAt(0) + 1, i =>
    String.fromCharCode(range.from.charCodeAt(0) + i)
  )
)

// Maps a character options into an array of its individual constituint characters. E.g.:
// takes '[a-dxyz]' and transforms it into: [ "a", "b", "c", "d", "x", "y", "z" ].
const mapCharacterClassOptions = memoize((options: CharacterClassOptionsType) =>
  options.flatMap(option => (typeof option === 'string' ? option : mapCharacterClassRange(option)))
)

// FIXME: stack overflow when re.buildAndMatch2('/w+(/./w+)*@/w+(/./w+)+', 'john.doe@gmail.com').
const innerNext = (
  node: NodeType,
  breadcrumbs: (NodeType | null | undefined)[] = []
): NodeType | null | undefined => (
  breadcrumbs.push(node),
  node.next !== undefined && node.next !== null && breadcrumbs.indexOf(node.next) === -1
    ? innerNext(node.next, breadcrumbs)
    : node.next
)

const setInnerNext = (
  node: NodeType,
  newNext: NodeType | null | undefined,
  breadcrumbs: (NodeType | null | undefined)[] = []
): void => {
  breadcrumbs.push(node)

  if (node.next !== undefined && node.next !== null && breadcrumbs.indexOf(node.next) === -1) {
    setInnerNext(node.next, newNext, breadcrumbs)
  } else {
    node.next = newNext
  }
}

const cloneNode = (
  node: NodeType | null | undefined,
  newNext: NodeType | null | undefined
): NodeType | null | undefined => {
  debug(() => `Cloning node ${inspect(node)}`)

  if (node === null || node === undefined) return node

  let next: NodeType | null | undefined
  let cycleDetected = false

  if (node.next !== undefined) {
    if (node.next !== null && innerNext(node) === node) {
      debug(() => `Cycle detected`)

      cycleDetected = true
      setInnerNext(node, undefined)
      next = cloneNode(node.next, undefined)
      node.next.next = node
    } else {
      next = cloneNode(node.next, newNext)
    }
  } else {
    next = newNext
  }

  let clone: NodeType | null | undefined

  switch (node.type) {
    case 'NNode':
      clone = createNNode(node.character, {
        next,
      })
      break

    case 'CNode':
      clone = createCNode(
        next,
        node.nextAlt !== undefined ? cloneNode(node.nextAlt, newNext) : newNext
      )
      break

    default: {
      const _exhaustiveCheck: never = node
      throw new Error('Invalid NFA node type')
    }
  }

  if (cycleDetected) clone.next!.next = clone

  debug(() => `Clone of node #${node.id}: ${inspect(clone)}`)

  return clone
}

export const createNfaNodeFromRegExp = (ast: RegExpType, nextNode?: NodeType | null): NodeType => {
  let node: NodeType | null | undefined = nextNode

  for (let i = ast.length - 1; i >= 0; i--) {
    node = createNfaNodeFromRegExpToken(ast[i], node)
  }

  return node!
}

export const createNfaNodeFromRegExpToken = (
  astNode: RegExpTokenType,
  nextNode?: NodeType | null
): NodeType => {
  switch (astNode.type) {
    case 'singleChar':
      return createNNode(astNode.character, { next: nextNode })
      break

    case 'alternation':
      return createCNode(
        createNfaNodeFromRegExp(astNode.left, nextNode),
        createNfaNodeFromRegExp(astNode.right, nextNode)
      )

    case 'parenthesized':
      return createNfaNodeFromRegExp(astNode.expr, nextNode)

    case 'characterClass': {
      // Non-negative (default) case for character class '[a-dxyz]':
      //
      // {
      //   type: "CNode",
      //   id: 16,
      //   next: { type: "NNode", id: 18, character: "a", escaped: false, next: undefined },
      //   nextAlt: {
      //     type: "CNode",
      //     id: 15,
      //     next: { type: "NNode", id: 17, character: "b", escaped: false, next: undefined },
      //     nextAlt: {
      //       type: "CNode",
      //       id: 14,
      //       next: { type: "NNode", id: 16, character: "c", escaped: false, next: undefined },
      //       nextAlt: {
      //         type: "CNode",
      //         id: 13,
      //         next: { type: "NNode", id: 15, character: "d", escaped: false, next: undefined },
      //         nextAlt: {
      //           type: "CNode",
      //           id: 12,
      //           next: { type: "NNode", id: 14, character: "x", escaped: false, next: undefined },
      //           nextAlt: {
      //             type: "CNode",
      //             id: 11,
      //             next: { type: "NNode", id: 13, character: "y", escaped: false, next: undefined },
      //             nextAlt: { type: "NNode", id: 12, character: "z", escaped: false, next: undefined }
      //           }
      //         }
      //       }
      //     }
      //   }
      // }
      //
      // Negative case for character class '[a-dxyz]':
      //
      // {
      //   type: "CNode",
      //   id: 24,
      //   next: { type: "NNode", id: 27, character: "a", escaped: false, next: null },
      //   nextAlt: {
      //     type: "CNode",
      //     id: 23,
      //     next: { type: "NNode", id: 26, character: "b", escaped: false, next: null },
      //     nextAlt: {
      //       type: "CNode",
      //       id: 22,
      //       next: { type: "NNode", id: 25, character: "c", escaped: false, next: null },
      //       nextAlt: {
      //         type: "CNode",
      //         id: 21,
      //         next: { type: "NNode", id: 24, character: "d", escaped: false, next: null },
      //         nextAlt: {
      //           type: "CNode",
      //           id: 20,
      //           next: { type: "NNode", id: 23, character: "x", escaped: false, next: null },
      //           nextAlt: {
      //             type: "CNode",
      //             id: 19,
      //             next: { type: "NNode", id: 22, character: "y", escaped: false, next: null },
      //             nextAlt: {
      //               type: "CNode",
      //               id: 18,
      //               next: { type: "NNode", id: 19, character: "z", escaped: false, next: null },
      //               nextAlt: {
      //                 type: "CNode",
      //                 id: 17,
      //                 next: { type: "NNode", id: 20, character: ".", escaped: false, next: undefined },
      //                 nextAlt: { type: "NNode", id: 21, character: "\n", escaped: false, next: undefined }
      //               }
      //             }
      //           }
      //         }
      //       }
      //     }
      //   }
      // }

      const options = mapCharacterClassOptions(astNode.options)
      let lastNode: NodeType = createNNode(options.at(-1)!, { next: nextNode })

      if (astNode.negated) {
        const periodNode = createNNode(PERIOD, { next: nextNode }) // All but "\n"
        const newLineNode = createNNode(NEW_LINE, { next: nextNode }) // Only "\n"
        const catchAllNode = createCNode(periodNode, newLineNode)

        lastNode.next = null // next = null means "no match!".
        lastNode = createCNode(lastNode, catchAllNode)
        nextNode = null
      }

      let accNode: NodeType = lastNode

      for (let i = options.length - 2; i >= 0; i--) {
        const node = createNNode(options[i], { next: nextNode })

        accNode = createCNode(node, accNode)
      }

      return accNode
    }

    case 'repetition': {
      // Notice below that:
      //
      // ▬▶[a] or ▬▶[†]: initial states
      // [b]▬▶ or ◀▬[b]: end states
      // - [a], [b]: NNodes or CNodes
      // - [†]: CNode
      // - m, n: natural numbers
      //
      // Possible cases:
      //
      // -------------------------------------------------------------------
      //
      // a+b ≅ a{1,}b ≅ a{1,∞}b: ▬▶[a] ⮂ [†]
      //                                   ↓
      //                                  [b]▬▶
      //
      // -------------------------------------------------------------------
      //
      // a*b ≅ a{0,}b ≅ a{0,∞}b: ▬▶[†] ⮂ [a]
      //                            ↓
      //                           [b]▬▶
      //
      // -------------------------------------------------------------------
      //
      // a?b ≅ a{0,1}b: ▬▶[†] ⭢ [a]
      //                    ↓  ↙
      //                    [b]▬▶
      //
      // -------------------------------------------------------------------
      //
      // a{m}b ≅ a{m,m}b: ▬▶[a] ⭢ [a] ⭢ [a] ⭢ … [a] ⭢ [b]▬▶
      //                     ▙▃▃▃▃▃ (m times) ▃▃▃▃▃▟
      //
      // -------------------------------------------------------------------
      //
      // a{m,}b ≅ a{m,∞}b ≅ ▬▶[a] ⭢ [a] ⭢ [a] ⭢ … [a] ⮂ [†]
      //                       ▙▃▃▃▃▃ (m times) ▃▃▃▃▃▟      ↓
      //                                                   [b]▬▶
      //
      // -------------------------------------------------------------------
      //
      // a{,n}b ≅ a{0,n}b ≅ ▬▶[†] ⭢ [a]   ▜
      //                       ↓      ↓    ▐
      //                    ◀▬[b] ⭠ [†]   ▐
      //                       ⭡     ↓    ▐
      //                       │     [a]   ▐
      //                       │      ↓    ▐
      //                       ├──── [†]   ▐
      //                       │      ↓   (n times)
      //                       │     [a]   ▐
      //                       │      ↓    ▐
      //                       ├──── [†]   ▐
      //                       │      ↓    ▐
      //                       │      …    ▐
      //                       └──── [a]   ▟
      //
      // -------------------------------------------------------------------
      //
      // a{m,n}b ≅ ▬▶[a] ⭢ [a] ⭢ [a] ⭢ … [a] ⭢ [†] ⭢ [a]   ▜
      //              ▙▃▃▃▃▃ (m times) ▃▃▃▃▃▟      ↓     ↓     ▐
      //                                        ◀▬[b] ⭠ [†]   ▐
      //                                           ⭡     ↓    ▐
      //                                           │     [a]   ▐
      //                                           │      ↓    ▐
      //                                           ├──── [†]   ▐
      //                                           │      ↓   (n-m times)
      //                                           │     [a]   ▐
      //                                           │      ↓    ▐
      //                                           ├──── [†]   ▐
      //                                           │      ↓    ▐
      //                                           │      …    ▐
      //                                           └──── [a]   ▟

      const limits = astNode.limits
      const repeatingNode = createNfaNodeFromRegExpToken(astNode.expr)

      let rightNNodeNext: NodeType | null | undefined = nextNode
      let rightCNode: NodeType | null | undefined = nextNode

      if (limits.max !== Infinity) {
        times(limits.max - limits.min, () => {
          const rightNNode = cloneNode(repeatingNode, rightNNodeNext)

          rightCNode = createCNode(rightNNode, nextNode)

          rightNNodeNext = rightCNode
        })
      } // limits.max === Infinity
      else {
        rightCNode = createCNode(undefined, nextNode)

        const rightNNode = cloneNode(repeatingNode, rightCNode)

        rightCNode.next = rightNNode
      }

      let leftNNodeNext: NodeType | null | undefined = rightCNode

      times(limits.min, () => {
        const leftNNode = cloneNode(repeatingNode, leftNNodeNext)

        leftNNodeNext = leftNNode
      })

      return leftNNodeNext!
    }

    default: {
      const _exhaustiveCheck: never = astNode
      throw new Error('Invalid AST node type')
    }
  }
}

export const buildRegExpASTAndCreateNfaNodeFromRegExp = (regExpAsString: string): NodeType => {
  const previousNNodeCount = nNodeCount
  const previousCNodeCount = cNodeCount

  const ast = buildRegExpAST(regExpAsString)
  debug(() => `\nAST: \n\n${inspect(ast)}`)

  const nfa = createNfaNodeFromRegExp(ast)
  debug(() => `\nNFA: \n\n${inspect(nfa)}`)

  debug(
    () =>
      `\nNFA nodes generated: \n\nnNodeCount: ${nNodeCount - previousNNodeCount}, cNodeCount: ${
        cNodeCount - previousCNodeCount
      }`
  )

  return nfa
}

export const matchNfa = (
  nfa: NodeType | null | undefined,
  input: string,
  index = 0
): { matched: boolean; input: string; index: number } => {
  if (nfa === undefined) return { matched: true, input, index }
  if (nfa === null) return { matched: false, input, index } // throw new Error('Sorry, no match!')

  switch (nfa.type) {
    case 'NNode': {
      const currentChar = input[0]
      const rest = input.slice(1)

      debug(
        () =>
          `[input: '${input}', index: ${index}] Trying to match character '${
            currentChar ?? ''
          }' against NNode #${nfa.id}`
      )

      switch (nfa.character) {
        case CARET:
          if (index === 0) return debug(() => 'Passed!'), matchNfa(nfa.next, input, index)
          break
        case DOLLAR_SIGN:
          if (input.length === 0) return debug(() => 'Passed!'), { matched: true, input, index }
          break
        case PERIOD:
          if (currentChar !== NEW_LINE && input.length > 0)
            return debug(() => 'Passed!'), matchNfa(nfa.next, rest, index + 1)
          break
        default:
          if (currentChar === nfa.character)
            return debug(() => 'Passed!'), matchNfa(nfa.next, rest, index + 1)
      }
      break
    }

    case 'CNode':
      debug(() => `[input: '${input}', index: ${index}] Trying to match against CNode #${nfa.id}`)

      let match = matchNfa(nfa.next, input, index)

      if (match.matched)
        return (
          debug(() => `[input: '${input}', index: ${index}] Passed CNode #${nfa.id}'s next path!`),
          match
        )
      else {
        match = matchNfa(nfa.nextAlt, input, index)

        if (match.matched)
          return (
            debug(
              () => `[input: '${input}', index: ${index}] Passed CNode #${nfa.id}'s nextAlt path!`
            ),
            match
          )
      }

      break

    default: {
      const _exhaustiveCheck: never = nfa
      throw new Error('Invalid NFA node type')
    }
  }

  return { matched: false, input, index }
}

export const buildAndMatch2 = (
  regExpAsString: string,
  input: string,
  exactMatch = false
): { match: string; index: number } | string => {
  const nfa = buildRegExpASTAndCreateNfaNodeFromRegExp(regExpAsString)

  // Try to match the regular expression from left to right.
  for (let index = 0; index < (exactMatch || input.length === 0 ? 1 : input.length); index++) {
    let rest: string

    const slicedInput = input.slice(index)

    const match = matchNfa(nfa, slicedInput, index)

    debug(() => `match: ${inspect(match)}`)

    if (match.matched) {
      return {
        match: [
          input.slice(0, index),
          '->',
          input.slice(index, match.index),
          '<-',
          input.slice(match.index),
        ].join(EMPTY_STRING),
        index,
      }
    }
  }

  return '(sorry, no match)'
}

// import * as re from './regexp_nfa.ts'; import * as pc from '../reactive-spreadsheet/src/parser_combinators.ts'
// const regExpAsString = 'a*b'; const ast = re.buildRegExpAST(regExpAsString); const nfa = re.createNfaNodeFromRegExp(ast); re.print(nfa)

// a+b:
// const node1 = re.createNNode('a')
// const node3 = re.createNNode('b')
// const node2 = re.createCNode(node1, node3, { previous: node1 })
// re.print(node1)

// a*b:
// const node2 = re.createNNode('a')
// const node3 = re.createNNode('b')
// const node1 = re.createCNode(node2, node3)
// node2.next = node1
// re.print(node1)

// a?b:
// const node2 = re.createNNode('a')
// const node3 = re.createNNode('b', { previous: node2 })
// const node1 = re.createCNode(node2, node3)
// re.print(node1)

// a{2}b:
// const node1 = re.createNNode('a')
// const node2 = re.createNNode('a', { previous: node1 })
// const node3 = re.createNNode('b', { previous: node2 })
// re.print(node1)

// a{2,}b:
// const node1 = re.createNNode('a')
// const node2 = re.createNNode('a', { previous: node1 })
// const node3 = re.createNNode('b')
// const node4 = re.createCNode(node2, node3, { previous: node2 })
// re.print(node1)

// a{1,2}b:
// const node1 = re.createNNode('a')
// const node2 = re.createNNode('a')
// const node3 = re.createNNode('b', { previous: node2 })
// const node4 = re.createCNode(node2, node3, { previous: node1 })
// re.print(node1)

// a{,2}b:
// const node2 = re.createNNode('a')
// const node3 = re.createNNode('b')
// const node1 = re.createCNode(node2, node3)
// const node5 = re.createNNode('a', { next: node3 })
// const node4 = re.createCNode(node5, node3, { previous: node2 })
// re.print(node1)
