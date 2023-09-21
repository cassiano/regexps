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
  orN,
  PERIOD,
  EMPTY_STRING,
  SingleChar,
  many,
  succeededBy,
  allButCharSet,
  or4,
  charSequence,
  precededBy,
  memoize,
  allButChar,
} from '../reactive-spreadsheet/src/parser_combinators.ts'

const NO_MATCH_MESSAGE = '(sorry, no match)'
const ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS = true

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

// const replaceEscapedChars = (regExpAsString: string): string =>
//   regExpAsString.replaceAll(/[/\\](.)/g, '[$1]')

export const buildRegExpAst = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(
    // replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))
    replaceCharacterClassAbbreviations(regExpAsString)
  )

  // debug(() => `RegExp: ${replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))}`)
  debug(() => `RegExp: ${replaceCharacterClassAbbreviations(regExpAsString)}`)

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  return result
}

declare const Deno: {
  inspect: (...args: unknown[]) => void
  stdin: { read: (...args: unknown[]) => void }
}

export const log = console.log

export const inspect = (value: any) =>
  Deno.inspect(value, { depth: 999, colors: true }) as unknown as string
export const print = (value: object) => log(inspect(value))

export const showRegExp = (regExpAsString: string) => print(buildRegExpAst(regExpAsString))

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)

const debug = (messageOrFalse: () => string | false): void => {
  if (debugMode) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

//////////////////////////////////////////////////////////////////////////////////////////
// The following data structures, algorithms etc are based on ideas extracted from this //
// article from Ken Thompson: https://dl.acm.org/doi/epdf/10.1145/363347.363387         //
//////////////////////////////////////////////////////////////////////////////////////////

// CNode ("Cross" Node). Splits the current search path, matching one of 2 possible choices/alternatives. It always has one
// input path and exactly two output paths. An output path equals to `undefined` means an end state (i.e. a successful match).
type CNodeType = { type: 'CNode'; id: number; next?: NodeType | null; nextAlt?: NodeType | null }

// NNode ("Normal" Node). Matches a single character. It always has one input path and exactly one output path. An output path
// equals to `undefined` means an end state (i.e. a successful match).
type NNodeType = {
  type: 'NNode'
  id: number
  character: SingleChar
  isLiteral: boolean
  next?: NodeType | null
}
type NodeType = NNodeType | CNodeType

type CreateNNodeOptionsType = {
  next?: NodeType | null
  isLiteral?: boolean
}

let nNodeCount: number
let cNodeCount: number

const createNNode = (
  character: SingleChar,
  { next, isLiteral = false }: CreateNNodeOptionsType = {}
) => ({ type: 'NNode', id: nNodeCount++, character, isLiteral, next } as NNodeType)

const createCNode = (next?: NodeType | null, nextAlt?: NodeType | null) =>
  ({ type: 'CNode', id: cNodeCount++, next, nextAlt } as CNodeType)

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

const nodeAsString = (node: NodeType | null | undefined) =>
  node === null || node === undefined ? node : `${node.type} #${node.id}`

// Clones a node, setting its `next` and `nextAlt` (in the case of a CNode) props which are `undefined` to `defaultNext`.
const cloneNode = (
  node: NodeType | null | undefined,
  defaultNext: NodeType | null | undefined,
  partialClonesHistory: Map<NodeType, NodeType> = new Map()
): NodeType | null | undefined => {
  if (node === null || node === undefined) return node

  // Node already cloned?
  if (partialClonesHistory.has(node)) return partialClonesHistory.get(node)

  let partialClone: NodeType

  // Clone the node, delaying the creation of its `next` and `nextAlt` (in the case of a CNode) props, so the new node can be added right away
  // into the clones map.
  switch (node.type) {
    case 'NNode':
      partialClone = createNNode(node.character, {
        isLiteral: node.isLiteral,
      })
      break

    case 'CNode':
      partialClone = createCNode(undefined, undefined)
      break

    default: {
      const _exhaustiveCheck: never = node
      throw new Error('Invalid NFA node type')
    }
  }

  partialClonesHistory.set(node, partialClone)

  // With the new clone properly saved in the `clones` map, set its `next` prop.
  partialClone.next =
    node.next === undefined ? defaultNext : cloneNode(node.next, defaultNext, partialClonesHistory)

  // Set its `nextAlt` prop (for CNodes).
  if (node.type === 'CNode' && partialClone.type === 'CNode')
    partialClone.nextAlt =
      node.nextAlt === undefined
        ? defaultNext
        : cloneNode(node.nextAlt, defaultNext, partialClonesHistory)

  return partialClone
}

const createNfaFromAst = (ast: RegExpType, nextNode?: NodeType | null): NodeType => {
  let next: NodeType | null | undefined = nextNode

  for (let i = ast.length - 1; i >= 0; i--) {
    next = createNfaNodeFromRegExpToken(ast[i], next)
  }

  return next!
}

const createNfaNodeFromRegExpToken = (
  astNode: RegExpTokenType,
  nextNode?: NodeType | null
): NodeType => {
  switch (astNode.type) {
    case 'singleChar':
      return createNNode(astNode.character, { next: nextNode, isLiteral: false })

    case 'alternation':
      return createCNode(
        createNfaFromAst(astNode.left, nextNode),
        createNfaFromAst(astNode.right, nextNode)
      )

    case 'parenthesized':
      return createNfaFromAst(astNode.expr, nextNode)

    case 'characterClass': {
      const options = mapCharacterClassOptions(astNode.options)
      let lastNode: NodeType = createNNode(options.at(-1)!, { next: nextNode, isLiteral: true })

      if (astNode.negated) {
        const periodNode = createNNode(PERIOD, { next: nextNode, isLiteral: false }) // All but "\n"
        const newLineNode = createNNode(NEW_LINE, { next: nextNode, isLiteral: true }) // Only "\n"
        const catchAllNode = createCNode(periodNode, newLineNode)

        lastNode.next = null // next = null means "no match!".
        lastNode = createCNode(lastNode, catchAllNode)
        nextNode = null
      }

      let accNode: NodeType = lastNode

      for (let i = options.length - 2; i >= 0; i--) {
        const node = createNNode(options[i], { next: nextNode, isLiteral: true })

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
      // - [†]: CNode only
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
      //              ▙▃▃▃▃▃ (m times) ▃▃▃▃▃▟      ↓      ↓    ▐
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

      let rightNodeNext: NodeType | null | undefined = nextNode
      let rightCNode: NodeType | null | undefined = nextNode

      if (limits.max !== Infinity) {
        times(limits.max - limits.min, () => {
          rightCNode = createCNode(cloneNode(repeatingNode, rightNodeNext), nextNode)
          rightNodeNext = rightCNode
        })
      } // limits.max === Infinity
      else {
        rightCNode = createCNode(undefined, nextNode)
        rightCNode.next = cloneNode(repeatingNode, rightCNode)
      }

      let leftClonedNodeNext: NodeType | null | undefined = rightCNode

      times(limits.min, () => {
        leftClonedNodeNext = cloneNode(repeatingNode, leftClonedNodeNext)
      })

      return leftClonedNodeNext!
    }

    default: {
      const exhaustiveCheck: never = astNode
      throw new Error(`[${exhaustiveCheck}] Invalid AST node type`)
    }
  }
}

export const buildNfaFromRegExp = (
  regExpAsString: string,
  { printNodes = true } = {}
): NodeType => {
  const previousNNodeCount = nNodeCount
  const previousCNodeCount = cNodeCount

  const ast = buildRegExpAst(regExpAsString)

  debug(() => printNodes && `\nAST: \n\n${inspect(ast)}`)

  const nfa = createNfaFromAst(ast)

  debug(() => printNodes && `\nNFA: \n\n${inspect(nfa)}`)

  debug(
    () =>
      `\nNFA nodes generated: \n\nnNodeCount: ${nNodeCount - previousNNodeCount}, cNodeCount: ${
        cNodeCount - previousCNodeCount
      }`
  )

  return nfa
}

const WORD_BOUNDARY = '\b'

const isWordChar = (char: SingleChar) => {
  const upcasedChar = char.toUpperCase()

  return (
    (upcasedChar >= 'A' && upcasedChar <= 'Z') ||
    (upcasedChar >= '0' && upcasedChar <= '9') ||
    upcasedChar === '_'
  )
}

let matchNfaCount: number

type MatchNfaReturnType = {
  matched: boolean
  input: string
  index: number
  skipFollowingCNodeNextAltCall?: boolean
}

const matchNfa = (
  currentNode: NodeType | null | undefined,
  input: string,
  index: number,
  previousChar: SingleChar
): MatchNfaReturnType => {
  matchNfaCount++

  if (currentNode === undefined) return { matched: true, input, index }

  if (currentNode === null)
    return { matched: false, input, index, skipFollowingCNodeNextAltCall: true }

  const isEmptyInput = input.length === index
  const isStartOfInput = index === 0

  switch (currentNode.type) {
    case 'NNode': {
      const currentChar = isEmptyInput ? '' : input[index]

      debug(
        () =>
          `[input: '${input}', index: ${index}, previousChar: '${previousChar}', currentChar: '${currentChar}'] Trying to match character '${
            currentChar ?? ''
          }' against node ${nodeAsString(currentNode)}`
      )

      if (currentNode.isLiteral) {
        if (currentChar === currentNode.character)
          // Matches character literally.
          return debug(() => 'Matched!'), matchNfa(currentNode.next, input, index + 1, currentChar)
      } else {
        switch (currentNode.character) {
          case CARET: // A '^' matches the start of the input string (Ruby behavior) or the start of each individual line (JS behavior).
            if (
              ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS
                ? isStartOfInput || previousChar === '\n'
                : isStartOfInput
            )
              return debug(() => 'Matched!'), matchNfa(currentNode.next, input, index, previousChar)

            break

          case DOLLAR_SIGN: // A '$' matches the end of the input string (Ruby behavior) or the end of each individual line (JS behavior).
            if (
              ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS
                ? isEmptyInput || currentChar === '\n'
                : isEmptyInput
            )
              return debug(() => 'Matched!'), { matched: true, input, index }
            break

          case PERIOD: // A '.' matches anything but the new line (\n).
            if (currentChar !== NEW_LINE && !isEmptyInput)
              return (
                debug(() => 'Matched!'), matchNfa(currentNode.next, input, index + 1, currentChar)
              )
            break

          case WORD_BOUNDARY: // A '\b' matches the (empty) string immediately before or after a "word".
            if (
              (isStartOfInput && isWordChar(currentChar)) ||
              (!isStartOfInput && !isWordChar(previousChar) && isWordChar(currentChar)) ||
              (isEmptyInput && isWordChar(previousChar)) ||
              (!isWordChar(currentChar) && isWordChar(previousChar))
            )
              return debug(() => 'Matched!'), matchNfa(currentNode.next, input, index, previousChar)

            break

          default: // Matches character literally.
            if (currentChar === currentNode.character)
              return (
                debug(() => 'Matched!'), matchNfa(currentNode.next, input, index + 1, currentChar)
              )
        }
      }

      break
    }

    case 'CNode':
      debug(
        () =>
          `[input: '${input}', index: ${index}, previousChar: '${previousChar}'] Trying to match against node ${nodeAsString(
            currentNode
          )}`
      )

      let match = matchNfa(currentNode.next, input, index, previousChar)

      if (match.matched)
        return (
          debug(
            () =>
              `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s next path!`
          ),
          match
        )
      else if (!match.skipFollowingCNodeNextAltCall) {
        match = matchNfa(currentNode.nextAlt, input, index, previousChar)

        if (match.matched)
          return (
            debug(
              () =>
                `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s nextAlt path!`
            ),
            match
          )
      }

      break

    default: {
      const _exhaustiveCheck: never = currentNode
      throw new Error('Invalid NFA node type')
    }
  }

  return { matched: false, input, index }
}

type BuildNfaFromRegExpAndMatchOptionsType = {
  exactMatch?: boolean
  printNodes?: boolean
  arrows?: boolean
  startingIndex?: number
}

export const buildNfaFromRegExpAndMatch = (
  regExpAsString: string,
  input: string,
  options: BuildNfaFromRegExpAndMatchOptionsType = {}
): { match: string; start: number; end: number } | typeof NO_MATCH_MESSAGE => {
  const nfa = buildNfaFromRegExp(regExpAsString, {
    printNodes: options.printNodes,
  })

  return matchFromNfa(nfa, input, options)
}

type MatchFromNfaReturnType =
  | {
      match: string
      start: number
      end: number
    }
  | typeof NO_MATCH_MESSAGE

const matchFromNfa = (
  nfa: NodeType,
  input: string,
  {
    exactMatch = false,
    arrows = false,
    startingIndex = 0,
  }: BuildNfaFromRegExpAndMatchOptionsType = {}
): MatchFromNfaReturnType => {
  matchNfaCount = 0

  // Try to match the regular expression from left to right.
  for (
    let index = startingIndex;
    index < (exactMatch || input.length === 0 ? 1 : input.length);
    index++
  ) {
    const match = matchNfa(nfa, input, index, index > 0 ? input[index - 1] : '')

    debug(() => `match: ${inspect(match)}, accumulated matchNfaCount: ${matchNfaCount}`)

    if (match.matched) {
      const matchedString = input.slice(index, match.index)

      return {
        match: arrows
          ? [
              input.slice(startingIndex, index),
              '->',
              matchedString,
              '<-',
              input.slice(match.index),
            ].join(EMPTY_STRING)
          : matchedString,
        start: index,
        end: match.index - 1,
      }
    }
  }

  return NO_MATCH_MESSAGE
}

export const scan = (regExpAsString: string, input: string): string[] => {
  let startingIndex = 0

  const matches = []

  const nfa = buildNfaFromRegExp(regExpAsString)

  while (true) {
    const match = matchFromNfa(nfa, input, { startingIndex })

    if (typeof match === 'string') break // Match unsuccessful! Stop scan.

    matches.push(match.match)

    startingIndex = match.end + 1
  }

  return matches.flat()
}

///////////
// Tests //
///////////

const assertMatches = (
  regExpAsString: string,
  input: string,
  matchedResult: string,
  exactMatch = false
) => {
  const match = buildNfaFromRegExpAndMatch(regExpAsString, input, { exactMatch, arrows: true })

  assertEquals(typeof match === 'string' ? match : match.match, matchedResult)
}

const previousDebugMode = debugMode
debugMode = false

log('Running tests...')

assertMatches('an+', 'banana', 'b->an<-ana')
assertMatches('(an)+', 'banana', 'b->anan<-a')
assertMatches('iss', 'mississipi', 'm->iss<-issipi')
assertMatches('(iss)+', 'mississipi', 'm->ississ<-ipi')
assertMatches('is+', 'mississipi', 'm->iss<-issipi')
assertMatches('(is+)+', 'mississipi', 'm->ississ<-ipi')
assertMatches('/d{2}/D/d{2}/s*([ap]m)', '12:50 am', '->12:50 am<-')
assertMatches('a*', '...aa', '-><-...aa')
assertMatches('a*', 'aa', '->aa<-')
assertMatches('a+', '...aa', '...->aa<-')
assertMatches('a+$', '..aa', '..->aa<-')
assertMatches('(x+x+)+y', 'xxxxxxxxxxy', '->xxxxxxxxxxy<-')
assertMatches('(x+x+)+y', 'xxxxxxxxxx', NO_MATCH_MESSAGE)
assertMatches('(a+)*ab', 'aaaaaaaaaaaab', '->aaaaaaaaaaaab<-')
assertMatches('.*.*=.*', 'x=x', '->x=x<-')
assertMatches('a*'.repeat(100), 'a'.repeat(1000), '->' + 'a'.repeat(1000) + '<-')

// Testing more than 2 repetition levels.
assertMatches(
  '(((a*b)+c)?d,){2,3}',
  'd,bcd,aaabababaaabbbbbbcd,d',
  '->d,bcd,aaabababaaabbbbbbcd,<-d'
)
assertMatches('(((a*b)+c)?d,){2,3}', 'd', NO_MATCH_MESSAGE)

// Testing anchors.
assertMatches('^a+', '...aa', NO_MATCH_MESSAGE)
assertMatches('^a+', 'aa', '->aa<-')
assertMatches('^a+$', 'aa...', NO_MATCH_MESSAGE)
assertMatches('^a+$', 'aa', '->aa<-')
assertMatches('\b', 'some_word', '-><-some_word')
assertMatches('\b/w{4}', '           some_word   ', '           ->some<-_word   ')
assertMatches('/w{4}\b', '           some_word   ', '           some_->word<-   ')
assertMatches('\b/w{4}', 'some_word   ', '->some<-_word   ')
assertMatches('/w{4}\b', '           some_word', '           some_->word<-')
assertMatches('\b/w\b', '               x              ', '               ->x<-              ')
assertMatches('\b/w\b', '               xx              ', NO_MATCH_MESSAGE)

// Testing `scan()`.
assertEquals(
  scan(
    '/w+([.]/w+)*@/w+([.]/w+)+',
    '| john.doe@gmail.com | john@gmail.com.us | john.doe@ | @gmail.com | john@gmail | jo.hn.do.e@g.mail.co.m |'
  ),
  ['john.doe@gmail.com', 'john@gmail.com.us', 'jo.hn.do.e@g.mail.co.m']
)
assertEquals(scan('/d{2}', '01234567'), ['01', '23', '45', '67'])
assertEquals(scan('/d', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
assertEquals(scan('.', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
assertEquals(scan('\b/w', 'regexps are really cool'), ['r', 'a', 'r', 'c'])
assertEquals(scan('/w\b', 'regexps are really cool'), ['s', 'e', 'y', 'l'])
assertEquals(
  scan('^.', 'regexps\nare\nreally\ncool'),
  ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS ? ['r', 'a', 'r', 'c'] : ['r']
)
assertEquals(
  scan('.$', 'regexps\nare\nreally\ncool'),
  ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS ? ['s', 'e', 'y', 'l'] : ['l']
)
assertEquals(scan('/w', 'ab+cd-efg*hijk/lmn'), [
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
])
assertEquals(scan('/W', 'ab+cd-efg*hijk/lmn'), ['+', '-', '*', '/'])

log('Done!')

// Reset the node counters.
nNodeCount = 0
cNodeCount = 0

debugMode = previousDebugMode
