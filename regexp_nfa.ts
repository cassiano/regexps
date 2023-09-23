import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import {
  plus,
  PLUS_SIGN,
  and3,
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
} from './parser_combinators.ts'

const NO_MATCH_MESSAGE = '(sorry, no match)'

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
  isLazy: boolean
  isPossessive: boolean
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

const QUESTION_MARK = '?'
const questionMark = char(QUESTION_MARK)

const repetition: Parser<RepetitionType> = memoize(
  map(
    and3(
      or3(singleChar, characterClass, parenthesized),
      quantifier,
      optional(or(questionMark, plus))
    ),
    ([expr, limits, lazyOrPossessive]) => ({
      type: 'repetition',
      expr,
      limits,
      isLazy: lazyOrPossessive === QUESTION_MARK,
      isPossessive: lazyOrPossessive === PLUS_SIGN,
    })
  )
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

export const buildRegExpAst = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(replaceCharacterClassAbbreviations(regExpAsString))

  debug(() => `RegExp: ${replaceCharacterClassAbbreviations(regExpAsString)}`)

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  return result
}

declare const Deno: {
  inspect: (...args: unknown[]) => void
  test: (title: string, testFn: () => void) => void
  stdin: { read: (...args: unknown[]) => void }
}

export const log = console.log

// deno-lint-ignore no-explicit-any
export const inspect = (value: any) =>
  Deno.inspect(value, { depth: 999, colors: true }) as unknown as string
// deno-lint-ignore ban-types
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

// CNode ("Cross" non-terminal Node). Splits the current search path, matching one of 2 possible
// choices/alternatives. It always has one input path and exactly two output paths. An output path
// equals to `endNode` means an end state (i.e. a successful match). If it equals to
// `failedNode`, it means a failed state (i.e. an unsuccessful match, which does not allow
// backtracking).
type CNodeType = {
  type: 'CNode'
  id: number
  next: NodeType
  nextAlt: NodeType
  isPossessive: boolean
  isLazy: boolean
}

// NNode ("Normal" terminal Node). Matches a single character. It always has one input path and
// exactly one output path. An output path equals to `endNode` means an end state (i.e. a
// successful match). If it equals to `failedNode`, it means a failed state (i.e. an
// unsuccessful match, which does not allow backtracking).
type NNodeType = {
  type: 'NNode'
  id: number
  character: SingleChar
  isLiteral: boolean
  next: NodeType
}

// ENode ("End" terminal node)
type ENodeType = {
  type: 'ENode'
  id: number
}

// FNode ("Failed match" terminal node)
type FNodeType = {
  type: 'FNode'
  id: number
}

type NodeType = NNodeType | CNodeType | ENodeType | FNodeType

let nNodeCount = 0
let cNodeCount = 0

const createNNode = (character: SingleChar, next: NodeType, isLiteral: boolean): NNodeType => ({
  type: 'NNode',
  id: nNodeCount++,
  character,
  isLiteral,
  next,
})

const createCNode = (
  next: NodeType,
  nextAlt: NodeType,
  isPossessive = false,
  isLazy = false
): CNodeType => ({
  type: 'CNode',
  id: cNodeCount++,
  next,
  nextAlt,
  isPossessive,
  isLazy,
})

// Create the singleton End Node.
const endNode: ENodeType = {
  type: 'ENode',
  id: 0,
}

// Create the singleton Failed Node.
const failedNode: FNodeType = {
  type: 'FNode',
  id: 0,
}

// Maps a character class range into an array of its individual constituint characters. E.g.: takes
// the 1st range in '[a-dxyz]' ('a-d'), and transforms it into [ "a", "b", "c", "d" ].
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

const nodeAsString = (node: NodeType) => `${node.type} #${node.id}`

// Clones a node, setting its `next` and `nextAlt` (in the case of a CNode) props which are
// `endNode` to `defaultNext`.
const cloneNode = (
  node: NodeType,
  defaultNext: NodeType,
  partialClonesHistory: Map<NodeType, NodeType> = new Map()
): NodeType => {
  // Node already cloned?
  if (partialClonesHistory.has(node)) return partialClonesHistory.get(node)!

  let partialClone: NodeType

  // Clone the node for NNode and CNode types, delaying the creation of its `next` and `nextAlt` (in
  // the case of a CNode) props, so the new node can be added right away into the clones map. We
  // temporarily use `failedNode` in place of the actual (yet to be cloned) nodes.
  switch (node.type) {
    case 'NNode':
      partialClone = createNNode(node.character, failedNode, node.isLiteral)
      break

    case 'CNode':
      partialClone = createCNode(failedNode, failedNode, node.isPossessive, node.isLazy)
      break

    case 'ENode':
    case 'FNode':
      return node

    default: {
      const exhaustiveCheck: never = node
      throw new Error(`[${exhaustiveCheck}] Invalid NFA node type`)
    }
  }

  partialClonesHistory.set(node, partialClone)

  // With the new clone properly saved in the `clones` map, set its `next` prop.
  partialClone.next =
    node.next === endNode ? defaultNext : cloneNode(node.next, defaultNext, partialClonesHistory)

  // Set its `nextAlt` prop (for CNodes).
  if (node.type === 'CNode' && partialClone.type === 'CNode')
    partialClone.nextAlt =
      node.nextAlt === endNode
        ? defaultNext
        : cloneNode(node.nextAlt, defaultNext, partialClonesHistory)

  return partialClone
}

const createNfaFromAst = (ast: RegExpType, nextNode: NodeType): NodeType => {
  let next: NodeType = nextNode

  for (let i = ast.length - 1; i >= 0; i--) {
    next = createNfaNodeFromRegExpToken(ast[i], next)
  }

  return next
}

const createNfaNodeFromCharacterClassRegExpToken = (
  astNode: CharacterClassType,
  nextNode: NodeType
): NodeType => {
  const options = mapCharacterClassOptions(astNode.options)
  let lastNode: NodeType = createNNode(options.at(-1)!, nextNode, true)

  if (astNode.negated) {
    const periodNode = createNNode(PERIOD, nextNode, false) // All but "\n"
    const newLineNode = createNNode(NEW_LINE, nextNode, true) // Only "\n"
    const catchAllNode = createCNode(periodNode, newLineNode)

    lastNode.next = failedNode // FNode means "no match!".
    lastNode = createCNode(lastNode, catchAllNode)
    nextNode = failedNode
  }

  let accNode: NodeType = lastNode

  for (let i = options.length - 2; i >= 0; i--) {
    const node = createNNode(options[i], nextNode, true)

    accNode = createCNode(node, accNode)
  }

  return accNode
}

const createNfaNodeFromRepetitionRegExpToken = (
  astNode: RepetitionType,
  nextNode: NodeType
): NodeType => {
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
  const repeatingNode = createNfaNodeFromRegExpToken(astNode.expr, endNode)

  let rightNodeNext: NodeType = nextNode
  let rightCNode: NodeType = nextNode

  if (limits.max !== Infinity) {
    times(limits.max - limits.min, () => {
      rightCNode = createCNode(
        cloneNode(repeatingNode, rightNodeNext),
        nextNode,
        astNode.isPossessive,
        astNode.isLazy
      )
      rightNodeNext = rightCNode
    })
  } // limits.max === Infinity
  else {
    // Notice the temporary use of `failedNode` below as the `next` prop, since it will be replaced
    // right after creating the CNode.
    rightCNode = createCNode(failedNode, nextNode, astNode.isPossessive, astNode.isLazy)
    rightCNode.next = cloneNode(repeatingNode, rightCNode)
  }

  let leftClonedNodeNext: NodeType = rightCNode

  times(limits.min, () => {
    leftClonedNodeNext = cloneNode(repeatingNode, leftClonedNodeNext)
  })

  return leftClonedNodeNext!
}

const createNfaNodeFromRegExpToken = (astNode: RegExpTokenType, nextNode: NodeType): NodeType => {
  switch (astNode.type) {
    case 'singleChar':
      return createNNode(astNode.character, nextNode, false)

    case 'alternation':
      return createCNode(
        createNfaFromAst(astNode.left, nextNode),
        createNfaFromAst(astNode.right, nextNode)
      )

    case 'parenthesized':
      return createNfaFromAst(astNode.expr, nextNode)

    case 'characterClass':
      return createNfaNodeFromCharacterClassRegExpToken(astNode, nextNode)

    case 'repetition':
      return createNfaNodeFromRepetitionRegExpToken(astNode, nextNode)

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

  const nfa = createNfaFromAst(ast, endNode)

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
  stopBacktracking?: boolean
}

const matchNfa = (
  currentNode: NodeType,
  input: string,
  index: number,
  previousChar: SingleChar,
  options: RegExpOptionsType
): MatchNfaReturnType => {
  matchNfaCount++

  const isEmptyInput = input.length === index
  const isStartOfInput = index === 0
  const currentChar = isEmptyInput ? '' : input[index]

  debug(
    () =>
      `[input: '${input}', index: ${index}, currentChar: '${currentChar}', previousChar: '${previousChar}', options: ${inspect(
        options
      )}] Trying to match against node ${nodeAsString(currentNode)}`
  )

  switch (currentNode.type) {
    case 'NNode': {
      if (currentNode.isLiteral) {
        if (currentChar === currentNode.character)
          // Matches character literally.
          return (
            debug(() => 'Matched!'),
            matchNfa(currentNode.next, input, index + 1, currentChar, options)
          )
      } else {
        switch (currentNode.character) {
          case CARET: // A '^' matches the start of the input string or of each individual line.
            if (options.jsMultiline ? isStartOfInput || previousChar === '\n' : isStartOfInput)
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index, previousChar, options)
              )

            break

          case DOLLAR_SIGN: // A '$' matches the end of the input string or of each individual line.
            if (options.jsMultiline ? isEmptyInput || currentChar === '\n' : isEmptyInput)
              return debug(() => 'Matched!'), { matched: true, input, index }
            break

          case PERIOD: // A '.' matches anything but the new line (\n).
            if (currentChar !== NEW_LINE && !isEmptyInput)
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index + 1, currentChar, options)
              )
            break

          case WORD_BOUNDARY: // A '\b' matches the (empty) string immediately before or after a "word".
            if (
              (isStartOfInput && isWordChar(currentChar)) ||
              (!isStartOfInput && !isWordChar(previousChar) && isWordChar(currentChar)) ||
              (isEmptyInput && isWordChar(previousChar)) ||
              (!isWordChar(currentChar) && isWordChar(previousChar))
            )
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index, previousChar, options)
              )

            break

          default: // Matches character literally.
            if (currentChar === currentNode.character)
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index + 1, currentChar, options)
              )
        }
      }

      break
    }

    case 'CNode': {
      const methodToCall = !currentNode.isLazy ? 'next' : 'nextAlt'

      let match = matchNfa(currentNode[methodToCall], input, index, previousChar, options)

      if (match.matched)
        return (
          debug(
            () =>
              `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s ${methodToCall} path!`
          ),
          match
        )
      else if (
        !(currentNode.isPossessive || match.stopBacktracking) ||
        currentChar === EMPTY_STRING
      ) {
        const methodToCall = !currentNode.isLazy ? 'nextAlt' : 'next'

        match = matchNfa(currentNode[methodToCall], input, index, previousChar, options)

        if (match.matched)
          return (
            debug(
              () =>
                `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s ${methodToCall} path!`
            ),
            match
          )
      }

      break
    }

    case 'ENode':
      return { matched: true, input, index }

    case 'FNode':
      return { matched: false, input, index, stopBacktracking: true }

    default: {
      const exhaustiveCheck: never = currentNode
      throw new Error(`[${exhaustiveCheck}] Invalid NFA node type`)
    }
  }

  return { matched: false, input, index }
}

type RegExpOptionsType = {
  jsMultiline?: boolean
}

type BuildNfaFromRegExpAndMatchOptionsType = {
  exactMatch?: boolean
  printNodes?: boolean
  arrows?: boolean
  startingIndex?: number
} & RegExpOptionsType

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
    jsMultiline = true,
  }: BuildNfaFromRegExpAndMatchOptionsType = {}
): MatchFromNfaReturnType => {
  matchNfaCount = 0

  // Try to match the regular expression from left to right.
  for (
    let index = startingIndex;
    index < (exactMatch || input.length === 0 ? 1 : input.length);
    index++
  ) {
    const match = matchNfa(nfa, input, index, index > 0 ? input[index - 1] : '', {
      jsMultiline,
    })

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

export const scan = (
  regExpAsString: string,
  input: string,
  options: RegExpOptionsType = {}
): string[] => {
  let startingIndex = 0

  const matches = []

  const nfa = buildNfaFromRegExp(regExpAsString)

  while (true) {
    const match = matchFromNfa(nfa, input, { startingIndex, ...options })

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
  options: BuildNfaFromRegExpAndMatchOptionsType = {}
) => {
  const match = buildNfaFromRegExpAndMatch(regExpAsString, input, { ...options, arrows: true })

  assertEquals(typeof match === 'string' ? match : match.match, matchedResult)
}

Deno.test('Basic behavior', () => {
  debugMode = false

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
})

Deno.test('Complex repetitions', () => {
  debugMode = false

  // Produces complex, multi-level, repetitions, such as: "(a+)", "((a+)+)", "(((a+)+)+)" etc.
  const repeatedAs = (n: number) => {
    let regexp = 'a'

    times(n, () => {
      regexp = `(${regexp}+)`
    })

    return regexp
  }

  assertMatches('a*'.repeat(100), 'a'.repeat(100), '->' + 'a'.repeat(100) + '<-')
  assertMatches(repeatedAs(20), 'a'.repeat(1000), '->' + 'a'.repeat(1000) + '<-')

  assertMatches(
    '(((a*b)+c)?d,){2,3}',
    'd,bcd,aaabababaaabbbbbbcd,d',
    '->d,bcd,aaabababaaabbbbbbcd,<-d'
  )
  assertMatches('(((a*b)+c)?d,){2,3}', 'd', NO_MATCH_MESSAGE)
})

Deno.test('Anchors', () => {
  debugMode = false

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
})

Deno.test('scan()', () => {
  debugMode = false

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
})

Deno.test('jsMultiline on x off behavior', () => {
  debugMode = false

  assertEquals(scan('^.', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    'r',
    'a',
    'r',
    'c',
  ])
  assertEquals(scan('^.', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['r'])
  assertEquals(scan('.$', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    's',
    'e',
    'y',
    'l',
  ])
  assertEquals(scan('.$', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['l'])
})

Deno.test('greedy (default) x lazy behavior', () => {
  debugMode = false

  assertMatches('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches('(iss)+?', 'mississipi', 'm->iss<-issipi')

  assertMatches('a*', 'aaaaa', '->aaaaa<-')
  assertMatches('a*?', 'aaaaa', '-><-aaaaa')

  assertMatches('a+', 'aaaaa', '->aaaaa<-')
  assertMatches('a+?', 'aaaaa', '->a<-aaaa')
})

Deno.test('backtrackable (default) x possessive behavior', () => {
  debugMode = false

  assertMatches('a*a', 'aaaa', '->aaaa<-')
  assertMatches('a*+a', 'aaaa', NO_MATCH_MESSAGE)

  assertMatches('a+a', 'aaaa', '->aaaa<-')
  assertMatches('a++a', 'aaaa', NO_MATCH_MESSAGE)

  assertEquals(scan('/d++', '1234567890'), ['1234567890'])
  assertEquals(scan('/d*+', '1234567890'), ['1234567890'])
})
