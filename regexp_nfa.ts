import { assertEquals } from 'https://deno.land/std@0.202.0/assert/mod.ts'
import {
  OPEN_PARENS,
  CLOSE_PARENS,
  isError,
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
  memoize,
  allButChar,
  charSet,
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
  isLiteral: boolean
}

type CharacterClassType = {
  type: 'characterClass'
  negated: boolean
  options: CharacterClassOptionsType
}
type ParenthesizedType = {
  type: 'parenthesized'
  expr: RegExpType
  isCaptureGroup: boolean
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

const VERTICAL_BAR = '|'
const alternation = char(VERTICAL_BAR)

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

const DOLLAR_SIGN = '$'
const CARET = '^'
const BACK_SLASH = '\\'
const OPEN_BRACKETS = '{'
const CLOSE_BRACKETS = '}'
const OPEN_SQUARE_BRACKETS = '['
const CLOSE_SQUARE_BRACKETS = ']'
const WORD_BOUNDARY_CHAR = 'b'

const FORBIDDEN_AS_SINGLE_CHARS = [
  ...Object.keys(QUANTIFIERS),
  VERTICAL_BAR,
  OPEN_BRACKETS,
  CLOSE_BRACKETS,
  OPEN_SQUARE_BRACKETS,
  CLOSE_SQUARE_BRACKETS,
  OPEN_PARENS,
  CLOSE_PARENS,
]

const ALWAYS_ESCAPED_LITERAL_CHAR_MAPPINGS: { [key: SingleChar]: SingleChar } = {
  n: '\n', // New line
  t: '\t', // Tab
  f: '\f', // line Feed
}

const NON_LITERAL_CHARS = {
  escaped: [
    WORD_BOUNDARY_CHAR, // Word boundary anchor
  ],
  nonEscaped: [
    PERIOD, // Catch all (but new line)
    CARET, // Start of line/string anchor
    DOLLAR_SIGN, // End of line/string anchor
  ],
}

const singleChar: Parser<SingleCharType> = memoize(
  map(
    or(
      and(
        char(BACK_SLASH),
        charSet(
          [
            ...Object.keys(ALWAYS_ESCAPED_LITERAL_CHAR_MAPPINGS),
            ...NON_LITERAL_CHARS.escaped,
            ...NON_LITERAL_CHARS.nonEscaped,
            ...FORBIDDEN_AS_SINGLE_CHARS,
          ].join(EMPTY_STRING)
        )
      ),
      allButCharSet(FORBIDDEN_AS_SINGLE_CHARS.join(EMPTY_STRING))
    ),
    charOrEscapedChar =>
      ({
        type: 'singleChar',
        character: Array.isArray(charOrEscapedChar)
          ? charOrEscapedChar[1] in ALWAYS_ESCAPED_LITERAL_CHAR_MAPPINGS
            ? ALWAYS_ESCAPED_LITERAL_CHAR_MAPPINGS[charOrEscapedChar[1]]
            : charOrEscapedChar[1]
          : charOrEscapedChar,
        isLiteral:
          // Neither '.' nor '\b'
          !(
            (typeof charOrEscapedChar === 'string' &&
              NON_LITERAL_CHARS.nonEscaped.indexOf(charOrEscapedChar) >= 0) ||
            (Array.isArray(charOrEscapedChar) &&
              NON_LITERAL_CHARS.escaped.indexOf(charOrEscapedChar[1]) >= 0)
          ),
      } as SingleCharType)
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

const characterClass: Parser<CharacterClassType> = memoize(
  map(
    delimitedBy(
      char('['),
      and(optional(char(CARET)), many1(characterClassOption)),
      char(CLOSE_SQUARE_BRACKETS)
    ),
    ([caret, options]) => ({
      type: 'characterClass',
      negated: caret === CARET,
      options,
    })
  )
)

const parenthesized: Parser<ParenthesizedType> = memoize(
  map(
    delimitedBy(openParens, and(optional(charSequence('?:')), regExp), closeParens),
    ([nonCaptureGroupPrefix, expr]) => ({
      type: 'parenthesized' as const,
      expr,
      isCaptureGroup: nonCaptureGroupPrefix === EMPTY_STRING,
    })
  )
)

const quantifier: Parser<RepetitionLimitsType> = memoize(
  map(
    or(
      orN(Object.keys(QUANTIFIERS).map(charSequence)),
      delimitedBy(
        char(OPEN_BRACKETS),
        or(joinedBy(optional(natural), comma), natural),
        char(CLOSE_BRACKETS)
      )
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

type FactorType = RepetitionType | SingleCharType | CharacterClassType | ParenthesizedType

const factor: Parser<RegExpTokenType> = memoize(input => {
  let result: FactorType | Error
  let rest: string
  let repetitionsReduced: boolean

  ;[result, rest] = or4(repetition, singleChar, characterClass, parenthesized)(input)

  if (isError(result)) return [result, input]

  while (true) {
    ;[repetitionsReduced, result] = reduceRepetitions(result)

    if (!repetitionsReduced) break
  }

  return [result, rest]
})

const reduceRepetitions = (regexpToken: FactorType): [boolean, FactorType] => {
  let reduced = false

  // Identify the following cases:
  //
  // 1) (N*)*
  // 2) (N+)*
  // 3) (N?)*
  // 4) (N*)+
  // 5) (N+)+
  // 6) (N?)+
  //
  // and replace them by, respectively:
  //
  // 1) N*
  // 2) N*
  // 3) N*
  // 4) N*
  // 5) N+ (notice this is the only case where the `+` is used, while all others use `*`)
  // 6) N*

  if (regexpToken.type === 'repetition') {
    const outerRepetition = regexpToken

    if (
      outerRepetition.limits.max === Infinity &&
      outerRepetition.expr.type === 'parenthesized' &&
      outerRepetition.expr.expr.length === 1
    ) {
      const innerToken = outerRepetition.expr.expr[0]

      if (innerToken.type === 'repetition') {
        const innerRepetition = innerToken

        if (innerRepetition.limits.min === 0 || innerRepetition.limits.max === Infinity) {
          const minimumLimits = [outerRepetition.limits.min, innerRepetition.limits.min]
          const maximumLimits = [outerRepetition.limits.max, innerRepetition.limits.max]

          regexpToken = { ...innerRepetition }

          regexpToken.limits = {
            min:
              minimumLimits.every(limit => limit >= 1) &&
              maximumLimits.every(limit => limit === Infinity)
                ? 1
                : 0,
            max: Infinity,
          }

          reduced = true
        }
      }
    }
  }

  return [reduced, regexpToken]
}

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

//////////////////////////////////////////////////////////////////////////////////////////
// The following data structures, algorithms etc are based on ideas extracted from this //
// article by Ken Thompson: https://dl.acm.org/doi/epdf/10.1145/363347.363387           //
//////////////////////////////////////////////////////////////////////////////////////////

type CNodeBranchingModeType = 'default' | 'lazy' | 'possessive'

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
  branchingMode: CNodeBranchingModeType
  possessiveWatermark?: number
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
  branchingMode: CNodeBranchingModeType = 'default'
): CNodeType => ({
  type: 'CNode',
  id: cNodeCount++,
  next,
  nextAlt,
  branchingMode,
})

// Create the singleton End Node.
const endNode: ENodeType = {
  type: 'ENode',
  id: 0,
}

// Create the singleton Failed Node. Applicable to negated character classes only.
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
      partialClone = createCNode(failedNode, failedNode, node.branchingMode)
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
    const newLineNode = createNNode(NEW_LINE, nextNode, false) // Only "\n"
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
        astNode.isPossessive ? 'possessive' : astNode.isLazy ? 'lazy' : 'default'
      )
      rightNodeNext = rightCNode
    })
  } // limits.max === Infinity
  else {
    // Notice the temporary use of `failedNode` below as the `next` prop, which will be replaced
    // right after creating the CNode.
    rightCNode = createCNode(
      failedNode,
      nextNode,
      astNode.isPossessive ? 'possessive' : astNode.isLazy ? 'lazy' : 'default'
    )
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
      return createNNode(astNode.character, nextNode, astNode.isLiteral)

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
  index: number
  skipBacktrackingInNextAlternativeBranch?: boolean
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

  debug(
    () =>
      `[input: '${input}', index: ${index}, previousChar: '${previousChar}', options: ${inspect(
        options
      )}] Trying to match against node ${nodeAsString(currentNode)}`
  )

  switch (currentNode.type) {
    case 'NNode': {
      const currentChar = isEmptyInput ? '' : input[index]

      debug(() => `[currentChar: '${currentChar}']`)

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
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index, previousChar, options)
              )
            break

          case PERIOD: // A '.' matches anything but the new line (\n).
            if (currentChar !== NEW_LINE && !isEmptyInput)
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index + 1, currentChar, options)
              )
            break

          case WORD_BOUNDARY_CHAR: // A '\b' matches the (empty) string immediately before or after a "word".
            if (
              // Either it is a left boundary...
              ((isStartOfInput || !isWordChar(previousChar)) && isWordChar(currentChar)) ||
              // ...or a right boundary.
              ((isEmptyInput || !isWordChar(currentChar)) && isWordChar(previousChar))
            )
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index, previousChar, options)
              )

            break

          default:
            throw new Error(`Invalid non-literal character '${currentNode.character}'`)
        }
      }

      break
    }

    case 'CNode': {
      const branch = currentNode.branchingMode !== 'lazy' ? 'next' : 'nextAlt'

      debug(
        () =>
          `[input: '${input}', index: ${index}] Trying CNode's #${currentNode.id}'s "${branch}" branch`
      )

      if (currentNode.branchingMode === 'possessive') currentNode.possessiveWatermark = index

      const match = matchNfa(currentNode[branch], input, index, previousChar, options)

      if (match.matched) {
        const charsConsumed = match.index - index

        debug(() => `>>>>> Match successful! Characters consumed: ${charsConsumed}`)

        return (
          debug(
            () =>
              `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s "${branch}" branch!`
          ),
          match
        )
      } else if (
        !match.skipBacktrackingInNextAlternativeBranch &&
        // Checks if we reached the end of a possessive repetition.
        (currentNode.branchingMode !== 'possessive' || index >= currentNode.possessiveWatermark!)
      ) {
        const branch = currentNode.branchingMode !== 'lazy' ? 'nextAlt' : 'next'

        debug(
          () =>
            `[input: '${input}', index: ${index}] Trying CNode's #${currentNode.id}'s alternative "${branch}" branch`
        )

        return (
          debug(
            () =>
              `[input: '${input}', index: ${index}] Passed CNode #${currentNode.id}'s "${branch}" branch!`
          ),
          matchNfa(currentNode[branch], input, index, previousChar, options)
        )
      }

      break
    }

    case 'ENode':
      return { matched: true, index }

    case 'FNode':
      return { matched: false, index, skipBacktrackingInNextAlternativeBranch: true }

    default: {
      const exhaustiveCheck: never = currentNode
      throw new Error(`[${exhaustiveCheck}] Invalid NFA node type`)
    }
  }

  debug(() => `[input: '${input}', index: ${index}] No match!`)

  return { matched: false, index }
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

Deno.test('Repetitions', () => {
  debugMode = false

  assertMatches('an+', 'banana', 'b->an<-ana')
  assertMatches('(an)+', 'banana', 'b->anan<-a')
  assertMatches('iss', 'mississipi', 'm->iss<-issipi')
  assertMatches('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches('is+', 'mississipi', 'm->iss<-issipi')
  assertMatches('(is+)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches('/d{2}/D/d{2}/s*([ap]m)', '12:50 am', '->12:50 am<-')
  assertMatches('a*', '', '-><-')
  assertMatches('a+', '', NO_MATCH_MESSAGE)
  assertMatches('a*', '...aa', '-><-...aa')
  assertMatches('a*', 'aa', '->aa<-')
  assertMatches('a+', '...aa', '...->aa<-')
  assertMatches('a+$', '..aa', '..->aa<-')
  assertMatches('(x+x+)+y', 'xxxxxxxxxxy', '->xxxxxxxxxxy<-')
  assertMatches('(x+x+)+y', 'xxxxxxxxxx', NO_MATCH_MESSAGE)
  assertMatches('(a+)*ab', 'aaaaaaaaaaaab', '->aaaaaaaaaaaab<-')
  assertMatches('.*.*=.*', 'x=x', '->x=x<-')
})

Deno.test('AST nodes of problematic repetitions', () => {
  debugMode = false

  // [
  //   {
  //     type: 'repetition',
  //     expr: { type: 'singleChar', character: 'a', isLiteral: true },
  //     limits: { min: 0, max: Infinity },
  //     isLazy: false,
  //     isPossessive: false,
  //   },
  // ]
  const zeroOrMoreAsAst = buildRegExpAst('a*')

  // [
  //   {
  //     type: 'repetition',
  //     expr: { type: 'singleChar', character: 'a', isLiteral: true },
  //     limits: { min: 1, max: Infinity },
  //     isLazy: false,
  //     isPossessive: false,
  //   },
  // ]
  const oneOrMoreAsAst = buildRegExpAst('a+')

  assertEquals(buildRegExpAst('(a*)*'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('(a+)*'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('(a?)*'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('(a*)+'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('(a+)+'), oneOrMoreAsAst)
  assertEquals(buildRegExpAst('(a?)+'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('((((a*)*)*)*)*'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('((((a+)+)+)+)+'), oneOrMoreAsAst)
  assertEquals(buildRegExpAst('((((a+)+)*)+)+'), zeroOrMoreAsAst)

  // And finally, (very) deep repetitions, mixing many quantifiers.
  assertEquals(buildRegExpAst('(((((((((a*)+)?)*){,5}){0,2})?)*)?)+'), zeroOrMoreAsAst)
  assertEquals(buildRegExpAst('(((((((((a+){5,})+)+){2,})+)+){3,})+)+'), oneOrMoreAsAst)
})

Deno.test('Problematic repetitions', () => {
  debugMode = false

  // Case (N+)+
  assertMatches('(a+)+', '', NO_MATCH_MESSAGE)
  assertMatches('(a+)+', 'aaaaa', '->aaaaa<-')
  assertMatches('(a+)+', 'baaaaa', 'b->aaaaa<-')

  // Case (N+)*
  assertMatches('(a+)*', '', '-><-')
  assertMatches('(a+)*', 'aaaaa', '->aaaaa<-')
  assertMatches('(a+)*', 'baaaaa', '-><-baaaaa')

  // Case (N*)*
  assertMatches('(a*)*', '', '-><-')
  assertMatches('(a*)*', 'aaaaa', '->aaaaa<-')
  assertMatches('(a*)*', 'baaaaa', '-><-baaaaa')

  // Case (N*)+
  assertMatches('(a*)+', '', '-><-')
  assertMatches('(a*)+', 'aaaaa', '->aaaaa<-')
  assertMatches('(a*)+', 'baaaaa', '-><-baaaaa')

  // Case (N?)*
  assertMatches('(a?)*', '', '-><-')
  assertMatches('(a?)*', 'aaaaa', '->aaaaa<-')
  assertMatches('(a?)*', 'baaaaa', '-><-baaaaa')

  // Case (N?)+
  assertMatches('(a?)+', '', '-><-')
  assertMatches('(a?)+', 'aaaaa', '->aaaaa<-')
  assertMatches('(a?)+', 'baaaaa', '-><-baaaaa')

  assertMatches('((a*)?)+', 'aaaaa', '->aaaaa<-')
  assertMatches('(((a+)*)?)+', 'aaaaa', '->aaaaa<-')
})

Deno.test('Complex repetitions', () => {
  debugMode = false

  // Produces complex, multi-level repetitions, such as: "(a+)", "((a+)+)", "(((a+)+)+)" etc.
  const repeatedAs = (n: number) => {
    let regexp = 'a'

    times(n, () => {
      regexp = `(${regexp}+)`
    })

    return regexp
  }

  assertMatches('a*'.repeat(100), 'a'.repeat(100), '->' + 'a'.repeat(100) + '<-')
  assertMatches(repeatedAs(250), 'a'.repeat(1000), '->' + 'a'.repeat(1000) + '<-')

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
  assertMatches('\\b', 'some_word', '-><-some_word')
  assertMatches('\\b/w{4}', '           some_word   ', '           ->some<-_word   ')
  assertMatches('/w{4}\\b', '           some_word   ', '           some_->word<-   ')
  assertMatches('\\b/w{4}', 'some_word   ', '->some<-_word   ')
  assertMatches('/w{4}\\b', '           some_word', '           some_->word<-')
  assertMatches('\\b/w\\b', '               x              ', '               ->x<-              ')
  assertMatches('\\b/w\\b', '               xx              ', NO_MATCH_MESSAGE)
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
  assertEquals(scan('\\b/w', 'regexps are really cool'), ['r', 'a', 'r', 'c'])
  assertEquals(scan('/w\\b', 'regexps are really cool'), ['s', 'e', 'y', 'l'])
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

Deno.test('Greedy (default) x lazy behavior', () => {
  debugMode = false

  assertMatches('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches('(iss)+?', 'mississipi', 'm->iss<-issipi')

  assertMatches('a*', 'aaaaa', '->aaaaa<-')
  assertMatches('a*?', 'aaaaa', '-><-aaaaa')

  assertMatches('a+', 'aaaaa', '->aaaaa<-')
  assertMatches('a+?', 'aaaaa', '->a<-aaaa')
})

Deno.test('Backtrackable (default) x possessive behavior', () => {
  debugMode = false

  assertMatches('a*a', 'aaaa', '->aaaa<-')
  assertMatches('a*+a', 'aaaa', NO_MATCH_MESSAGE)

  assertMatches('a+a', 'aaaa', '->aaaa<-')
  assertMatches('a++a', 'aaaa', NO_MATCH_MESSAGE)

  assertMatches('a+b', 'aaaab', '->aaaab<-')
  assertMatches('a++b', 'aaaab', '->aaaab<-')

  assertMatches('a+ab', 'aaaab', '->aaaab<-')
  assertMatches('a++ab', 'aaaab', NO_MATCH_MESSAGE)

  assertEquals(scan('/d++', '1234567890'), ['1234567890'])
  assertEquals(scan('/d*+', '1234567890'), ['1234567890'])
})

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
