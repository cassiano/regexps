import { assertEquals } from 'https://deno.land/std@0.205.0/assert/mod.ts'
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
const UNESCAPED_WORD_BOUNDARY = 'b'

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
    UNESCAPED_WORD_BOUNDARY, // Word boundary anchor
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

const factor: Parser<RegExpTokenType> = memoize(
  or4(repetition, singleChar, characterClass, parenthesized)
)

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
  waterline?: number
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
  id: cNodeCount + nNodeCount++,
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
  id: nNodeCount + cNodeCount++,
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
  // Notice below that:
  //
  // ▬▶[a]: initial state
  // [b]▬▶: successful end state
  // [F]▬▶: failed state
  // [†]: CNode only
  // [a], [b]: NNodes or CNodes
  // [0], [1], [2], [x], [y], [.], [\n]: NNodes only
  // .|\n: matches anything
  //
  // Possible cases:
  //
  // a[0-9xy]b ≅ a[0123456789xy]b: ▬▶[a] ⭢ [†] ⭢ [0] ──┬─⭢ [b]▬▶
  //                                         ↓           |
  //                                        [†] ⭢ [1] ──┤
  //                                         ↓           |
  //                                        [†] ⭢ [2] ──┤
  //                                         …           …
  //                                         ↓           |
  //                                        [†] ⭢ [8] ──┤
  //                                         ↓           |
  //                                        [†] ⭢ [9] ──┤
  //                                         ↓           |
  //                                        [†] ⭢ [x] ──┤
  //                                         |           |
  //                                         └──⭢ [y] ──┛
  //
  // a[^0-9xy]b ≅ a[^0123456789xy]b: ▬▶[a] ⭢ [†] ⭢ [0] ──┬─⭢ [F]▬▶
  //                                           ↓           |
  //                                          [†] ⭢ [1] ──┤
  //                                           ↓           |
  //                                          [†] ⭢ [2] ──┤
  //                                           …           …
  //                                           ↓           |
  //                                          [†] ⭢ [8] ──┤
  //                                           ↓           |
  //                                          [†] ⭢ [9] ──┤
  //                                           ↓           |
  //                                          [†] ⭢ [x] ──┤
  //                                           ↓           |
  //                                          [†] ⭢ [y] ──┛
  //                                           ↓
  //                                          [†] ⭢ [.] ──┬─⭢ [b]▬▶
  //                                           |           |
  //                                           └──⭢ [\n] ─┛

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
  // [b]▬▶ or ◀▬[b]: successful end state
  // [a], [b]: NNodes or CNodes
  // [†]: CNode only
  // m, n: natural numbers
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

const isWordChar = memoize((char: SingleChar) => {
  const upcasedChar = char.toUpperCase()

  return (
    (upcasedChar >= 'A' && upcasedChar <= 'Z') ||
    (upcasedChar >= '0' && upcasedChar <= '9') ||
    upcasedChar === '_'
  )
})

const nodeCycleAllowsMatchingEmptyString = memoize(
  (originNode: NodeType, destinationNode: CNodeType): boolean =>
    // Origin node has finally reached the destination node in a cycle/loop?
    originNode === destinationNode ||
    // Is a CNode with an alternative branch which reaches the destination node?
    (originNode.type === 'CNode' &&
      nodeCycleAllowsMatchingEmptyString(originNode.nextAlt, destinationNode)) ||
    // Is an NNode which contains a non-literal (and no char-consuming) anchor with a default branch
    // which reaches the destination node?
    (originNode.type === 'NNode' &&
      !originNode.isLiteral &&
      [CARET, DOLLAR_SIGN, UNESCAPED_WORD_BOUNDARY].includes(originNode.character) &&
      nodeCycleAllowsMatchingEmptyString(originNode.next, destinationNode))
)

const cNodeAllowsMatchingEmptyString = memoize((node: CNodeType): boolean =>
  nodeCycleAllowsMatchingEmptyString(node.next, node)
)

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
          case PERIOD: // A '.' matches anything but the new line (\n).
            if (currentChar !== NEW_LINE && !isEmptyInput)
              return (
                debug(() => 'Matched!'),
                matchNfa(currentNode.next, input, index + 1, currentChar, options)
              )
            break

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

          case UNESCAPED_WORD_BOUNDARY: // A '\b' matches the (empty) string immediately before or after a "word".
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

      // Detect a possible infinite loop, when the current node's next node:
      //
      // - Is a CNode, which directly or indirectly points to the current node, by following its
      //   `nextAlt` branch; or
      // - Is an NNode containing an anchor (^, $ or /b), which directly or indirectly points to the
      //   current node, by following its `next` branch; and (in both cases)
      // - No characters have been consumed since the last call to the current node.
      //
      // The above situation happens in regular expressions where the expression inside a pair of
      // parentheses with infinite maximum limit (by using either `+` or `*`) could potentially match
      // an empty string (''), e.g.:
      //
      // - (a*)*
      // - (a?)*
      // - (a*b{0,999}c?)*
      // - (a*)+
      // - (a*b?)+
      // - (a*b?){2,}
      // - (a*b?^$\b){2,}
      // - etc
      //
      // It appears in Ken's article in the "Notes" section and is explained this way: "Code compiled
      // for a** will go into a loop due to the closure operator on an operand containing the null
      // regular expression, lambda."
      if (
        cNodeAllowsMatchingEmptyString(currentNode) &&
        currentNode.waterline === index // No chars consumed in previous match attempt.
      )
        return (
          debug(() => '+++++ Infinite loop avoided +++++'),
          matchNfa(currentNode.nextAlt, input, index, previousChar, options)
        )

      currentNode.waterline = index

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
        // Negated character class not matched?
        !match.skipBacktrackingInNextAlternativeBranch &&
        // Checks if we reached the end of a possessive repetition.
        (currentNode.branchingMode !== 'possessive' || index >= currentNode.waterline)
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
  startIndex?: number
} & RegExpOptionsType

export const buildNfaFromRegExpAndMatch = (
  regExpAsString: string,
  input: string,
  options: BuildNfaFromRegExpAndMatchOptionsType = {}
): { match: string; start: number; end: number } | typeof NO_MATCH_MESSAGE => {
  const nfa = buildNfaFromRegExp(regExpAsString, {
    printNodes: options.printNodes,
  })

  return match_v1(nfa, input, options)
}

type MatchFromNfaReturnType =
  | {
      match: string
      start: number
      end: number
    }
  | typeof NO_MATCH_MESSAGE

export const match_v1 = (
  nfa: NodeType,
  input: string,
  {
    exactMatch = false,
    arrows = false,
    startIndex = 0,
    jsMultiline = true,
  }: BuildNfaFromRegExpAndMatchOptionsType = {}
): MatchFromNfaReturnType => {
  matchNfaCount = 0

  // Try to match the regular expression from left to right.
  for (
    let index = startIndex;
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
              input.slice(startIndex, index),
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

export const scan_v1 = (
  regExpAsString: string,
  input: string,
  options: RegExpOptionsType = {}
): string[] => {
  let startIndex = 0

  const matches = []

  const nfa = buildNfaFromRegExp(regExpAsString)

  while (true) {
    const match = match_v1(nfa, input, { startIndex, ...options })

    if (typeof match === 'string') break // Match unsuccessful! Stop scan.

    matches.push(match.match)

    startIndex = match.end >= match.start ? match.end + 1 : startIndex + 1
  }

  return matches.flat()
}

///////////
// Tests //
///////////

const assertMatches_v1 = (
  regExpAsString: string,
  input: string,
  matchedResult: string,
  options: BuildNfaFromRegExpAndMatchOptionsType = {}
) => {
  const nfa = buildNfaFromRegExp(regExpAsString, {
    printNodes: options.printNodes,
  })

  const matchData = match_v1(nfa, input, { ...options, arrows: true })

  assertEquals(typeof matchData === 'string' ? matchData : matchData?.match, matchedResult)
}

Deno.test('Repetitions (v1)', () => {
  debugMode = false

  assertMatches_v1('an+', 'banana', 'b->an<-ana')
  assertMatches_v1('(an)+', 'banana', 'b->anan<-a')
  assertMatches_v1('iss', 'mississipi', 'm->iss<-issipi')
  assertMatches_v1('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches_v1('is+', 'mississipi', 'm->iss<-issipi')
  assertMatches_v1('(is+)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches_v1('/d{2}:/d{2}/s*([ap]m)', '12:50 am', '->12:50 am<-')
  assertMatches_v1('a*', '', '-><-')
  assertMatches_v1('a+', '', NO_MATCH_MESSAGE)
  assertMatches_v1('a*', '...aa', '-><-...aa')
  assertMatches_v1('a*', 'aa', '->aa<-')
  assertMatches_v1('a+', '...aa', '...->aa<-')
  assertMatches_v1('a+$', '..aa', '..->aa<-')
  assertMatches_v1('(x+x+)+y', 'xxxxxxxxxxy', '->xxxxxxxxxxy<-')
  assertMatches_v1('(x+x+)+y', 'xxxxxxxxxx', NO_MATCH_MESSAGE)
  assertMatches_v1('(a+)*ab', 'aaaaaaaaaaaab', '->aaaaaaaaaaaab<-')
  assertMatches_v1('.*.*=.*', 'x=x', '->x=x<-')
  assertMatches_v1(
    '.*.*=.*',
    'x=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    '->x=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx<-'
  )
  assertMatches_v1('.*.*=.*', 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx', NO_MATCH_MESSAGE)
})

Deno.test('Repetitions which can potentially match an empty string (v1)', () => {
  debugMode = false

  ////////////////////////////
  // Default branching mode //
  ////////////////////////////

  assertMatches_v1('(a+)*', '', '-><-')
  assertMatches_v1('(a+)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a+)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a*)*', '', '-><-')
  assertMatches_v1('(a*)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a*)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a*)+', '', '-><-')
  assertMatches_v1('(a*)+', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a*)+', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a?)*', '', '-><-')
  assertMatches_v1('(a?)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a?)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a?)+', '', '-><-')
  assertMatches_v1('(a?)+', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a?)+', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a{0,9})*', '', '-><-')
  assertMatches_v1('(a{0,9})*', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a{0,9})*', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a{0,9})+', '', '-><-')
  assertMatches_v1('(a{0,9})+', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('(a{0,9})+', 'baaaaa', '-><-baaaaa')

  assertMatches_v1('(a*b*)*', '', '-><-')
  assertMatches_v1('(a*b*)*', 'aaaaabbb', '->aaaaabbb<-')
  assertMatches_v1('(a*b*)*', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v1('(a?b*)*', '', '-><-')
  assertMatches_v1('(a?b*)*', 'abbb', '->abbb<-')
  assertMatches_v1('(a?b*)*', 'bbb', '->bbb<-')
  assertMatches_v1('(a?b*)*', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v1('(a?b*)+', '', '-><-')
  assertMatches_v1('(a?b*)+', 'abbb', '->abbb<-')
  assertMatches_v1('(a?b*)+', 'bbb', '->bbb<-')
  assertMatches_v1('(a?b*)+', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v1('(a?b*)?', '', '-><-')
  assertMatches_v1('(a?b*)?', 'abbb', '->abbb<-')
  assertMatches_v1('(a?b*)?', 'bbb', '->bbb<-')
  assertMatches_v1('(a?b*)?', 'abbbabbbbbb', '->abbb<-abbbbbb')
  assertMatches_v1('(a?b*)?', 'caaaaabbb', '-><-caaaaabbb')

  ///////////////
  // Lazy mode //
  ///////////////

  assertMatches_v1('(a+)*?', '', '-><-')
  assertMatches_v1('(a*)*?', '', '-><-')
  assertMatches_v1('(a*)+?', '', '-><-')
  assertMatches_v1('(a?)*?', '', '-><-')
  assertMatches_v1('(a?)+?', '', '-><-')
  assertMatches_v1('(a{0,9})*?', '', '-><-')
  assertMatches_v1('(a{0,9})+?', '', '-><-')

  assertMatches_v1('(a+?)*', '', '-><-')
  assertMatches_v1('(a*?)*', '', '-><-')
  assertMatches_v1('(a*?)+', '', '-><-')
  assertMatches_v1('(a??)*', '', '-><-')
  assertMatches_v1('(a??)+', '', '-><-')
  assertMatches_v1('(a{0,9}?)*', '', '-><-')
  assertMatches_v1('(a{0,9}?)+', '', '-><-')

  assertMatches_v1('(a+?)*?', '', '-><-')
  assertMatches_v1('(a*?)*?', '', '-><-')
  assertMatches_v1('(a*?)+?', '', '-><-')
  assertMatches_v1('(a??)*?', '', '-><-')
  assertMatches_v1('(a??)+?', '', '-><-')
  assertMatches_v1('(a{0,9}?)*?', '', '-><-')
  assertMatches_v1('(a{0,9}?)+?', '', '-><-')

  /////////////////////
  // Possessive mode //
  /////////////////////

  assertMatches_v1('(a+)*+', '', '-><-')
  assertMatches_v1('(a*)*+', '', '-><-')
  assertMatches_v1('(a*)++', '', '-><-')
  assertMatches_v1('(a?)*+', '', '-><-')
  assertMatches_v1('(a?)++', '', '-><-')
  assertMatches_v1('(a{0,9})*+', '', '-><-')
  assertMatches_v1('(a{0,9})++', '', '-><-')

  assertMatches_v1('(a++)*', '', '-><-')
  assertMatches_v1('(a*+)*', '', '-><-')
  assertMatches_v1('(a*+)+', '', '-><-')
  assertMatches_v1('(a?+)*', '', '-><-')
  assertMatches_v1('(a?+)+', '', '-><-')
  assertMatches_v1('(a{0,9}+)*', '', '-><-')
  assertMatches_v1('(a{0,9}+)+', '', '-><-')

  assertMatches_v1('(a++)*+', '', '-><-')
  assertMatches_v1('(a*+)*+', '', '-><-')
  assertMatches_v1('(a*+)++', '', '-><-')
  assertMatches_v1('(a?+)*+', '', '-><-')
  assertMatches_v1('(a?+)++', '', '-><-')
  assertMatches_v1('(a{0,9}+)*+', '', '-><-')
  assertMatches_v1('(a{0,9}+)++', '', '-><-')

  assertMatches_v1('(((a*)+)+)+', '', '-><-')
  assertMatches_v1('(((a*)+)+)+', 'a', '->a<-')
  assertMatches_v1('(((a*)+)+)+', 'ba', '-><-ba')
  assertMatches_v1('(((a*)+)+)+', 'aaaaaaaaaaaaaaaaa', '->aaaaaaaaaaaaaaaaa<-')
})

Deno.test('Complex repetitions (v1)', () => {
  debugMode = false

  // Produces complex, multi-level repetitions, such as: "(a+)", "((a+)+)", "(((a+)+)+)" etc.
  const repeatedAs = (n: number) => {
    let regexp = 'a'

    times(n, () => {
      regexp = `(${regexp}+)`
    })

    return regexp
  }

  const m = 10
  const o = 1000
  assertMatches_v1(repeatedAs(m), 'a'.repeat(o), '->' + 'a'.repeat(o) + '<-')

  const n = 10
  assertMatches_v1('a*'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')
  assertMatches_v1('a*'.repeat(n) + 'a'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')
  assertMatches_v1('a?'.repeat(n) + 'a'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')

  assertMatches_v1(
    '(((a*b)+c)?d,){2,3}',
    'd,bcd,aaabababaaabbbbbbcd,d',
    '->d,bcd,aaabababaaabbbbbbcd,<-d'
  )
  assertMatches_v1('(((a*b)+c)?d,){2,3}', 'd', NO_MATCH_MESSAGE)
})

Deno.test('Anchors (v1)', () => {
  debugMode = false

  assertMatches_v1('^a+', '...aa', NO_MATCH_MESSAGE)
  assertMatches_v1('^a+', 'aa', '->aa<-')
  assertMatches_v1('^a+$', 'aa...', NO_MATCH_MESSAGE)
  assertMatches_v1('^a+$', 'aa', '->aa<-')
  assertMatches_v1('^', '', '-><-')
  assertMatches_v1('$', '', '-><-')
  assertMatches_v1('^a*', '', '-><-')
  assertMatches_v1('a*$', '', '-><-')
  assertMatches_v1('\\b', 'some_word', '-><-some_word')
  assertMatches_v1('\\b/w{4}', '           some_word   ', '           ->some<-_word   ')
  assertMatches_v1('/w{4}\\b', '           some_word   ', '           some_->word<-   ')
  assertMatches_v1('\\b/w{4}', 'some_word   ', '->some<-_word   ')
  assertMatches_v1('/w{4}\\b', '           some_word', '           some_->word<-')
  assertMatches_v1(
    '\\b/w\\b',
    '               x              ',
    '               ->x<-              '
  )
  assertMatches_v1('\\b/w\\b', '               xx              ', NO_MATCH_MESSAGE)
})

Deno.test('scan() (v1)', () => {
  debugMode = false

  assertEquals(
    scan_v1(
      '/w+([.]/w+)*@/w+([.]/w+)+',
      '| john.doe@gmail.com | john@gmail.com.us | john.doe@ | @gmail.com | john@gmail | jo.hn.do.e@g.mail.co.m |'
    ),
    ['john.doe@gmail.com', 'john@gmail.com.us', 'jo.hn.do.e@g.mail.co.m']
  )
  assertEquals(scan_v1('/d{2}', '01234567'), ['01', '23', '45', '67'])
  assertEquals(scan_v1('/d', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
  assertEquals(scan_v1('.', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
  // assertEquals(scan_v1('\\b/w', 'regexps are really cool'), ['r', 'a', 'r', 'c'])
  // assertEquals(scan2('/w\\b', 'regexps are really cool'), ['s', 'e', 'y', 'l'])
  assertEquals(scan_v1('/w', 'ab+cd-efg*hijk/lmn'), [
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
  assertEquals(scan_v1('/W', 'ab+cd-efg*hijk/lmn'), ['+', '-', '*', '/'])
  // assertEquals(scan_v1('\\b\\w*\\b', '   aaa    bbb    ccc    '), ['aaa', 'bbb', 'ccc'])
})

Deno.test('jsMultiline on x off behavior (v1)', () => {
  debugMode = false

  assertEquals(scan_v1('^.', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    'r',
    'a',
    'r',
    'c',
  ])
  assertEquals(scan_v1('^.', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['r'])
  assertEquals(scan_v1('.$', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    's',
    'e',
    'y',
    'l',
  ])
  assertEquals(scan_v1('.$', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['l'])
})

Deno.test('Greedy (default) x lazy behavior (v1)', () => {
  debugMode = false

  assertMatches_v1('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches_v1('(iss)+?', 'mississipi', 'm->iss<-issipi')

  assertMatches_v1('a*', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('a*?', 'aaaaa', '-><-aaaaa')

  assertMatches_v1('a+', 'aaaaa', '->aaaaa<-')
  assertMatches_v1('a+?', 'aaaaa', '->a<-aaaa')
})

Deno.test('Backtrackable (default) x possessive behavior (v1)', () => {
  debugMode = false

  assertMatches_v1('a*a', 'aaaa', '->aaaa<-')
  assertMatches_v1('a*+a', 'aaaa', NO_MATCH_MESSAGE)

  assertMatches_v1('a+a', 'aaaa', '->aaaa<-')
  assertMatches_v1('a++a', 'aaaa', NO_MATCH_MESSAGE)

  assertMatches_v1('a+b', 'aaaab', '->aaaab<-')
  assertMatches_v1('a++b', 'aaaab', '->aaaab<-')

  assertMatches_v1('a+ab', 'aaaab', '->aaaab<-')
  assertMatches_v1('a++ab', 'aaaab', NO_MATCH_MESSAGE)

  assertEquals(scan_v1('/d++', '1234567890'), ['1234567890'])
  assertEquals(scan_v1('/d*+', '1234567890'), ['1234567890'])
})

const assertMatches_v2 = (
  regExpAsString: string,
  input: string,
  matchedResult: string,
  options: BuildNfaFromRegExpAndMatchOptionsType = {}
) => {
  const nfa = buildNfaFromRegExp(regExpAsString, {
    printNodes: options.printNodes,
  })

  const matchData = match_v2(nfa, input, { ...options, arrows: true })

  assertEquals(typeof matchData === 'string' ? matchData : matchData?.match, matchedResult)
}

Deno.test('Repetitions (v2)', () => {
  debugMode = false

  assertMatches_v2('an+', 'banana', 'b->an<-ana')
  assertMatches_v2('(an)+', 'banana', 'b->anan<-a')
  assertMatches_v2('iss', 'mississipi', 'm->iss<-issipi')
  assertMatches_v2('(iss)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches_v2('is+', 'mississipi', 'm->iss<-issipi')
  assertMatches_v2('(is+)+', 'mississipi', 'm->ississ<-ipi')
  assertMatches_v2('/d{2}:/d{2}/s*([ap]m)', '12:50 am', '->12:50 am<-')
  assertMatches_v2('a*', '', '-><-')
  assertMatches_v2('a+', '', NO_MATCH_MESSAGE)
  assertMatches_v2('a*', '...aa', '-><-...aa')
  assertMatches_v2('a*', 'aa', '->aa<-')
  assertMatches_v2('a+', '...aa', '...->aa<-')
  assertMatches_v2('a+$', '..aa', '..->aa<-')
  assertMatches_v2('(x+x+)+y', 'xxxxxxxxxxy', '->xxxxxxxxxxy<-')
  assertMatches_v2('(x+x+)+y', 'xxxxxxxxxx', NO_MATCH_MESSAGE)
  assertMatches_v2('(a+)*ab', 'aaaaaaaaaaaab', '->aaaaaaaaaaaab<-')
  assertMatches_v2('.*.*=.*', 'x=x', '->x=x<-')
  assertMatches_v2(
    '.*.*=.*',
    'x=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    '->x=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx<-'
  )
  assertMatches_v2('.*.*=.*', 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx', NO_MATCH_MESSAGE)
})

Deno.test('Repetitions which can potentially match an empty string (v2)', () => {
  debugMode = false

  ////////////////////////////
  // Default branching mode //
  ////////////////////////////

  assertMatches_v2('(a+)*', '', '-><-')
  assertMatches_v2('(a+)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a+)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a*)*', '', '-><-')
  assertMatches_v2('(a*)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a*)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a*)+', '', '-><-')
  assertMatches_v2('(a*)+', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a*)+', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a?)*', '', '-><-')
  assertMatches_v2('(a?)*', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a?)*', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a?)+', '', '-><-')
  assertMatches_v2('(a?)+', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a?)+', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a{0,9})*', '', '-><-')
  assertMatches_v2('(a{0,9})*', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a{0,9})*', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a{0,9})+', '', '-><-')
  assertMatches_v2('(a{0,9})+', 'aaaaa', '->aaaaa<-')
  assertMatches_v2('(a{0,9})+', 'baaaaa', '-><-baaaaa')

  assertMatches_v2('(a*b*)*', '', '-><-')
  assertMatches_v2('(a*b*)*', 'aaaaabbb', '->aaaaabbb<-')
  assertMatches_v2('(a*b*)*', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v2('(a?b*)*', '', '-><-')
  assertMatches_v2('(a?b*)*', 'abbb', '->abbb<-')
  assertMatches_v2('(a?b*)*', 'bbb', '->bbb<-')
  assertMatches_v2('(a?b*)*', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v2('(a?b*)+', '', '-><-')
  assertMatches_v2('(a?b*)+', 'abbb', '->abbb<-')
  assertMatches_v2('(a?b*)+', 'bbb', '->bbb<-')
  assertMatches_v2('(a?b*)+', 'caaaaabbb', '-><-caaaaabbb')

  assertMatches_v2('(a?b*)?', '', '-><-')
  assertMatches_v2('(a?b*)?', 'abbb', '->abbb<-')
  assertMatches_v2('(a?b*)?', 'bbb', '->bbb<-')
  assertMatches_v2('(a?b*)?', 'abbbabbbbbb', '->abbb<-abbbbbb')
  assertMatches_v2('(a?b*)?', 'caaaaabbb', '-><-caaaaabbb')

  ///////////////
  // Lazy mode //
  ///////////////

  assertMatches_v2('(a+)*?', '', '-><-')
  assertMatches_v2('(a*)*?', '', '-><-')
  assertMatches_v2('(a*)+?', '', '-><-')
  assertMatches_v2('(a?)*?', '', '-><-')
  assertMatches_v2('(a?)+?', '', '-><-')
  assertMatches_v2('(a{0,9})*?', '', '-><-')
  assertMatches_v2('(a{0,9})+?', '', '-><-')

  assertMatches_v2('(a+?)*', '', '-><-')
  assertMatches_v2('(a*?)*', '', '-><-')
  assertMatches_v2('(a*?)+', '', '-><-')
  assertMatches_v2('(a??)*', '', '-><-')
  assertMatches_v2('(a??)+', '', '-><-')
  assertMatches_v2('(a{0,9}?)*', '', '-><-')
  assertMatches_v2('(a{0,9}?)+', '', '-><-')

  assertMatches_v2('(a+?)*?', '', '-><-')
  assertMatches_v2('(a*?)*?', '', '-><-')
  assertMatches_v2('(a*?)+?', '', '-><-')
  assertMatches_v2('(a??)*?', '', '-><-')
  assertMatches_v2('(a??)+?', '', '-><-')
  assertMatches_v2('(a{0,9}?)*?', '', '-><-')
  assertMatches_v2('(a{0,9}?)+?', '', '-><-')

  /////////////////////
  // Possessive mode //
  /////////////////////

  assertMatches_v2('(a+)*+', '', '-><-')
  assertMatches_v2('(a*)*+', '', '-><-')
  assertMatches_v2('(a*)++', '', '-><-')
  assertMatches_v2('(a?)*+', '', '-><-')
  assertMatches_v2('(a?)++', '', '-><-')
  assertMatches_v2('(a{0,9})*+', '', '-><-')
  assertMatches_v2('(a{0,9})++', '', '-><-')

  assertMatches_v2('(a++)*', '', '-><-')
  assertMatches_v2('(a*+)*', '', '-><-')
  assertMatches_v2('(a*+)+', '', '-><-')
  assertMatches_v2('(a?+)*', '', '-><-')
  assertMatches_v2('(a?+)+', '', '-><-')
  assertMatches_v2('(a{0,9}+)*', '', '-><-')
  assertMatches_v2('(a{0,9}+)+', '', '-><-')

  assertMatches_v2('(a++)*+', '', '-><-')
  assertMatches_v2('(a*+)*+', '', '-><-')
  assertMatches_v2('(a*+)++', '', '-><-')
  assertMatches_v2('(a?+)*+', '', '-><-')
  assertMatches_v2('(a?+)++', '', '-><-')
  assertMatches_v2('(a{0,9}+)*+', '', '-><-')
  assertMatches_v2('(a{0,9}+)++', '', '-><-')

  assertMatches_v2('(((a*)+)+)+', '', '-><-')
  assertMatches_v2('(((a*)+)+)+', 'a', '->a<-')
  assertMatches_v2('(((a*)+)+)+', 'ba', '-><-ba')
  assertMatches_v2('(((a*)+)+)+', 'aaaaaaaaaaaaaaaaa', '->aaaaaaaaaaaaaaaaa<-')
})

Deno.test('Complex repetitions (v2)', () => {
  debugMode = false

  // Produces complex, multi-level repetitions, such as: "(a+)", "((a+)+)", "(((a+)+)+)" etc.
  const repeatedAs = (n: number) => {
    let regexp = 'a'

    times(n, () => {
      regexp = `(${regexp}+)`
    })

    return regexp
  }

  const m = 10
  const o = 1000
  assertMatches_v2(repeatedAs(m), 'a'.repeat(o), '->' + 'a'.repeat(o) + '<-')

  const n = 100
  assertMatches_v2('a*'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')
  assertMatches_v2('a*'.repeat(n) + 'a'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')
  assertMatches_v2('a?'.repeat(n) + 'a'.repeat(n), 'a'.repeat(n), '->' + 'a'.repeat(n) + '<-')

  assertMatches_v2(
    '(((a*b)+c)?d,){2,3}',
    'd,bcd,aaabababaaabbbbbbcd,d',
    '->d,bcd,aaabababaaabbbbbbcd,<-d'
  )
  assertMatches_v2('(((a*b)+c)?d,){2,3}', 'd', NO_MATCH_MESSAGE)
})

Deno.test('Anchors (v2)', () => {
  debugMode = false

  assertMatches_v2('^a+', '...aa', NO_MATCH_MESSAGE)
  assertMatches_v2('^a+', 'aa', '->aa<-')
  assertMatches_v2('^a+$', 'aa...', NO_MATCH_MESSAGE)
  assertMatches_v2('^a+$', 'aa', '->aa<-')
  assertMatches_v2('^', '', '-><-')
  assertMatches_v2('$', '', '-><-')
  assertMatches_v2('^a*', '', '-><-')
  assertMatches_v2('a*$', '', '-><-')
  // assertMatches('\\b', 'some_word', '-><-some_word')
  // assertMatches('\\b/w{4}', '           some_word   ', '           ->some<-_word   ')
  // assertMatches('/w{4}\\b', '           some_word   ', '           some_->word<-   ')
  // assertMatches('\\b/w{4}', 'some_word   ', '->some<-_word   ')
  // assertMatches('/w{4}\\b', '           some_word', '           some_->word<-')
  // assertMatches('\\b/w\\b', '               x              ', '               ->x<-              ')
  // assertMatches('\\b/w\\b', '               xx              ', NO_MATCH_MESSAGE)
})

Deno.test('scan() (v2)', () => {
  debugMode = false

  assertEquals(
    scan_v2(
      '/w+([.]/w+)*@/w+([.]/w+)+',
      '| john.doe@gmail.com | john@gmail.com.us | john.doe@ | @gmail.com | john@gmail | jo.hn.do.e@g.mail.co.m |'
    ),
    ['john.doe@gmail.com', 'john@gmail.com.us', 'jo.hn.do.e@g.mail.co.m']
  )
  assertEquals(scan_v2('/d{2}', '01234567'), ['01', '23', '45', '67'])
  assertEquals(scan_v2('/d', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
  assertEquals(scan_v2('.', '01234567'), ['0', '1', '2', '3', '4', '5', '6', '7'])
  // assertEquals(scan_v2('\\b/w', 'regexps are really cool'), ['r', 'a', 'r', 'c'])
  // assertEquals(scan2('/w\\b', 'regexps are really cool'), ['s', 'e', 'y', 'l'])
  assertEquals(scan_v2('/w', 'ab+cd-efg*hijk/lmn'), [
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
  assertEquals(scan_v2('/W', 'ab+cd-efg*hijk/lmn'), ['+', '-', '*', '/'])
  // assertEquals(scan_v2('\\b\\w*\\b', '   aaa    bbb    ccc    '), ['aaa', 'bbb', 'ccc'])
})

Deno.test('jsMultiline on x off behavior (v2)', () => {
  debugMode = false

  assertEquals(scan_v2('^.', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    'r',
    'a',
    'r',
    'c',
  ])
  assertEquals(scan_v2('^.', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['r'])
  assertEquals(scan_v2('.$', 'regexps\nare\nreally\ncool', { jsMultiline: true }), [
    's',
    'e',
    'y',
    'l',
  ])
  assertEquals(scan_v2('.$', 'regexps\nare\nreally\ncool', { jsMultiline: false }), ['l'])
})

// Deno.test('Greedy (default) x lazy behavior (v2)', () => {
//   debugMode = false

//   assertMatches('(iss)+', 'mississipi', 'm->ississ<-ipi')
//   assertMatches('(iss)+?', 'mississipi', 'm->iss<-issipi')

//   assertMatches('a*', 'aaaaa', '->aaaaa<-')
//   assertMatches('a*?', 'aaaaa', '-><-aaaaa')

//   assertMatches('a+', 'aaaaa', '->aaaaa<-')
//   assertMatches('a+?', 'aaaaa', '->a<-aaaa')
// })

// Deno.test('Backtrackable (default) x possessive behavior (v2)', () => {
//   debugMode = false

//   assertMatches('a*a', 'aaaa', '->aaaa<-')
//   assertMatches('a*+a', 'aaaa', NO_MATCH_MESSAGE)

//   assertMatches('a+a', 'aaaa', '->aaaa<-')
//   assertMatches('a++a', 'aaaa', NO_MATCH_MESSAGE)

//   assertMatches('a+b', 'aaaab', '->aaaab<-')
//   assertMatches('a++b', 'aaaab', '->aaaab<-')

//   assertMatches('a+ab', 'aaaab', '->aaaab<-')
//   assertMatches('a++ab', 'aaaab', NO_MATCH_MESSAGE)

//   assertEquals(scan('/d++', '1234567890'), ['1234567890'])
//   assertEquals(scan('/d*+', '1234567890'), ['1234567890'])
// })

declare const Deno: {
  inspect: (...args: unknown[]) => void
  test: (title: string, testFn: () => void) => void
  stdin: { read: (...args: unknown[]) => void }
  writeTextFile: (filename: string, contents: string) => Promise<void>
  run: (options: { cmd: string[] }) => { status: (...args: unknown[]) => Promise<void> }
}

export const log = console.log

// deno-lint-ignore no-explicit-any
export const inspect = (value: any) =>
  Deno.inspect(value, { depth: 999, colors: true }) as unknown as string
export const print = (value: object) => log(inspect(value))

export const showRegExp = (regExpAsString: string) => print(buildRegExpAst(regExpAsString))

export const visit = <T>(
  node: NodeType,
  fn?: (node: NodeType) => T,
  breadcrumbs: NodeType[] = []
): (T | NodeType)[] => {
  if (breadcrumbs.indexOf(node) >= 0) return []

  breadcrumbs.push(node)

  const result: T | NodeType = (fn ?? (node => node))(node)

  switch (node.type) {
    case 'CNode':
      return [result]
        .concat(visit(node.next, fn, breadcrumbs))
        .concat(visit(node.nextAlt, fn, breadcrumbs))

    case 'NNode':
      return [result].concat(visit(node.next, fn, breadcrumbs))

    case 'ENode':
    case 'FNode':
      return [result]
  }
}

export const asGraphviz = async (
  regExpAsString: string,
  { showIds = false, topToBottom = false }
): Promise<NodeType> => {
  const nfa = buildNfaFromRegExp(regExpAsString)
  const nodes: NodeType[] = visit(nfa)
  const cNodes = nodes.filter(node => node.type === 'CNode')
  const nNodes = nodes.filter(node => node.type === 'NNode')
  const fNodeIsPresent = nodes.some(node => node.type === 'FNode')

  const label = (node: NodeType, showIds = false) => {
    const optionalNodeId = showIds ? ` [${node.id}]` : ''

    switch (node.type) {
      case 'CNode':
        return `"†${optionalNodeId}${
          node.branchingMode !== 'default' ? `\n(${node.branchingMode})` : ''
        }"`
      case 'NNode':
        return `"'${node.character
          .replaceAll('\n', '\\n')
          .replaceAll('\\', '\\\\')}'${optionalNodeId}${node.isLiteral ? '' : '\n(non-literal)'}"`
      case 'ENode':
        return 'end'
      case 'FNode':
        return 'fail'
    }
  }

  const SPACES_AFTER_EDGE_LABEL_WHEN_TOP_BOTTOM = 3
  const LINES_BEFORE_EDGE_LABEL_WHEN_LEFT_RIGHT = 2

  const edgeLabel = (branch: string) =>
    topToBottom
      ? branch + ' '.repeat(SPACES_AFTER_EDGE_LABEL_WHEN_TOP_BOTTOM)
      : '\n'.repeat(LINES_BEFORE_EDGE_LABEL_WHEN_LEFT_RIGHT) + branch

  const edges = (node: NodeType): string[] => {
    const labels = {
      node: label(node, true),
      nodeNext: 'next' in node ? label(node.next, true) : '',
      nodeNextAlt: 'nextAlt' in node ? label(node.nextAlt, true) : '',
    }

    const edges = {
      nodeToNext: `  ${labels.node} -> ${labels.nodeNext} [label="${edgeLabel('next')}"]`,
      nodeToNextAlt: `  ${labels.node} -> ${labels.nodeNextAlt} [label="${edgeLabel('nextAlt')}"]`,
    }

    switch (node.type) {
      case 'CNode':
        return [edges.nodeToNext, edges.nodeToNextAlt]
      case 'NNode':
        return [edges.nodeToNext]
      case 'ENode':
      case 'FNode':
        return []
    }
  }

  // https://graphviz.org/doc/info/attrs.html
  let dot = ''

  dot += 'digraph {\n'
  dot += '  dpi=192;\n'
  dot += `  rankdir=${topToBottom ? 'TB' : 'LR'};\n`

  dot += '  labelloc="t";\n'
  dot += `  label="/${regExpAsString.replaceAll('\\', '\\\\')}/";\n\n`

  dot += '  # Node edges\n'
  dot += `  start -> ${label(nfa, true)};\n`
  dot += nodes.flatMap(edges).join(';\n') + ';\n\n'

  dot += '  # Node labels, shapes, colors etc\n'
  dot += '  start [shape=circle, style=filled, color=gray, fontcolor=black];\n'
  dot += '  end [shape=doublecircle, style=filled, color=orange, fontcolor=black];\n'

  if (fNodeIsPresent) dot += '  fail [shape=circle, style=filled, color=red];\n'

  dot +=
    cNodes
      .map(
        node =>
          '  ' + label(node, true) + ` [label=${label(node, showIds)}, shape=rect, color=blue];`
      )
      .join('\n') + '\n'

  dot +=
    nNodes
      .map(
        node =>
          '  ' +
          label(node, true) +
          ` [label=${label(
            node,
            showIds
          )}, shape=ellipse, color=darkgreen, style=filled, fontcolor=white];`
      )
      .join('\n') + '\n'

  dot += '}'

  await Deno.writeTextFile('graphviz/regexp.dot', dot)
  await Deno.run({
    cmd: ['dot', '-Tpng', 'graphviz/regexp.dot', '-o', 'graphviz/regexp.png'],
  }).status()
  await Deno.run({ cmd: ['open', 'graphviz/regexp.png'] }).status()

  return nfa
}

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)

const debug = (messageOrFalse: () => string | false): void => {
  if (debugMode) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

// ------------------------------------------------------------------------------------------------

class FailedNodeReached extends Error {}

const successorTerminalNodes = (
  node: NodeType,
  skipCaretAndDollarAnchorNodes = false,
  breadcrumbs: NodeType[] = []
): (NNodeType | ENodeType)[] => {
  // Avoid an infinite loop when node already visited!
  if (breadcrumbs.indexOf(node) >= 0) return []

  breadcrumbs.push(node)

  const nextSuccessors = <T extends { next: NodeType }>(someNode: T) =>
    successorTerminalNodes(someNode.next, skipCaretAndDollarAnchorNodes, breadcrumbs)

  const nextAltSuccessors = <T extends { nextAlt: NodeType }>(someNode: T) =>
    successorTerminalNodes(someNode.nextAlt, skipCaretAndDollarAnchorNodes, breadcrumbs)

  switch (node.type) {
    case 'CNode':
      return nextSuccessors(node).concat(nextAltSuccessors(node))

    case 'FNode':
      throw new FailedNodeReached()

    case 'NNode':
      return skipCaretAndDollarAnchorNodes &&
        !node.isLiteral &&
        [CARET, DOLLAR_SIGN].includes(node.character)
        ? nextSuccessors(node)
        : [node]

    default:
      return [node]
  }
}

const removeDuplicates = <T>(collection: T[]): T[] => [...new Set(collection)]

const findSuccessorTerminalNodesTreatingNonLiteralDollarAnchorNode = (
  currentNode: NNodeType,
  nextChar: SingleChar,
  options: RegExpOptionsType
) => {
  const nextNodes = successorTerminalNodes(currentNode.next)

  // Next nodes contain a non-literal '$'?
  const nonLiteralDollarNodeIndex = nextNodes.findIndex(
    node => node.type === 'NNode' && !node.isLiteral && node.character === DOLLAR_SIGN
  )

  if (nonLiteralDollarNodeIndex >= 0) {
    debug(() => `Found $ node at index ${nonLiteralDollarNodeIndex}`)

    const node = nextNodes[nonLiteralDollarNodeIndex] as NNodeType

    // Are we at the end of the string or current line (depending on the `jsMultiline` option)?
    if (options.jsMultiline ? nextChar === '' || nextChar === '\n' : nextChar === '') {
      const successors = successorTerminalNodes(node.next)

      debug(
        () =>
          `Replacing $ node by its successor(s): ${successors.map(n => nodeAsString(n)).join(', ')}`
      )

      // Yes. Replace the non-literal '$' node by its terminal successors.
      nextNodes.splice(nonLiteralDollarNodeIndex, 1, ...successors)

      debug(() => `nextNodes: ${nextNodes.map(n => nodeAsString(n)).join(', ')}`)
    }
  }

  return nextNodes
}

export const findMatches = (
  previousChar: SingleChar,
  currentChar: SingleChar,
  nextChar: SingleChar,
  nodeList: (NNodeType | ENodeType)[],
  options: RegExpOptionsType,
  newNodeList: (NNodeType | ENodeType)[] = []
): (NNodeType | ENodeType)[] => {
  return removeDuplicates(
    nodeList.flatMap(currentNode => {
      if (newNodeList.indexOf(currentNode) >= 0) return newNodeList

      debug(() => `[Trying to match '${currentChar}' against node ${nodeAsString(currentNode)}`)

      switch (currentNode.type) {
        case 'NNode': {
          if (currentNode.isLiteral) {
            return currentNode.character === currentChar
              ? (debug(() => 'Matched'),
                newNodeList.concat(
                  findSuccessorTerminalNodesTreatingNonLiteralDollarAnchorNode(
                    currentNode,
                    nextChar,
                    options
                  )
                ))
              : (debug(() => 'Not matched'), newNodeList)
          } else {
            switch (currentNode.character) {
              case PERIOD: // A '.' matches anything but the new line (\n).
                return currentChar !== NEW_LINE
                  ? (debug(() => 'Matched'),
                    newNodeList.concat(
                      findSuccessorTerminalNodesTreatingNonLiteralDollarAnchorNode(
                        currentNode,
                        nextChar,
                        options
                      )
                    ))
                  : (debug(() => 'Not matched'), newNodeList)

              case CARET: // A '^' matches the start of the input string or of each individual line.
                return (
                  options.jsMultiline
                    ? previousChar == '' || previousChar === '\n'
                    : previousChar == ''
                )
                  ? (debug(() => 'Matched!'),
                    newNodeList.concat(
                      findMatches(
                        previousChar,
                        currentChar,
                        nextChar,
                        findSuccessorTerminalNodesTreatingNonLiteralDollarAnchorNode(
                          currentNode,
                          nextChar,
                          options
                        ),
                        options,
                        newNodeList
                      )
                    ))
                  : (debug(() => 'Not matched'), newNodeList)

              case DOLLAR_SIGN: // A '$' matches the end of the input string or of each individual line.
                return []

              // case UNESCAPED_WORD_BOUNDARY: // A '\b' matches the (empty) string immediately before or after a "word".
              //   return (
              //     // Either it is a left boundary...
              //     ((previousChar === '' || !isWordChar(previousChar)) && isWordChar(currentChar)) ||
              //       // ...or a right boundary.
              //       ((currentChar === '' || !isWordChar(currentChar)) && isWordChar(previousChar))
              //       ? (debug(() => 'Matched!'),
              //         newNodeList.concat(
              //           findMatches(
              //             previousChar,
              //             currentChar,
              //             nextChar,
              //             findSuccessorNodesAndTreatNonLiteralDollarNode(
              //               currentNode,
              //               nextChar,
              //               options
              //             ),
              //             options,
              //             newNodeList
              //           )
              //         ))
              //       : (debug(() => 'Not matched'), newNodeList)
              //   )

              default:
                throw new Error(`Invalid non-literal character '${currentNode.character}'`)
            }
          }
        }

        case 'ENode':
          return currentChar === ''
            ? (debug(() => 'Matched'), newNodeList.concat(currentNode))
            : (debug(() => 'Not matched'), newNodeList)
      }
    })
  )
}

export const matchFromIndex = (
  nfa: NodeType,
  input: string,
  startIndex: number,
  options: BuildNfaFromRegExpAndMatchOptionsType
): MatchFromNfaReturnType => {
  let endIndex: number | undefined = undefined

  const initialNodeList = successorTerminalNodes(nfa, true)

  debug(() => 'initial list: ' + inspect(initialNodeList.map(nodeAsString)))

  // The initial list having the end node means that the regexp matches an empty string ('').
  if (initialNodeList.indexOf(endNode) >= 0) endIndex = startIndex - 1 // endIndex < startIndex = empty string.

  debug(() => inspect({ startIndex, endIndex }))

  let nodeList = successorTerminalNodes(nfa)

  for (let index = startIndex; index < input.length; index++) {
    const previousChar = index === 0 ? '' : input[index - 1]
    const currentChar = input[index]
    const nextChar = index === input.length - 1 ? '' : input[index + 1]

    debug(
      () =>
        `[index: ${index}, previousChar: '${previousChar}', currentChar: '${currentChar}', nextChar: '${nextChar}']`
    )
    debug(() => 'list before findMatches(): ' + inspect(nodeList.map(nodeAsString)))

    try {
      nodeList = findMatches(previousChar, currentChar, nextChar, nodeList, options)
    } catch (err) {
      if (err instanceof FailedNodeReached) break

      throw err
    }

    debug(() => 'list after findMatches(): ' + inspect(nodeList.map(nodeAsString)))

    if (nodeList.length === 0) break // No further matches.

    // If match found, update the end index in order to include all characters up to (and including) the current character.
    if (nodeList.indexOf(endNode) >= 0) endIndex = index

    debug(() => inspect({ index, startIndex, endIndex }))
  }

  if (endIndex !== undefined) {
    const matchedString = input.slice(startIndex, endIndex + 1)

    return {
      match: options.arrows
        ? [input.slice(0, startIndex), '->', matchedString, '<-', input.slice(endIndex + 1)].join(
            EMPTY_STRING
          )
        : matchedString,
      start: startIndex,
      end: endIndex,
    }
  } else return NO_MATCH_MESSAGE
}

export const match_v2 = (
  nfa: NodeType,
  input: string,
  {
    exactMatch = false,
    arrows = false,
    startIndex = 0,
    jsMultiline = true,
  }: BuildNfaFromRegExpAndMatchOptionsType = {}
) => {
  let match: MatchFromNfaReturnType | undefined = undefined

  for (let index = startIndex; index < Math.max(startIndex + 1, input.length); index++) {
    match = matchFromIndex(nfa, input, index, { exactMatch, arrows, startIndex, jsMultiline })

    if (match !== NO_MATCH_MESSAGE) break
  }

  return match!
}

export const scan_v2 = (
  regExpAsString: string,
  input: string,
  options: RegExpOptionsType = {}
): string[] => {
  let startIndex = 0

  const matches = []

  const nfa = buildNfaFromRegExp(regExpAsString)

  while (true) {
    const match = match_v2(nfa, input, { startIndex, ...options })

    if (typeof match === 'string') break // Match unsuccessful! Stop scan.

    matches.push(match.match)

    startIndex = match.end >= match.start ? match.end + 1 : startIndex + 1
  }

  return matches.flat()
}

// https://dl.acm.org/doi/epdf/10.1145/363347.363387
// import * as re from './regexp_nfa.ts'
// const n = 5; const nfa = await re.asGraphviz('a?'.repeat(n) + 'a'.repeat(n), { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'a'.repeat(n)))
// const n = 5, m = 10; const nfa = await re.asGraphviz('a*'.repeat(n), { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'a'.repeat(m)))
// const nfa = await re.asGraphviz('(is+)+', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'mississipi'))
// const nfa = await re.asGraphviz('a+', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'aaa'))
// const nfa = await re.asGraphviz('a+', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, ''))
// const nfa = await re.asGraphviz('a*', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'aaa'))
// const nfa = await re.asGraphviz('a*', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, ''))
// const nfa = await re.asGraphviz('a*', { showIds: true, topToBottom: false }); console.log(re.match_v2(nfa, 'baa'))
