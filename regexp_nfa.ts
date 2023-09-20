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

export const debug = (messageOrFalse: () => string | false): void => {
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

let nNodeCount: number
let cNodeCount: number

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

const nodeAsString = (node: NodeType | null | undefined) =>
  node === null || node === undefined ? node : `${node.type} #${node.id}`

// Clones a node, setting its `next` and `nextAlt` (in the case of a CNode) props which are `undefined` to `defaultNext`.
const cloneNode = (
  node: NodeType | null | undefined,
  defaultNext: NodeType | null | undefined,
  clones: Map<NodeType, NodeType> = new Map()
): NodeType | null | undefined => {
  if (node === null || node === undefined) return node

  // Node already cloned?
  if (clones.has(node)) return clones.get(node)

  let clone: NodeType

  // Clone the node, delaying the creation of its `next` and `nextAlt` (in the case of a CNode) props, so the new node can be added right away
  // into the clones map.
  switch (node.type) {
    case 'NNode':
      clone = createNNode(node.character, {
        escaped: node.escaped,
      })
      break

    case 'CNode':
      clone = createCNode(undefined, undefined)
      break

    default: {
      const _exhaustiveCheck: never = node
      throw new Error('Invalid NFA node type')
    }
  }

  clones.set(node, clone)

  // With the new clone properly saved in the `clones` map, set its `next` prop.
  clone.next = node.next === undefined ? defaultNext : cloneNode(node.next, defaultNext, clones)

  // Set its `nextAlt` prop (for CNodes).
  if (node.type === 'CNode' && clone.type === 'CNode')
    clone.nextAlt =
      node.nextAlt === undefined ? defaultNext : cloneNode(node.nextAlt, defaultNext, clones)

  return clone
}

export const createNfaFromAst = (ast: RegExpType, nextNode?: NodeType | null): NodeType => {
  let next: NodeType | null | undefined = nextNode

  for (let i = ast.length - 1; i >= 0; i--) {
    next = createNfaNodeFromRegExpToken(ast[i], next)
  }

  return next!
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
        createNfaFromAst(astNode.left, nextNode),
        createNfaFromAst(astNode.right, nextNode)
      )

    case 'parenthesized':
      return createNfaFromAst(astNode.expr, nextNode)

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
      let lastNode: NodeType = createNNode(options.at(-1)!, { next: nextNode, escaped: true })

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
        const node = createNNode(options[i], { next: nextNode, escaped: true })

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
      const _exhaustiveCheck: never = astNode
      throw new Error('Invalid AST node type')
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

export const matchNfa = (
  currentNode: NodeType | null | undefined,
  input: string,
  index = 0,
  previousChar: SingleChar
): { matched: boolean; input: string; index: number } => {
  matchNfaCount++

  if (currentNode === undefined) return { matched: true, input, index }
  if (currentNode === null) throw new Error(NO_MATCH_MESSAGE) // return { matched: false, input, index }

  const isEmptyInput = input.length === 0
  const isStartOfInput = index === 0

  switch (currentNode.type) {
    case 'NNode': {
      const currentChar = isEmptyInput ? '' : input[0]
      const rest = input.slice(1)

      debug(
        () =>
          `[input: '${input}', index: ${index}, previousChar: '${previousChar}', currentChar: '${currentChar}'] Trying to match character '${
            currentChar ?? ''
          }' against node ${nodeAsString(currentNode)}`
      )

      if (currentNode.escaped) {
        if (currentChar === currentNode.character)
          // Matches character literally.
          return debug(() => 'Matched!'), matchNfa(currentNode.next, rest, index + 1, currentChar)
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
                debug(() => 'Matched!'), matchNfa(currentNode.next, rest, index + 1, currentChar)
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
                debug(() => 'Matched!'), matchNfa(currentNode.next, rest, index + 1, currentChar)
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
      else {
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

export const buildAndMatch = (
  regExpAsString: string,
  input: string,
  { exactMatch = false, printNodes = true, arrows = false, startingIndex = 0 } = {}
): { match: string; start: number; end: number } | typeof NO_MATCH_MESSAGE => {
  const nfa = buildNfaFromRegExp(regExpAsString, {
    printNodes,
  })

  matchNfaCount = 0

  // Try to match the regular expression from left to right.
  for (
    let index = startingIndex;
    index < startingIndex + (exactMatch || input.length === 0 ? 1 : input.length);
    index++
  ) {
    let rest: string

    const slicedInput = input.slice(index)

    const match = matchNfa(nfa, slicedInput, index, index > 0 ? input[index - 1] : '')

    debug(() => `match: ${inspect(match)}, accumulated matchNfaCount: ${matchNfaCount}`)

    if (match.matched) {
      const matchedString = input.slice(index, match.index)

      return {
        match: arrows
          ? [input.slice(0, index), '->', matchedString, '<-', input.slice(match.index)].join(
              EMPTY_STRING
            )
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

  while (true) {
    const match = buildAndMatch(regExpAsString, input, { startingIndex })

    if (typeof match !== 'string') {
      matches.push(match.match)

      startingIndex += match.end + 1
    } else {
      break // Match unsuccessful! Stop scan.
    }
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
  const match = buildAndMatch(regExpAsString, input, { exactMatch, arrows: true })

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
assertMatches('/d{2}:/d{2}/s*([ap]m)', '12:50 am', '->12:50 am<-')
assertMatches('a*', '...aa', '-><-...aa')
assertMatches('a*', 'aa', '->aa<-')
assertMatches('a+', '...aa', '...->aa<-')
assertMatches('a+$', '..aa', '..->aa<-')
assertMatches('(x+x+)+y', 'xxxxxxxxxxy', '->xxxxxxxxxxy<-')
assertMatches('(x+x+)+y', 'xxxxxxxxxx', NO_MATCH_MESSAGE)
assertMatches('(a+)*ab', 'aaaaaaaaaaaab', '->aaaaaaaaaaaab<-')
assertMatches('.*.*=.*', 'x=x', '->x=x<-')
assertMatches('a*'.repeat(100), 'a'.repeat(1000), '->' + 'a'.repeat(1000) + '<-')

// Anchors.
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
assertEquals(
  scan('^.', 'abc\ndef\nghi'),
  ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS ? ['a', 'd', 'g'] : ['a']
)
assertEquals(
  scan('.$', 'abc\ndef\nghi'),
  ENABLE_JS_BEHAVIOR_FOR_CARET_AND_DOLLAR_ANCHORS ? ['c', 'f', 'i'] : ['i']
)

assertEquals(
  scan(
    '/w+([.]/w+)*@/w+([.]/w+)+',
    '| john.doe@gmail.com | john@gmail.com.us | john.doe@ | @gmail.com | john@gmail | jo.hn.do.e@g.mail.co.m |'
  ),
  ['john.doe@gmail.com', 'john@gmail.com.us', 'jo.hn.do.e@g.mail.co.m']
)

log('Done!')

// Reset the node counters.
nNodeCount = 0
cNodeCount = 0

debugMode = previousDebugMode

// > re.buildAndMatch('(/.+/w+ /d+/.+)+', '...xxx 123.........yyy 4.........z 5678...')
// { match: "->...xxx 123.........<-yyy 4.........z 5678...", index: 0 }
