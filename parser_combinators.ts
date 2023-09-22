import { RefType } from './spreadsheet_utils.ts'

type MemoizableFnType<T> = (...args: any[]) => T

export const memoize = <T>(fn: MemoizableFnType<T>): MemoizableFnType<T> => {
  const cache: { key: unknown[]; value: T }[] = []

  const memoizedFn: MemoizableFnType<T> = (...args) => {
    let value: T

    const entry = cache.find(({ key }) => key.every((param, i) => param === args[i]))

    if (entry !== undefined) {
      value = entry.value
    } else {
      value = fn(...args)

      cache.unshift({ key: args, value })
    }

    return value
  }

  return memoizedFn
}

// const memoize = <T>(fn: MemoizableFnType<T>): MemoizableFnType<T> => {
//   const cache: { [key: string]: T } = {}
//
//   const memoizedFn: MemoizableFnType<T> = (...args) => {
//     if (args.some(arg => typeof arg === 'function'))
//       throw new Error(
//         'Sorry, this function has at least 1 function argument and cannot be memoized!'
//       )
//
//     const key = JSON.stringify(args)
//
//     if (!(key in cache)) cache[key] = fn(...args)
//
//     return cache[key]
//   }
//
//   return memoizedFn
// }

export const error = (msg: string) => new Error(msg)

export const EMPTY_STRING = ''

export type ParserResult<T> = [resultOrError: T | Error, rest: string]
export type Parser<T> = (input: string) => ParserResult<T>
export type SingleChar = string
export type EmptyString = typeof EMPTY_STRING

export const isError = <T>(result: T | Error): result is Error => result instanceof Error

export const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<SingleChar> =>
  input =>
    input.length > 0 && matchFn(input[0]) ? [input[0], input.slice(1)] : [error('no match'), input]

export const map =
  <A, B>(parserA: Parser<A>, fn: (value: A) => B): Parser<B> =>
  input => {
    const [result, rest] = parserA(input)

    return isError(result) ? [result, input] : [fn(result), rest]
  }

export const sequence =
  <A, B>(parserA: Parser<A>, fn: (value: A) => Parser<B>): Parser<B> =>
  input => {
    const [result, rest] = parserA(input)

    return isError(result) ? [result, input] : fn(result)(rest)
  }

export const or =
  <A, B>(parserA: Parser<A>, parserB: Parser<B>): Parser<A | B> =>
  input => {
    const [resultA, restA] = parserA(input)

    return isError(resultA) ? parserB(input) : [resultA, restA]
  }
export const or2 = or

export const or3 = <A, B, C>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>
): Parser<A | B | C> => or(parserA, or2(parserB, parserC))

export const or4 = <A, B, C, D>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>
): Parser<A | B | C | D> => or(parserA, or3(parserB, parserC, parserD))

export const or5 = <A, B, C, D, E>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>,
  parserE: Parser<E>
): Parser<A | B | C | D | E> => or(parserA, or4(parserB, parserC, parserD, parserE))

export const orN =
  <T>(parsers: Parser<T>[]): Parser<T> =>
  input => {
    for (const parser of parsers) {
      const [result, rest] = parser(input)

      if (!isError(result)) return [result, rest]
    }

    return [error(`(orN) none of ${parsers.length} parsers satisfied`), input]
  }

export const and = <A, B>(parserA: Parser<A>, parserB: Parser<B>): Parser<[A, B]> =>
  sequence(parserA, resultA => map(parserB, resultB => [resultA, resultB]))
export const and2 = and

export const and3 = <A, B, C>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>
): Parser<[A, B, C]> =>
  map(and(parserA, and2(parserB, parserC)), ([resultA, otherResults]) => [resultA, ...otherResults])

export const and4 = <A, B, C, D>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>
): Parser<[A, B, C, D]> =>
  map(and(parserA, and3(parserB, parserC, parserD)), ([resultA, otherResults]) => [
    resultA,
    ...otherResults,
  ])

export const and5 = <A, B, C, D, E>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>,
  parserE: Parser<E>
): Parser<[A, B, C, D, E]> =>
  map(and(parserA, and4(parserB, parserC, parserD, parserE)), ([resultA, otherResults]) => [
    resultA,
    ...otherResults,
  ])

export const andN =
  <T>(parsers: Parser<T>[]): Parser<T[]> =>
  input => {
    let rest = input
    let result: T[] = []

    for (const parser of parsers) {
      const [tempResult, newRest] = parser(rest)

      if (isError(tempResult)) return [tempResult, input]

      rest = newRest
      result.push(tempResult)
    }

    return [result, rest]
  }

export const all =
  <A>(parsers: Parser<A>[]): Parser<A> =>
  input => {
    let rest = input
    let result!: A | Error

    if (parsers.length === 0) return [error('(all) no parsers specified'), input]

    for (const parser of parsers) {
      ;[result, rest] = parser(input)

      if (isError(result)) return [result, input]
    }

    // Return right-most (last) match.
    return [result, rest]
  }

export const not =
  ({ charsToConsume = 0 } = {}) =>
  (parser: Parser<string>): Parser<string> =>
  input => {
    const [result, _] = parser(input)

    return isError(result)
      ? [input.slice(0, charsToConsume), input.slice(charsToConsume)]
      : [error('(not) Match ok, but should not'), input]
  }
export const not0 = not()
export const not1 = not({ charsToConsume: 1 })
export const not2 = not({ charsToConsume: 2 })

// Might be used as both negative look-behind and negative look-ahead.
export const none =
  ({ charsToConsume = 0 } = {}) =>
  <A>(parsers: Parser<A>[]): Parser<string> =>
  input => {
    if (input.length === 0) return [error(`(none) Empty input`), input]

    for (const parser of parsers) {
      const [result, _] = parser(input)

      if (!isError(result))
        return [error(`(none) One of the ${parsers.length} parsers satisfied`), input]
    }

    return [input.slice(0, charsToConsume), input.slice(charsToConsume)]
  }

export const none0 = none
export const none1 = none({ charsToConsume: 1 })
export const none2 = none({ charsToConsume: 2 })

export type ManyNLimitsType = { min?: number; max?: number }

export const manyN =
  <A>(parser: Parser<A>, { min = 0, max = Infinity }: ManyNLimitsType = {}): Parser<A[]> =>
  input => {
    if (max === 0) return [[], input]

    const [result, rest] = parser(input)

    if (isError(result)) return min > 0 ? [result, input] : [[], input]

    if (rest.length === input.length) {
      // Successful match but no characters consumed. Avoid an infinite loop.
      return [[], input]
    }

    return map(manyN(parser, { min: min - 1, max: max - 1 }), otherResults => [
      result,
      ...otherResults,
    ])(rest)
  }

export const many = manyN
export const many0 = many

export const many1 = <A>(
  parser: Parser<A>,
  { max = Infinity }: ManyNLimitsType = {}
): Parser<A[]> => manyN(parser, { min: 1, max })

export const many2 = <A>(
  parser: Parser<A>,
  { max = Infinity }: ManyNLimitsType = {}
): Parser<A[]> => manyN(parser, { min: 2, max })

export const empty: Parser<EmptyString> = input => [EMPTY_STRING, input]

export const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> => or(parser, empty)
// const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> =>
//   map(many(parser, { maxOccurences: 1 }), results => (results.length === 0 ? EMPTY_STRING : results[0]))

export const concat = (parser: Parser<string[]>): Parser<string> =>
  map(parser, chars => chars.join(EMPTY_STRING))

export const precededBy = <A>(parserBefore: Parser<unknown>, parser: Parser<A>): Parser<A> =>
  map(and(parserBefore, parser), ([_, result]) => result)

export const succeededBy = <A>(parser: Parser<A>, parserAfter: Parser<unknown>): Parser<A> =>
  map(and(parser, parserAfter), ([result, _]) => result)

export const delimitedBy = <A>(
  parserBefore: Parser<unknown>,
  parser: Parser<A>,
  parserAfter: Parser<unknown>
): Parser<A> => precededBy(parserBefore, succeededBy(parser, parserAfter))
// map(and3(parserBefore, parser, parserAfter), ([_, result, __]) => result)

export const surroundedBy = <A>(
  parserBeforeAndAfter: Parser<unknown>,
  parser: Parser<A>
): Parser<A> => delimitedBy(parserBeforeAndAfter, parser, parserBeforeAndAfter)

export const joinedBy = <A>(
  parser: Parser<A>,
  parserInTheMiddle: Parser<unknown>
): Parser<[A, A]> =>
  map(and(succeededBy(parser, parserInTheMiddle), parser), ([result1, result2]) => [
    result1,
    result2,
  ])

export const char = (singleChar: SingleChar): Parser<SingleChar> => satisfy(c => c === singleChar)
export const anyChar = (): Parser<SingleChar> => satisfy(_ => true)
export const allButChar = (singleChar: SingleChar): Parser<SingleChar> => not1(char(singleChar))

export const charSet = (set: string): Parser<SingleChar> => satisfy(c => set.includes(c))
export const allButCharSet = (set: string): Parser<SingleChar> => not1(charSet(set))

export const charSequence =
  (seq: string): Parser<string> =>
  input =>
    input.startsWith(seq) ? [seq, input.slice(seq.length)] : [error('no match'), input]

export const letter = satisfy(char => {
  const upcasedChar = char.toUpperCase()

  return upcasedChar >= 'A' && upcasedChar <= 'Z'
})
export const letters = many1(letter)

// Decimal.
export const digit = map(
  satisfy(char => char >= '0' && char <= '9'),
  digit => +digit
)
export const digits = many1(digit)

export const charRange = (from: SingleChar, to: SingleChar) =>
  satisfy(char => char >= from && char <= to)

export const allButCharRange = (from: SingleChar, to: SingleChar) => not1(charRange(from, to))

export const numberRange =
  ({ from = -Infinity, to = Infinity } = {}): Parser<number> =>
  input => {
    const [result, rest] = numeric(input)

    if (isError(result)) return [result, input]
    if (!(result >= from && result <= to))
      return [error(`Number must be ≥ ${from} and ≤ ${to}, but was ${result}`), input]

    return [result, rest]
  }

export const allButNumberRange =
  ({ from = -Infinity, to = Infinity } = {}): Parser<number> =>
  input => {
    const [result, rest] = numeric(input)

    if (isError(result)) return [result, input]
    if (result >= from && result <= to)
      return [error(`Number must be ≤ ${from} and ≥ ${to}, but was ${result}`), input]

    return [result, rest]
  }

export type HexDigitType =
  | '0'
  | '1'
  | '2'
  | '3'
  | '4'
  | '5'
  | '6'
  | '7'
  | '8'
  | '9'
  | 'A'
  | 'B'
  | 'C'
  | 'D'
  | 'E'
  | 'F'
  | 'a'
  | 'b'
  | 'c'
  | 'd'
  | 'e'
  | 'f'

// Hexadecimal.
export const hexDigit = or3(
  charRange('0', '9'),
  charRange('A', 'F'),
  charRange('a', 'f')
) as Parser<HexDigitType>
export const hexDigits = many1(hexDigit)
export const hexNumber = concat(precededBy(charSequence('0x'), hexDigits))

export type BitType = '0' | '1'

// Binary.
export const ZERO = '0'
export const ONE = '1'
export const zero = char(ZERO)
export const one = char(ONE)
export const bit = or(zero, one) as Parser<BitType>
export const binaryDigits = many1(bit)
export const binaryNumber = concat(precededBy(charSequence('0b'), binaryDigits))

export const SPACE = ' '

export const spaced = <A>(parser: Parser<A>): Parser<A> => surroundedBy(many(char(SPACE)), parser)

export const DOUBLE_QUOTE = '"'
export const SINGLE_QUOTE = "'"
export const BACK_TICK = '`'
export const UNDERSCORE = '_'
export const PLUS_SIGN = '+'
export const MINUS_SIGN = '-'
export const PERIOD = '.'

export const doubleQuote = char(DOUBLE_QUOTE)
export const singleQuote = char(SINGLE_QUOTE)
export const backTick = char(BACK_TICK)
export const underscore = char(UNDERSCORE)
export const plus = char(PLUS_SIGN)
export const minus = char(MINUS_SIGN)
export const period = char(PERIOD)

export const string = concat(
  or3(
    surroundedBy(doubleQuote, many(allButChar(DOUBLE_QUOTE))),
    surroundedBy(singleQuote, many(allButChar(SINGLE_QUOTE))),
    surroundedBy(backTick, many(allButChar(BACK_TICK)))
  )
)

export const wordChar = map(or3(letter, digit, underscore), res => res.toString())
export const word = concat(many1(wordChar))
export const identifier = word

export const sign = or(plus, minus)

export const BASE_2 = 2
export const BASE_10 = 10
export const BASE_16 = 16

export const natural = map(precededBy(optional(plus), digits), digs =>
  digs.reduce((acc, dig, i) => acc + dig * BASE_10 ** (digs.length - (i + 1)), 0)
)

export const integer = map(
  and(optional(sign), natural),
  ([signChar, nat]) => (signChar === MINUS_SIGN ? -1 : 1) * nat
)

export const naturalGreaterThanZero = numberRange({ from: 1 })

export const float = map(
  and(integer, precededBy(period, natural)),
  ([int, nat]) =>
    int + (nat === 0 ? 0 : (Math.sign(int) * nat) / BASE_10 ** Math.trunc(Math.log10(nat) + 1))
)

export const numeric = or(float, integer)

export const ADD = '+'
export const SUBTRACT = '-'
export const MULTIPLY = '*'
export const DIVIDE = '/'
export const RAISE = '^'
export const RAISE_ALT = '**'
export const OPEN_PARENS = '('
export const CLOSE_PARENS = ')'
export const EQUALS = '='

export const add = spaced(charSequence(ADD))
export const subtract = spaced(charSequence(SUBTRACT))
export const multiply = spaced(charSequence(MULTIPLY))
export const divide = spaced(charSequence(DIVIDE))
export const raise = spaced(orN([RAISE, RAISE_ALT].map(charSequence)))
export const openParens = spaced(charSequence(OPEN_PARENS))
export const closeParens = spaced(charSequence(CLOSE_PARENS))
export const equals = spaced(charSequence(EQUALS))

export type OperatorType =
  | typeof ADD
  | typeof SUBTRACT
  | typeof MULTIPLY
  | typeof DIVIDE
  | typeof RAISE
  | typeof RAISE_ALT

export const EQUAL_TO = '=='
export const EQUAL_TO_ALT = '='
export const DIFFERENT_FROM = '≠'
export const DIFFERENT_FROM_ALT1 = '<>'
export const DIFFERENT_FROM_ALT2 = '!='
export const LESS_THAN_OR_EQUAL_TO = '≤'
export const LESS_THAN_OR_EQUAL_TO_ALT = '<='
export const GREATER_THAN_OR_EQUAL_TO = '≥'
export const GREATER_THAN_OR_EQUAL_TO_ALT = '>='
export const LESS_THAN = '<'
export const GREATER_THAN = '>'

export const equalTo = spaced(orN([EQUAL_TO, EQUAL_TO_ALT].map(charSequence)))
export const differentFrom = spaced(
  orN([DIFFERENT_FROM, DIFFERENT_FROM_ALT1, DIFFERENT_FROM_ALT2].map(charSequence))
)
export const lessThanOrEqualTo = spaced(
  orN([LESS_THAN_OR_EQUAL_TO, LESS_THAN_OR_EQUAL_TO_ALT].map(charSequence))
)
export const greaterThanOrEqualTo = spaced(
  orN([GREATER_THAN_OR_EQUAL_TO, GREATER_THAN_OR_EQUAL_TO_ALT].map(charSequence))
)
export const lessThan = spaced(charSequence(LESS_THAN))
export const greaterThan = spaced(charSequence(GREATER_THAN))

export type BooleanOperatorType =
  | typeof EQUAL_TO
  | typeof EQUAL_TO_ALT
  | typeof DIFFERENT_FROM
  | typeof DIFFERENT_FROM_ALT1
  | typeof DIFFERENT_FROM_ALT2
  | typeof LESS_THAN_OR_EQUAL_TO
  | typeof LESS_THAN_OR_EQUAL_TO_ALT
  | typeof GREATER_THAN_OR_EQUAL_TO
  | typeof GREATER_THAN_OR_EQUAL_TO_ALT
  | typeof LESS_THAN
  | typeof GREATER_THAN

export type RangeType = { type: 'range'; from: RefType; to: RefType }

export type FnParameterType = ExpressionType | BooleanExpressionType | RangeType

type BinaryOperationType = {
  type: 'binaryOperation'
  left: ExpressionType
  operator: OperatorType
  right: ExpressionType
}
export type NumericType = { type: 'numeric'; value: number }
type ReferenceType = { type: 'reference'; ref: RefType }
type ParenthesizedExpressionType = { type: 'parenthesizedExpression'; expr: ExpressionType }
type FormulaFnCallType = {
  type: 'formulaFnCall'
  fnName: string
  parameters: FnParameterType[]
}

export type ExpressionType =
  | BinaryOperationType
  | NumericType
  | ReferenceType
  | ParenthesizedExpressionType
  | FormulaFnCallType

export type BooleanExpressionType = {
  type: 'booleanExpression'
  left: ExpressionType
  operator: BooleanOperatorType
  right: ExpressionType
}

export const ref = memoize(
  concat(map(and(letters, naturalGreaterThanZero), ([col, row]) => [...col, row.toString()]))
)

export const NUMBER_1: NumericType = { type: 'numeric', value: 1 }
export const NUMBER_MINUS_1: NumericType = { type: 'numeric', value: -1 }

export const createBinaryOperation = (
  left: ExpressionType,
  operator: OperatorType,
  right: ExpressionType
): BinaryOperationType => ({
  type: 'binaryOperation',
  left,
  operator,
  right,
})

export const mapToBinaryOperation = (
  parser: Parser<[ExpressionType, OperatorType, ExpressionType]>
): Parser<BinaryOperationType> =>
  memoize(map(parser, ([left, operator, right]) => createBinaryOperation(left, operator, right)))

export const additiveTerm: Parser<ExpressionType> = memoize(input => {
  const [result, rest] = or(
    mapToBinaryOperation(
      and3(multiplicativeTerm, or(add, subtract) as Parser<OperatorType>, additiveTerm)
    ),
    multiplicativeTerm
  )(input)

  if (isError(result)) return [result, input]

  // Given that:
  //
  // 1) '+' is both left-associative (i.e. can be evaluated from left to right) and right-associative
  //    (from right to left), so: a + b + c = a + (b + c) = (a + b) + c
  // 2) '-' is only left-associative, so: a - b - c = (a - b) - c ≠ a - (b - c)
  // 3) our parser is right-recursive (so will effectively evaluate from right to left)
  //
  // replace `a - b` by `a + (-b)`.
  if (
    result.type === 'binaryOperation' &&
    result.operator === SUBTRACT &&
    result.right.type === 'binaryOperation'
  ) {
    result.operator = ADD
    result.right.left = createBinaryOperation(NUMBER_MINUS_1, MULTIPLY, result.right.left)
  }

  return [result, rest]
})

export const expression = additiveTerm

export const multiplicativeTerm: Parser<ExpressionType> = memoize(input => {
  const [result, rest] = or(
    mapToBinaryOperation(
      and3(exponentialTerm, or(multiply, divide) as Parser<OperatorType>, multiplicativeTerm)
    ),
    exponentialTerm
  )(input)

  if (isError(result)) return [result, input]

  // Given that:
  //
  // 1) '*' is both left-associative (i.e. can be evaluated from left to right) and right-associative
  //    (from right to left), so: a * b * c = a * (b * c) = (a * b) * c
  // 2) '/' is only left-associative, so: a / b / c = (a / b) / c ≠ a / (b / c)
  // 3) our parser is right-recursive (so will effectively evaluate from right to left)
  //
  // replace `a / b` by `a * (1 / b)`.
  if (
    result.type === 'binaryOperation' &&
    result.operator === DIVIDE &&
    result.right.type === 'binaryOperation'
  ) {
    result.operator = MULTIPLY
    result.right.left = createBinaryOperation(NUMBER_1, DIVIDE, result.right.left)
  }

  return [result, rest]
})

export const exponentialTerm: Parser<ExpressionType> = memoize(input =>
  or(
    mapToBinaryOperation(and3(factor, raise as Parser<OperatorType>, exponentialTerm)),
    factor
  )(input)
)

export const optionallySigned = <A extends ExpressionType>(parser: Parser<A>) =>
  memoize(
    map(and(optional(sign), parser), ([signChar, result]) =>
      signChar === MINUS_SIGN ? createBinaryOperation(NUMBER_MINUS_1, MULTIPLY, result) : result
    )
  )

export const hexDigitToDecimal = memoize((hexDigit: HexDigitType) => {
  hexDigit = hexDigit.toUpperCase() as HexDigitType

  return (
    hexDigit.charCodeAt(0) -
    (hexDigit >= '0' && hexDigit <= '9' ? '0'.charCodeAt(0) : 'A'.charCodeAt(0) - 10)
  )
})

export const binaryToDecimal = memoize((binary: string): number =>
  binary
    .split('')
    .reduce((acc, bit, i) => acc + +(bit as BitType) * BASE_2 ** (binary.length - 1 - i), 0)
)

export const hexToDecimal = memoize((hex: string): number =>
  hex
    .split('')
    .reduce(
      (acc, hexDigit, i) =>
        acc + hexDigitToDecimal(hexDigit as HexDigitType) * BASE_16 ** (hex.length - 1 - i),
      0
    )
)

export const operand: Parser<ExpressionType> = memoize(
  or(
    map(or3(map(binaryNumber, binaryToDecimal), map(hexNumber, hexToDecimal), numeric), value => ({
      type: 'numeric',
      value,
    })),
    optionallySigned(map(ref, ref => ({ type: 'reference', ref })))
  )
)

export const parenthesizedExpression: Parser<ExpressionType> = memoize(
  optionallySigned(
    map(delimitedBy(openParens, expression, closeParens), expr => ({
      type: 'parenthesizedExpression',
      expr,
    }))
  )
)

export const colon = spaced(char(':'))
export const comma = spaced(char(','))

export const range: Parser<RangeType> = memoize(
  map(joinedBy(ref, colon), ([from, to]) => ({
    type: 'range',
    from,
    to,
  }))
)

export const booleanExpression: Parser<BooleanExpressionType> = memoize(
  map(
    and3(
      expression,
      orN([
        equalTo,
        differentFrom,
        lessThanOrEqualTo,
        greaterThanOrEqualTo,
        lessThan,
        greaterThan,
      ]) as Parser<BooleanOperatorType>,
      expression
    ),
    ([left, operator, right]) => ({
      type: 'booleanExpression',
      left,
      operator,
      right,
    })
  )
)

export const fnParameter: Parser<FnParameterType> = memoize(
  or3(range, booleanExpression, expression)
)

export const formulaFnCall: Parser<FormulaFnCallType> = memoize(
  map(
    and(
      identifier,
      delimitedBy(
        openParens,
        optional(and(fnParameter, many(precededBy(comma, fnParameter)))),
        closeParens
      )
    ),
    ([fnName, params]) => ({
      type: 'formulaFnCall',
      fnName,
      parameters: params === EMPTY_STRING ? [] : params.flat(),
    })
  )
)

export const factor: Parser<ExpressionType> = memoize(
  or3(operand, parenthesizedExpression, formulaFnCall)
)

export const formula: Parser<ExpressionType> = precededBy(equals, expression)

declare const Deno: {
  inspect: (...args: unknown[]) => void
}

export const print = (value: object) =>
  console.log(Deno.inspect(value, { depth: 999, colors: true }))
