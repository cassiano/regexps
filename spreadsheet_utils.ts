import {
  ADD,
  DIVIDE,
  RAISE,
  RAISE_ALT,
  ExpressionType,
  MULTIPLY,
  RangeType,
  SUBTRACT,
  formula,
  isError,
  BooleanExpressionType,
  EQUAL_TO,
  EQUAL_TO_ALT,
  DIFFERENT_FROM,
  DIFFERENT_FROM_ALT1,
  DIFFERENT_FROM_ALT2,
  LESS_THAN_OR_EQUAL_TO,
  LESS_THAN_OR_EQUAL_TO_ALT,
  GREATER_THAN_OR_EQUAL_TO,
  GREATER_THAN_OR_EQUAL_TO_ALT,
  LESS_THAN,
  GREATER_THAN,
  FnParameterType,
} from './parser_combinators.ts'
import {
  ComputedSignalKind,
  IComputedSignalWrapper,
  computed,
  times,
  signalReplacerFn,
  debug,
} from './signals.ts'

const ALPHABET_LENGTH = 'Z'.charCodeAt(0) - 'A'.charCodeAt(0) + 1

export interface CoordsInterface {
  row: number
  col: number
  labelCol?: string
}

export type LetterType = string
export type ColRefType = string
export type RowColType = [number, number]
export type RefType = string
export type RefOrRowColType = RefType | RowColType
export type CellValueType = string | number | undefined

export type SheetCellType = string | number
export type SheetDataType = {
  [ref: RefType]: SheetCellType
}
export type FormulaType = {
  rawValue: string
  parsedValue?: ExpressionType
}
export type CellType = {
  formula?: FormulaType
  signalWrapper: IComputedSignalWrapper<number>
}
export type SheetType = {
  cells: {
    [ref: RefType]: CellType
  }
  rows: number
  cols: number
}

export const sequence = (count: number) => times(count, i => i)

export const sequenceReduce = <T>(number: number, fn: (acc: T, item: number) => T, initialAcc: T) =>
  sequence(number).reduce(fn, initialAcc)

// Converts 'A' to 1, 'B' to 2... 'Z' to 26.
export const colIndexFromSingleLetter = (colSingleRef: LetterType): number => {
  return colSingleRef.charCodeAt(0) - 'A'.charCodeAt(0) + 1
}

// Converts 'A' to 1, 'B' to 2... 'Z' to 26, 'AA' to 27 etc
export const colIndexFromLabel = (colRef: ColRefType): number => {
  return colRef
    .split('')
    .reduce(
      (acc, letter, i) =>
        acc + colIndexFromSingleLetter(letter) * ALPHABET_LENGTH ** (colRef.length - i - 1),
      0
    )
}

// Converts 1 to 'A', 2 to 'B'... 26 to 'Z'.
export const colSingleLetter = (colIndex: number): LetterType => {
  return String.fromCharCode(colIndex - 1 + 'A'.charCodeAt(0))
}

// Converts 1 to 'A', 2 to 'B'... 26 to 'Z', 27 to 'AA' etc
export const colAsLabel = (colIndexOrLabel: number | ColRefType): ColRefType => {
  if (typeof colIndexOrLabel === 'string') return colIndexOrLabel

  let colIndex = colIndexOrLabel - 1
  let colRef = ''

  while (colIndex >= 0) {
    colRef = colSingleLetter((colIndex % ALPHABET_LENGTH) + 1) + colRef

    colIndex = Math.trunc(colIndex / ALPHABET_LENGTH) - 1
  }

  return colRef
}

export const rowColFromRef = (ref: RefType): CoordsInterface => {
  const match = ref.toUpperCase().match(/^([A-Z]+)(\d+)$/i)
  const col = colIndexFromLabel(match![1])
  const row = Number(match![2])

  return { row, col }
}

export const asCoords = (refOrCoords: RefOrRowColType): CoordsInterface => {
  let row: number, col: number

  if (refOrCoords instanceof Array) [row, col] = refOrCoords
  else ({ row, col } = rowColFromRef(refOrCoords))

  if (typeof col === 'string') col = colIndexFromLabel(col)

  return { row, col, labelCol: colAsLabel(col) }
}

export const asRef = (refOrCoords: RefOrRowColType): RefType => {
  const { row, col } = asCoords(refOrCoords)

  return colAsLabel(col) + String(row)
}

export const range = (from: number, to: number) => times(to - from + 1, i => i + from)

export const expandRange = (from: RefType, to: RefType) => {
  const fromCoords = asCoords(from)
  const toCoords = asCoords(to)

  return range(fromCoords.row, toCoords.row).map(row =>
    range(fromCoords.col, toCoords.col).map(col => asRef([row, col]))
  )
}

export const findOrCreateAndEvaluateCell = (sheet: SheetType, ref: RefType) => {
  if (!(ref in sheet.cells)) addCell(sheet, ref, () => 0)

  return sheet.cells[ref].signalWrapper()
}

const FORMULA_FUNCTIONS = {
  sum: function (args: (number | number[][])[]) {
    return args.flat(2).reduce((acc, ref) => acc + ref, 0)
  },
  count: function (args: (number | number[][])[]) {
    return args.flat(2).length
  },
  avg: function (args: (number | number[][])[]) {
    return this.sum(args) / this.count(args)
  },
  max: function (args: (number | number[][])[]) {
    return Math.max(...args.flat(2))
  },
  min: function (args: (number | number[][])[]) {
    return Math.min(...args.flat(2))
  },
  rows: function (args: (number | number[][])[]) {
    if (!(args.length === 1 && Array.isArray(args[0]) && Array.isArray(args[0][0])))
      throw new Error(`Invalid arguments passed to function 'rows'`)

    return args[0].length
  },
  cols: function (args: (number | number[][])[]) {
    if (!(args.length === 1 && Array.isArray(args[0]) && Array.isArray(args[0][0])))
      throw new Error(`Invalid arguments passed to function 'cols'`)

    return args[0][0].length
  },
  if: function (args: (number | number[][])[]) {
    // Does not trigger TS's type narrowing:
    // if (!(args.length === 3 && args.every(arg => typeof arg === 'number')))
    if (
      !(
        args.length === 3 &&
        typeof args[0] === 'number' &&
        typeof args[1] === 'number' &&
        typeof args[2] === 'number'
      )
    )
      throw new Error(`Invalid arguments passed to function 'if'`)

    return args[0] !== 0 ? args[1] : args[2]
  },
}

const evaluateRange = (sheet: SheetType, range: RangeType) =>
  expandRange(range.from, range.to).map(row =>
    row.map(ref => findOrCreateAndEvaluateCell(sheet, ref.toUpperCase()))
  )

const evaluateBooleanExpression = (
  sheet: SheetType,
  booleanExpr: BooleanExpressionType
): number => {
  const booleanToNumber = (value: boolean) => (value === true ? 1 : 0)

  const left = evaluateExpression(sheet, booleanExpr.left)
  const right = evaluateExpression(sheet, booleanExpr.right)

  switch (booleanExpr.operator) {
    case EQUAL_TO:
    case EQUAL_TO_ALT:
      return booleanToNumber(left === right)
    case DIFFERENT_FROM:
    case DIFFERENT_FROM_ALT1:
    case DIFFERENT_FROM_ALT2:
      return booleanToNumber(left !== right)
    case LESS_THAN:
      return booleanToNumber(left < right)
    case LESS_THAN_OR_EQUAL_TO:
    case LESS_THAN_OR_EQUAL_TO_ALT:
      return booleanToNumber(left <= right)
    case GREATER_THAN:
      return booleanToNumber(left > right)
    case GREATER_THAN_OR_EQUAL_TO:
    case GREATER_THAN_OR_EQUAL_TO_ALT:
      return booleanToNumber(left >= right)
    default:
      throw new Error(`Invalid operator ${booleanExpr.operator}`)
  }
}

const evaluateParam = (sheet: SheetType, param: FnParameterType) => {
  switch (param.type) {
    case 'range':
      return evaluateRange(sheet, param)
    case 'booleanExpression':
      return evaluateBooleanExpression(sheet, param)
    default:
      return evaluateExpression(sheet, param)
  }
}

const evaluateExpression = (sheet: SheetType, expr: ExpressionType): number => {
  switch (expr.type) {
    case 'numeric':
      return expr.value

    case 'reference':
      return findOrCreateAndEvaluateCell(sheet, expr.ref.toUpperCase())

    case 'parenthesizedExpression':
      return evaluateExpression(sheet, expr.expr)

    case 'binaryOperation':
      const left = evaluateExpression(sheet, expr.left)
      const right = evaluateExpression(sheet, expr.right)

      switch (expr.operator) {
        case ADD:
          return left + right
        case SUBTRACT:
          return left - right
        case MULTIPLY:
          return left * right
        case DIVIDE:
          return left / right
        case RAISE:
        case RAISE_ALT:
          return left ** right
        default: {
          const _exhaustiveCheck1: never = expr.operator
          throw new Error(`[${_exhaustiveCheck1}] Invalid operator ${expr.operator}`)
        }
      }

    case 'formulaFnCall':
      const fnName = expr.fnName.toLocaleLowerCase()
      const parameters = expr.parameters.map(param => evaluateParam(sheet, param))

      if (fnName in FORMULA_FUNCTIONS) {
        // TODO: Review code below.
        return FORMULA_FUNCTIONS[fnName as keyof typeof FORMULA_FUNCTIONS](parameters)
      } else {
        throw new Error(`Invalid formula function '${fnName}' called.`)
      }

    default: {
      const _exhaustiveCheck2: never = expr
      throw new Error(`[${_exhaustiveCheck2}] Invalid expression type`)
    }
  }
}

export const evaluateFormula = (sheet: SheetType, value: string, ref: RefType): number => {
  let expression: ExpressionType | Error
  const cell: CellType = sheet.cells[ref]
  const useCachedVersion = cell.formula?.parsedValue !== undefined

  if (useCachedVersion) {
    debug(() => `Using cached formula for ${ref} cell...`)

    expression = cell.formula!.parsedValue!
  } else {
    debug(() => `Calculating ${ref} cell formula...`)

    const match = formula(value.trim())
    expression = match[0]

    if (isError(expression) || match[1] !== '') throw `Invalid formula '${value}' for cell ${ref}`

    cell.formula!.parsedValue = expression
  }

  return evaluateExpression(sheet, expression)
}

const upsertCell = (sheet: SheetType, ref: RefType, fn: () => number, formula?: string) => {
  if (ref in sheet.cells) {
    sheet.cells[ref].formula = formula !== undefined ? { rawValue: formula } : undefined

    sheet.cells[ref].signalWrapper.set(fn)
  } else {
    addCell(sheet, ref, fn, formula)
  }
}

export const loadSheet = (sheetData: SheetDataType) => {
  const sheet: SheetType = { cells: {}, rows: 0, cols: 0 }

  Object.entries(sheetData).forEach(([ref, value]) => {
    ref = ref.toUpperCase()

    if (typeof value === 'number') {
      const fn = () => value

      upsertCell(sheet, ref, fn)
    } else {
      const trimmeValue = value.trim()

      if (trimmeValue[0] === '=') {
        const fn = () => evaluateFormula(sheet, trimmeValue, ref)

        upsertCell(sheet, ref, fn, trimmeValue)
      } else throw new Error(`Invalid formula: '${trimmeValue}' must start with '='`)
    }
  })

  return sheet
}

export const addCell = (sheet: SheetType, ref: RefType, fn: () => number, formula?: string) => {
  sheet.cells[ref] = {
    formula: formula !== undefined ? { rawValue: formula } : undefined,
    signalWrapper: computed(ref, () => 0), // Temporary signal. It will be replaced below, after the cell is created.
  }

  sheet.cells[ref].signalWrapper = computed(ref, fn, { kind: ComputedSignalKind.Eager })

  const { row, col } = asCoords(ref)

  sheet.rows = Math.max(sheet.rows, row)
  sheet.cols = Math.max(sheet.cols, col)
}

export const updateCellFormula = (sheet: SheetType, ref: RefType, newFormula: string) => {
  ref = ref.toUpperCase()

  const sheetRef = sheet.cells[ref]

  sheetRef.formula = { rawValue: newFormula }
  sheetRef.signalWrapper.set(() => evaluateFormula(sheet, newFormula, ref))
}

export const updateCellValue = (sheet: SheetType, ref: RefType, newValue: number) => {
  ref = ref.toUpperCase()

  const sheetRef = sheet.cells[ref]

  sheetRef.formula = undefined
  sheetRef.signalWrapper.set(() => newValue)
}

export const sheetAsJson = (sheet: SheetType) =>
  JSON.stringify(
    Object.entries(sheet.cells).map(([key, value]) => ({
      ref: key,
      formula: value.formula,
      value: value.signalWrapper(),
      signal: value.signalWrapper.signal,
    })),
    signalReplacerFn,
    2
  )

export const truncate = (text: string, limit: number, filler = ' ') => {
  const paddedText = text.trim().padEnd(limit, filler)

  return paddedText.length <= limit ? paddedText : paddedText.slice(0, limit - 1) + '…'
}

export const sheetAsTable = (sheet: SheetType, padding = 64) => {
  const colLabels = ['', ...range(1, sheet.cols).map(col => '[' + colAsLabel(col) + ']')]
  const results = [colLabels]

  range(1, sheet.rows).forEach(row => {
    const rowLabel = '[' + row.toString() + ']'
    const rowResult = [rowLabel]

    range(1, sheet.cols).forEach(col => {
      const colLabel = colAsLabel(col)
      const ref = [colLabel, row].join('')

      rowResult.push(
        ref in sheet.cells
          ? sheet.cells[ref].formula !== undefined
            ? sheet.cells[ref].signalWrapper().toString() + ` ${sheet.cells[ref].formula}`
            : sheet.cells[ref].signalWrapper().toString()
          : ''
      )
    })

    rowResult.push(rowLabel)
    results.push(rowResult)
  })

  results.push(colLabels)

  return (
    '\n' +
    results
      .map(row =>
        row
          .map((col, i) => truncate(col.toString(), i === 0 || i === row.length - 1 ? 4 : padding))
          .join(' | ')
      )
      .join('\n') +
    '\n'
  )
}

const generateCellColsRows = (
  side: number,
  initialRef: RefType,
  initialValue: CellValueType,
  baseRow: number,
  baseCol: number
) => {
  const { row, col } = asCoords(initialRef)
  const value =
    row === baseRow || col === baseCol ? initialValue : `=${colAsLabel(col - 1)}${row - 1}+1`

  const verticalResult = sequenceReduce(
    side - 1,
    (acc, i) => {
      const key = `${colAsLabel(col)}${i + 1 + row}`
      const value = `=${colAsLabel(col)}${i + row}+1`

      acc[key] = value

      return acc
    },
    { [initialRef]: value }
  )

  const horizontalResult = sequenceReduce(
    side - 1,
    (acc, i) => {
      const key = `${colAsLabel(i + col + 1)}${row}`
      const value = `=${colAsLabel(i + col)}${row}+1`

      acc[key] = value

      return acc
    },
    { [initialRef]: value }
  )

  return { ...(verticalResult as object), ...(horizontalResult as object) }
}

export const generateCellSquares = (
  side: number,
  initialRef: RefType,
  initialValue: CellValueType
): SheetDataType => {
  const { row, col } = asCoords(initialRef)

  return sequenceReduce(
    side,
    (acc: object, i: number): object =>
      Object.assign(
        acc,
        generateCellColsRows(side - i, asRef([i + row, i + col]), initialValue, row, col)
      ),
    {}
  )
}

type TurnDirection = 'left' | 'right'
type CardinalDirection = 'north' | 'south' | 'east' | 'west'

type CartesianDirectionSubType1 = Record<TurnDirection, CardinalDirection>

interface CartesianDirectionSubType2 {
  walk: (ref: RefType, step?: number) => RefType
}

type CartesianDirection = CartesianDirectionSubType1 & CartesianDirectionSubType2

const CARTESIAN_DIRECTIONS: Record<CardinalDirection, CartesianDirection> = {
  north: {
    right: 'east',
    left: 'west',
    walk: (ref, step = 1) => {
      const { row, col } = asCoords(ref)
      return asRef([row > step ? row - step : row, col])
    },
  },
  south: {
    right: 'west',
    left: 'east',
    walk: (ref, step = 1) => {
      const { row, col } = asCoords(ref)
      return asRef([row + step, col])
    },
  },
  east: {
    right: 'south',
    left: 'north',
    walk: (ref, step = 1) => {
      const { row, col } = asCoords(ref)
      return asRef([row, col + step])
    },
  },
  west: {
    right: 'north',
    left: 'south',
    walk: (ref, step = 1) => {
      const { row, col } = asCoords(ref)
      return asRef([row, col > step ? col - step : col])
    },
  },
}

export function generateSpiralSequence(
  firstSegmentSize: number,
  initialDirection: CardinalDirection,
  directionToTurn: TurnDirection,
  initialCells: Record<RefType, CellValueType>[],
  fn: (i: number, previousRefs: RefType[], nextRef: RefType) => CellValueType
): SheetDataType {
  let currentRef = Object.keys(initialCells[initialCells.length - 1])[0]

  const previousRefs: RefType[] = initialCells
    .slice(0, initialCells.length - 1)
    .flatMap(Object.keys)
  let direction = initialDirection
  let stepsToWalkInDirection = firstSegmentSize
  let stepsWalkedInDirection = initialCells.length

  const count =
    sequenceReduce(firstSegmentSize - 1, (acc, i) => acc + (i + 1), 0) - initialCells.length + 2

  return sequenceReduce(
    count - 1,
    (acc, i) => {
      previousRefs.push(currentRef)
      currentRef = CARTESIAN_DIRECTIONS[direction].walk(currentRef)

      stepsWalkedInDirection++

      if (stepsWalkedInDirection >= stepsToWalkInDirection) {
        direction = CARTESIAN_DIRECTIONS[direction][directionToTurn]

        stepsToWalkInDirection -= 1
        stepsWalkedInDirection = 1
      }

      acc[currentRef] = fn(i, previousRefs, CARTESIAN_DIRECTIONS[direction].walk(currentRef))

      return acc
    },
    initialCells.reduce((acc, item) => Object.assign(acc, item), {})
  ) as SheetDataType
}

export const repeat = (count: number, fn: (i: number) => string) => times(count, fn).join('')

export const deleteKeys = (object: { [key: string]: unknown }) => {
  Object.keys(object).forEach(key => delete object[key])
}

export const inChunksOf = <T>(collection: T[], size: number): T[][] => {
  const results: T[][] = []

  for (let i = 0; i < collection.length; i += size) results.push(collection.slice(i, i + size))

  return results
}

// const p = (value: string) => console.log(Deno.inspect(value, { depth: 999, colors: true }))
