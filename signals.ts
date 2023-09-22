///////////////
// Constants //
///////////////

let DEBUG = false

////////////////////////
// Helper functions 1 //
////////////////////////

export const log = console.log

export const debug = (messageOrFalse: () => string | false): void => {
  if (DEBUG) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

////////////////
// Exceptions //
////////////////

export class CircularDependencyError extends Error {
  name = 'CircularDependencyError'
}

/////////////////////////////////
// Types, interfaces and enums //
/////////////////////////////////

export enum ComputedSignalKind {
  Lazy = 'Lazy',
  Eager = 'Eager',
}

export interface ISubject {
  observers: Set<IObserver> // a.k.a "consumers", "readers", "subscribers", "listeners" ect
  /** Indicates whether the subject is still in use by an observer or has been removed during a recomputation. */
  inUse: boolean

  addObserver(observer: IObserver, addRelatedSubject?: boolean): void
  removeObserver(observer: IObserver, removeRelatedSubject?: boolean): void
  hasObserver(observer: IObserver): boolean
  notifyObservers(): void
  expireDescendantObservers(): void
  hasDescendantEagerObservers(breadcrumbs?: ObserverPatternType[]): boolean
}

export interface IObserver {
  subjects: Set<ISubject> // a.k.a "producers", "writers", "publishers", "dependencies" etc

  addSubject(subject: ISubject, addRelatedObserver?: boolean): void
  removeSubject(subject: ISubject, removeRelatedObserver?: boolean): void
  hasSubject(subject: ISubject): boolean
  expire(): void
  recalculate(): void // a.k.a. "update"
  recalculateStillExpiredObservers(breadcrumbs?: ComputedSignal<any>[]): void
}

export type ObserverPatternType = IObserver | ISubject

export interface IBaseSignal<T> extends ISubject {
  label: string
  value: T
  version: number

  get(previousValue?: T): T
  processInContext(observer?: IObserver): void
}

export interface IComputedSignal<T> extends IBaseSignal<T>, IObserver {
  valueFn: (previousValue: T) => T
  defaultValue?: T
  initialized: boolean
  expired: boolean
  /** Indicates whether the signal is being recomputed or not (used in the detection of cycles). */
  isRecomputing: boolean
  kind: ComputedSignalKind

  hasLazyBehavior(): boolean
  hasEagerBehavior(): boolean
  set(newValueFn: (previousValue?: T) => T): void
}

export interface IWritableSignal<T> extends IBaseSignal<T> {
  set(newValue: T): void
  update(newImmutableValueFn: (previousValue: T) => T): void
  mutate(newMutableValueFn: (previousValue: T) => void): void
}

export interface IComputedSignalWrapper<T> {
  (): T
  signal: IComputedSignal<T>
  set(newValueFn: (previousValue?: T) => T): void
}

export interface IWritableSignalWrapper<T> {
  (): T
  signal: IWritableSignal<T>
  set(newValue: T): void
  update(newImmutableValueFn: (previousValue: T) => T): void
  mutate(newMutableValueFn: (previousValue: T) => void): void
}

export type SignalWrapperType<T> = IComputedSignalWrapper<T> | IWritableSignalWrapper<T>

export interface IComputedSignalOptions<T> {
  kind?: ComputedSignalKind
  defaultValue?: T
}

/////////////
// Classes //
/////////////

class ExecutionContext<T> {
  private readonly stack: T[] = []

  run(value: T, fn: () => void): void {
    this.push(value)
    fn()
    this.pop()
  }

  push(value: T) {
    this.stack.push(value)
  }
  pop() {
    return this.stack.pop()
  }
  peek() {
    return this.stack.at(-1)
  }
  isEmpty() {
    return this.stack.length === 0
  }
  reset() {
    this.stack.length = 0
  }
}

export abstract class BaseSignal<T> implements IBaseSignal<T> {
  value!: T
  version: number = 0
  inUse = true
  readonly observers: Set<IObserver> = new Set()

  static readonly executionContext = new ExecutionContext<IObserver>()

  constructor(public readonly label: string) {}

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  get() {
    const observer = BaseSignal.executionContext.peek()

    this.processInContext(observer)

    debug(
      () =>
        `Reading value ${this.value} of signal ${this.label} from signal ${BaseSignal.label(
          observer
        )}`
    )

    if (observer !== undefined) {
      if (!observer.hasSubject(this)) observer.addSubject(this)

      this.inUse = true
    }

    return this.value
  }

  abstract processInContext(_observer?: IObserver): void

  /////////////////////////////
  // ISubject implementation //
  /////////////////////////////

  addObserver(observer: IObserver, addRelatedSubject: boolean = true): void {
    debug(() => `Adding observer ${BaseSignal.label(observer)} to ${this.label}`)

    this.observers.add(observer)

    if (addRelatedSubject) observer.addSubject(this, false)
  }

  removeObserver(observer: IObserver, removeRelatedSubject: boolean = true): void {
    debug(() => `Removing observer ${BaseSignal.label(observer)} from ${this.label}`)

    if (!this.observers.delete(observer))
      throw new Error(
        `Error when removing observer ${BaseSignal.label(observer)} from ${this.label}`
      )

    if (removeRelatedSubject) observer.removeSubject(this, false)
  }

  hasObserver(observer: IObserver): boolean {
    return this.observers.has(observer)
  }

  notifyObservers(): void {
    debug(() => `Notifying ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`)

    this.observers.forEach(observer => observer.recalculate())
  }

  expireDescendantObservers(): void {
    debug(
      () =>
        this.observers.size > 0 &&
        `Expiring ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`
    )

    this.observers.forEach(observer => observer.expire())
  }

  hasDescendantEagerObservers(breadcrumbs: ObserverPatternType[] = []): boolean {
    debug(
      () =>
        breadcrumbs.length > 0 &&
        `hasDescendantEagerObservers()'s breadcrumbs for ${this.label}: ${BaseSignal.labels(
          breadcrumbs
        )}`
    )

    // Signal already visited/checked?
    if (breadcrumbs.indexOf(this) !== -1) {
      debug(() => `Skipping already visited signal/observer ${this.label}`)

      return false
    }

    const observersArray = [...this.observers]

    // Check if there is any direct eager observer or, if none found, if there is any direct lazy observer
    // that, acting as a subject, has itself a direct or indirect eager observer.
    const found =
      observersArray.some(
        observer => (observer as ComputedSignal<unknown>).kind === ComputedSignalKind.Eager
      ) ||
      observersArray.some(observer =>
        (observer as unknown as ISubject).hasDescendantEagerObservers(breadcrumbs)
      )

    if (!found) breadcrumbs.push(this) // Notice our breadcrumbs set contains only negative/false matches.

    return found
  }

  ///////////////////
  // Other methods //
  ///////////////////

  static label(
    signal: ObserverPatternType | undefined,
    defaultLabel = '(non-reactive context) main'
  ) {
    return (signal as IBaseSignal<unknown>)?.label ?? defaultLabel
  }

  static labels(
    signals: Set<ObserverPatternType> | ObserverPatternType[],
    separator = ', '
  ): string {
    return (
      '[' +
      [...signals].map(signal => this.label(signal as IBaseSignal<unknown>)).join(separator) +
      ']'
    )
  }
}

export class ComputedSignal<T> extends BaseSignal<T> implements IComputedSignal<T> {
  expired: boolean = true
  isRecomputing: boolean = false
  initialized: boolean = false
  readonly kind: ComputedSignalKind
  readonly defaultValue?: T
  readonly subjects: Set<ISubject> = new Set()

  constructor(
    label: string,
    public valueFn: (previousValue?: T) => T,
    options: IComputedSignalOptions<T> = {}
  ) {
    super(label)

    this.kind = options.kind ?? ComputedSignalKind.Lazy
    this.defaultValue = options.defaultValue

    if (this.kind === ComputedSignalKind.Eager) this.get()
  }

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  processInContext(observer?: IObserver): void {
    let previousSubjectsInUseValues: Map<ISubject, boolean> = new Map()

    if (!this.expired) return

    if (this.isRecomputing) {
      BaseSignal.executionContext.reset()

      throw new CircularDependencyError(
        `Circular dependency detected when reading signal ${
          this.label
        } from signal ${BaseSignal.label(observer as ComputedSignal<unknown>)}`
      )
    }

    this.isRecomputing = true

    // Save the in-use values of all current (but soon to become "previous") subjects.
    this.subjects.forEach(subject => previousSubjectsInUseValues.set(subject, subject.inUse))

    // Mark all subjects as invalid before recomputing the signal.
    this.subjects.forEach(subject => {
      subject.inUse = false
    })

    // Recompute it.
    BaseSignal.executionContext.run(this, () => {
      debug(() => `(Re)computing ${this.label}`)

      this.value = this.valueFn(this.initialized ? this.value : this.defaultValue)

      this.version++
      this.initialized = true
    })

    // Remove all (still) invalid subjects.
    ;[...this.subjects]
      .filter(subject => !subject.inUse)
      .forEach(subject => this.removeSubject(subject))

    // Restore the in-use values of *all* previous subjects, including removed ones.
    previousSubjectsInUseValues.forEach((inUse, subject) => {
      subject.inUse = inUse
    })

    this.expired = false
    this.isRecomputing = false
  }

  //////////////////////////////
  // IObserver implementation //
  //////////////////////////////

  addSubject(subject: ISubject, addRelatedObserver: boolean = true): void {
    debug(() => `Adding subject ${BaseSignal.label(subject)} to ${this.label}`)

    this.subjects.add(subject)

    if (addRelatedObserver) subject.addObserver(this, false)
  }

  removeSubject(subject: ISubject, removeRelatedObserver: boolean = true): void {
    debug(() => `Removing subject ${BaseSignal.label(subject)} from ${this.label}`)

    if (!this.subjects.delete(subject))
      throw new Error(`Error when removing subject ${BaseSignal.label(subject)} from ${this.label}`)

    if (removeRelatedObserver) subject.removeObserver(this, false)
  }

  hasSubject(subject: ISubject): boolean {
    return this.subjects.has(subject)
  }

  expire(): void {
    if (this.expired) return

    this.expired = true
    this.expireDescendantObservers()
  }

  recalculate(): void {
    if (!this.expired) return

    // Nothing to do if observer has effective lazy behavior (i.e. is lazy and has no descendant eager observers).
    if (this.hasLazyBehavior()) {
      debug(
        () =>
          `Skipping recalculation of observer ${this.label}, with effective lazy behavior (i.e. is lazy and has no descendant eager observers)`
      )

      return
    }

    const previousValue: T = this.value

    this.get()

    // If value has changed, notify its own observers to recursively update (recompute/expire).
    if (this.value !== previousValue) this.notifyObservers()
  }

  recalculateStillExpiredObservers(breadcrumbs: ComputedSignal<any>[] = []): void {
    debug(
      () =>
        this.observers.size > 0 &&
        `Recalculating ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`
    )

    // Signal already visited/checked?
    if (breadcrumbs.indexOf(this) !== -1) return

    breadcrumbs.push(this)

    this.observers.forEach(observer => {
      observer.recalculate()
      observer.recalculateStillExpiredObservers(breadcrumbs)
    })
  }

  ////////////////////////////////////
  // IComputedSignal implementation //
  ////////////////////////////////////

  hasLazyBehavior(): boolean {
    return this.kind === ComputedSignalKind.Lazy && !this.hasDescendantEagerObservers()
  }

  hasEagerBehavior(): boolean {
    // Equivalent to: `return this.type === ObserverTypeEnum.Eager || this.hasDescendantEagerObservers(`.
    return !this.hasLazyBehavior()
  }

  set(newValueFn: (previousValue?: T) => T) {
    this.valueFn = newValueFn
    this.expire()
    this.recalculate()
    this.recalculateStillExpiredObservers() // Usually unfired effects...
  }
}

export class WritableSignal<T> extends BaseSignal<T> {
  constructor(label: string, public value: T) {
    super(label)

    this.version++
  }

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  // There is nothing to process in context for a WritableSignal.
  processInContext(_observer?: IComputedSignal<unknown> | undefined): void {}

  ////////////////////////////////////
  // IWritableSignal implementation //
  ////////////////////////////////////

  set(newValue: T): void {
    this.change(newValue)
  }

  update(newImmutableValueFn: (previousValue: T) => T): void {
    this.change(newImmutableValueFn)
  }

  mutate(newMutableValueFn: (previousValue: T) => void): void {
    newMutableValueFn(this.value)

    this.change(this.value, true)
  }

  ///////////////////
  // Other methods //
  ///////////////////

  private change(newValueOrFn: T | ((previousValue: T) => T), skipEqualityCheck = false): void {
    const newValue = newValueOrFn instanceof Function ? newValueOrFn(this.value) : newValueOrFn

    if (skipEqualityCheck || newValue !== this.value) {
      if (skipEqualityCheck)
        debug(
          () =>
            `Replacing the (already mutated) value of ${this.label}: ${JSON.stringify(this.value)}`
        )
      else
        debug(
          () =>
            `Replacing the immutable value of ${this.label}: ${JSON.stringify(
              this.value
            )} -> ${JSON.stringify(newValue)}`
        )

      this.value = newValue
      this.version++

      this.expireDescendantObservers() // Notice this expires all direct and indirect observers at once.
      this.notifyObservers()
    }
  }
}

///////////////////////////////
// Factory/wrapper functions //
///////////////////////////////

export const signal = function <T>(label: string, initialValue: T): IWritableSignalWrapper<T> {
  const signal = new WritableSignal(label, initialValue)

  // Wrap the getter in the function call itself.
  const wrapper: IWritableSignalWrapper<T> = () => signal.get()

  // Simply delegate the set(), update() and mutate() methods to the signal.
  wrapper.set = (newValue: T) => signal.set(newValue)
  wrapper.update = (newImmutableValueFn: (previousValue: T) => T) =>
    signal.update(newImmutableValueFn)
  wrapper.mutate = (newMutableValueFn: (previousValue: T) => void) =>
    signal.mutate(newMutableValueFn)

  // Allow access to the underlying signal.
  wrapper.signal = signal

  return wrapper
}

export const computed = function <T>(
  label: string,
  valueFn: (previousValue?: T) => T,
  options: IComputedSignalOptions<T> = {}
): IComputedSignalWrapper<T> {
  const signal = new ComputedSignal(label, valueFn, options)

  // Wrap the getter in the function call itself.
  const wrapper: IComputedSignalWrapper<T> = () => signal.get()

  // Simply delegate the set() method to the signal.
  wrapper.set = (newValueFn: (previousValue?: T) => T) => signal.set(newValueFn)

  // Allow access to the underlying signal.
  wrapper.signal = signal

  return wrapper
}

export const effect = <T>(
  label: string,
  fn: (previousValue?: T) => T,
  defaultValue?: T
): IComputedSignal<T> =>
  computed(label, fn, { kind: ComputedSignalKind.Eager, defaultValue }).signal

// https://tsplay.dev/Nr37Vm
export const isComputedSignalWrapper = <T>(
  wrapper: SignalWrapperType<T>
): wrapper is IComputedSignalWrapper<T> => wrapper.signal instanceof ComputedSignal
export const isWritableSignalWrapper = <T>(
  wrapper: SignalWrapperType<T>
): wrapper is IWritableSignalWrapper<T> => wrapper.signal instanceof WritableSignal

////////////////////////
// Helper functions 2 //
////////////////////////

export function signalReplacerFn<T>(key: string, value: any) {
  if (key === 'observers' || key === 'subjects') {
    return [...value].map((s: IBaseSignal<T>) => s.label)
  } else if (value instanceof Function) {
    return '(function)'
  } else if (value === undefined) {
    return '(undefined)'
  } else {
    return value // return as is
  }
}

export const inspectSignal = <T>(signalOrWrapper: BaseSignal<T> | SignalWrapperType<T>) => {
  log(
    JSON.stringify(
      signalOrWrapper instanceof BaseSignal ? signalOrWrapper : signalOrWrapper.signal,
      signalReplacerFn<T>,
      2
    )
  )
}

export const subjects = (computedSignalWrapper: IComputedSignalWrapper<any>) =>
  [...computedSignalWrapper.signal.subjects].map(subject => BaseSignal.label(subject))

export const observers = (signalWrapper: SignalWrapperType<any>) =>
  [...signalWrapper.signal.observers].map(observer => BaseSignal.label(observer))

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)
