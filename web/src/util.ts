import { find, forEach, map } from 'ramda'

export type {
  Case,
  Left,
  Right,
  Either,
  Maybe
}

export {
  caseOf,
  caseOf_,
  caseOfDefault,
  caseOfDefaultB,
  nil,
  singleton,
  isLeft,
  isRight,
  either,
  lefts,
  rights,
  cons,
  just,
  omit,
}


type Case<A, B> = {
  k: A
  v: B
}

function caseOf<A, B>(expr: A, ...cases: Case<A,B>[]): B | undefined {
  return find(c => c.k === expr, cases)?.v
}

// use only when case clauses encompasses the whole set A
function caseOf_<A, B>(expr: A, ...cases: Case<A,B>[]): B {
  return find(c => c.k === expr, cases)!.v
}

function caseOfDefault<A, B>(expr: A, def: B, ...cases: Case<A,B>[]){
 const x = caseOf<A,B>(expr, ...cases)
 return x ? x : def  
}

function caseOfDefaultB<B>(def: B, ...cases: Case<Boolean, B>[]): B {
  return caseOfDefault(true, def, ...cases)
}

const nil: Id = "00000000-0000-0000-0000-000000000000"


function singleton<A>(x: A){ return [x]}

type Left<A> = {
  Left: A;
  Right?: never;
};

type Right<B> = {
  Left?: never;
  Right: B;
};

type Either<A, B> = NonNullable<Left<A> | Right<B>>;

function isLeft<A, B>({ Left }: Either<A, B>) {
  return Left ? true : false
}

function isRight<A, B>({ Right }: Either<A, B>) {
  return Right ? true : false 
}

function either<A, B, C>(f: (_: A) => C, g: (_: B) => C, x: Either<A, B>): C {
  return isLeft(x) ? f(x.Left!) : g(x.Right!)
}

function lefts<A, B>(xs: Either<A, B>[]): A[]{
  return map(x => either(singleton, _ => [], x), xs).flat()
}

function rights<A, B>(xs: Either<A, B>[]): B[]{
  return map(x => either(_ => [], singleton, x), xs).flat()
}

function cons<A>(x : A, xs: A[]){
  return [x, ...xs]
}

type Maybe<A> = NonNullable<A> | undefined;

function just<A>(x : A): Maybe<A>{
  return x ?? undefined
}
function omit<A extends object, B extends keyof A>(obj: A, ...entries : B[]): A {
  const o = structuredClone(obj)
  forEach(entry => { delete o[entry]  }, entries)
  return o
}


