import { find } from 'ramda'
import { Id } from '../types/DB'

type Case<A, B> = {
  k: A
  v: B
}

export function caseOf<A, B>(expr: A, ...cases: Case<A,B>[]): B | undefined {
  return find(c => c.k === expr, cases)?.v
}

// use only when case clauses encompasses the whole set A
export function caseOf_<A, B>(expr: A, ...cases: Case<A,B>[]): B {
  return find(c => c.k === expr, cases)!.v
}

export function caseOfDefault<A, B>(expr: A, def: B, ...cases: Case<A,B>[]){
 const x = caseOf<A,B>(expr, ...cases)
 return x ? x : def  
}

export function caseOfDefaultB<B>(def: B, ...cases: Case<Boolean, B>[]): B {
  return caseOfDefault(true, def, ...cases)
}

export const nil: Id = "00000000-0000-0000-0000-000000000000"
