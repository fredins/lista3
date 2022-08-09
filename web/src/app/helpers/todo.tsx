import { map } from 'ramda'

import { Todo } from '../types/DB'

export function updateTodo(x: Todo, xs: Todo[]): Todo[] {
  return map(x_ => x_.id === x.id ? x : x_, xs)
}
