import { memo } from 'react'
import { map } from 'ramda'

import { TM } from '../types/todo'
import UnmemoizedTodo from './todo'

type Props = {
  todos: TM[]
  onChange: (newTm: TM) => void
}

export default function TodoList({ todos, onChange }: Props): JSX.Element {
  return (
    <div
      className="relative mx-4 max-w-xs p-2 divide-y divide-gray-300"
    >
      {
        map(tm => (
          <Todo
            key={tm.todo.id}
            todo={tm.todo}
            mode={tm.mode}
            onChange={onChange}
          />
        ), todos)
      }
    </div>
  )
}

const Todo = memo(UnmemoizedTodo)
