import { Dispatch } from 'react'
import { map, includes, curry, filter } from 'ramda'
import { useQuery } from '@tanstack/react-query'

import { getAllTodos } from '../apis/todoApi'
import UnmemoizedTodo from './todo'
import * as db from '../types/DB'
import { Id } from '../types/DB'
import AddTodo from './addTodo'
import { Mode } from '../types/todo'
import { nil } from '../helpers/unsorted'

type Props = {
  editing: Id | undefined
  setEditing: Dispatch<Id | undefined>
  selected: Id[]
  setSelected: Dispatch<Id[]>
}

export default function TodoList({ editing, setEditing, selected, setSelected }: Props): JSX.Element {
  const { data } = useQuery<db.Todo[], Error>(["todos"], getAllTodos)
  const todos = data ? data : []

  return (
    <div
      className="relative mx-4 max-w-xs p-2 divide-y divide-gray-300"
    >
      {
        map(t => (
          <UnmemoizedTodo
            key={t.id}
            todo={t}
            mode={getMode(t.id)}
            onModeChange={curry(handleModeChange)(t.id)}
          />
        ), todos)
      }
      <AddTodo
        mode={editing === nil ? "editing" : "normal"}
        onModeChange={curry(handleModeChange)(nil)}
      />
    </div>
  )

  function handleModeChange(id: Id, mode: Mode, requestedMode: Mode) {
    if (mode === requestedMode) {
      throw new Error("mode and requestedMode are equal!")
    }

    switch (requestedMode) {
      case "normal":
        removeFromEditing(id)
        removeFromSelected(id)
        break

      case "selected":
        if (!editing) {
          setSelected([...selected, id])
        }
        break

      case "editing":
        setSelected([])
        setEditing(id)
        break
    }
  }

  function removeFromEditing(id: Id) {
    if (editing === id)
      setEditing(undefined)
  }

  function removeFromSelected(id: Id) {
    setSelected(filter(id_ => id_ !== id, selected))
  }

  function getMode(id: Id): Mode {
    if (editing === id) return "editing"
    if (includes(id, selected)) return "selected"
    return "normal"
  }
}
