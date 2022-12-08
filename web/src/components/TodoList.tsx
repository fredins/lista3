import { Dispatch } from 'react'
import { map, includes, curry, filter } from 'ramda'
import { useQuery } from '@tanstack/react-query'

import { fetchAllTodos } from '../api'
import UnmemoizedTodo from './TodoItem'
import AddTodo from './AddTodo'
import { nil } from '../util'
import { useAuth } from './Auth'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  list: List
  editing: Id | undefined
  setEditing: Dispatch<Id | undefined>
  selected: Id[]
  setSelected: Dispatch<Id[]>
}

export default function TodoList({ list, editing, setEditing, selected, setSelected }: Props): JSX.Element {
  const { sessionKey } = useAuth()
  const { data } = useQuery<Todo[], Error>(["todos"], () => fetchAllTodos(list.id, sessionKey!))
  const todos = data ? data : []

  return (
    <div
      className='mx-4'
    >
      <p className="text-2xl">{list.name}</p>
      <div
        className="relative max-w-xs divide-y divide-gray-300"
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
          listId={list.id}
          mode={editing === nil ? "editing" : "normal"}
          onModeChange={curry(handleModeChange)(nil)}
        />
      </div>
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
