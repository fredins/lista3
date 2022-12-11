import { Dispatch } from 'react'
import { map, includes, curry, filter } from 'ramda'
import { useQuery } from '@tanstack/react-query'

import { fetchAllTodos } from '../api'
import UnmemoizedTodo from './TodoItem'
import AddTodo from './AddTodo'
import { nil } from '../util'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  activeList: List
  editing: Id | undefined
  setEditing: Dispatch<Id | undefined>
  selected: Id[]
  setSelected: Dispatch<Id[]>
}

export default function TodoList({ activeList, editing, setEditing, selected, setSelected }: Props): JSX.Element {
  const { data : todos } = useQuery<Todo[], Error>({
    queryKey: ["todos"],
    queryFn: () => fetchAllTodos(activeList.id),
    initialData: []
 
  })

  return (
    <div
      className="mx-2 border w-96"
    >
      <div
      className="bg-zinc-50 h-8 border-b px-2 flex flex-col justify-center"
      >
      {activeList.name}
      </div>
      <div
        className="divide-y divide-gray-300 p-4"
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
          listId={activeList.id}
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
