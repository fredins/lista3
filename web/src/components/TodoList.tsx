import { Dispatch } from 'react'
import { map, includes, curry, filter } from 'ramda'
import { useQuery } from '@tanstack/react-query'

import { fetchAllTodos } from '../api'
import UnmemoizedTodo from './TodoItem'
import AddTodo from './AddTodo'
import { ClassName, nil } from '../util'
import { useActiveList } from './useActiveList'
import { Desktop } from './MediaQuery'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  editing: Id | undefined
  setEditing: Dispatch<Id | undefined>
  selected: Id[]
  setSelected: Dispatch<Id[]>
}

export default function TodoList(
  { editing, setEditing, selected, setSelected, className }: Props & ClassName
): JSX.Element {

  const { activeList } = useActiveList()

  if (!activeList) throw new Error("TodoList: activeList undefined")

  const { data : mtodos } = useQuery<Todo[], Error>({
    queryKey: ["todos", activeList.id],
    queryFn: () => fetchAllTodos(activeList.id),
    refetchInterval: 1000*6, // refetch every 10s
  })
  
  return (
    <div
      className={`md:border drop-shadow-sm ${className}`}
    >
    <Desktop>  
    <div
    className="bg-zinc-100 h-8 border-b px-2 flex flex-col justify-center 
               relative text-lg"
    >
    { activeList.name }
    </div>
    </Desktop>
    <div
      className="divide-y divide-zinc-300 p-4"
    >
    <AddTodo
      listId={activeList.id}
      mode={editing === nil ? "editing" : "normal"}
      onModeChange={curry(handleModeChange)(nil)}
    />
    {
      map(t => (
        <UnmemoizedTodo
          key={t.id}
          todo={t}
          mode={getMode(t.id)}
          onModeChange={curry(handleModeChange)(t.id)}
        />
      ), mtodos ?? [])
    }
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
