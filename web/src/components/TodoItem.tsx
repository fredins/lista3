import { useMutation, useQueryClient } from '@tanstack/react-query'
import { useActiveList } from './useActiveList'

import TodoForm from './TodoForm'
import Checkbox from './Checkbox'
import { updateTodo } from '../api'
import { map } from 'ramda'
import { omit } from '../util'
import { useState } from 'react'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  todo: Todo
  mode: Mode
  onModeChange: (mode: Mode, requestedMode: Mode) => void
}

export default function TodoItem({ todo, mode, onModeChange }: Props) : JSX.Element {
  const queryClient = useQueryClient()
  const { activeList } = useActiveList()
 
  if (!activeList) throw new Error("TodoItem: activeList undefined")

  const updateTodoMutation = useMutation(updateTodo, {
    onMutate: async (todo: Todo) => {
      await queryClient.cancelQueries(["todos"])
      queryClient.setQueryData(["todos", activeList.id], 
        (prev: Todo[] | undefined) => prev 
          ? map(t => t.id === todo.id ? todo : t, prev) 
          : undefined)
    }
  })

  switch (mode) {
    case "normal":
      return <ViewTodo
               todo={todo}
               mode={mode}
               onModeChange={onModeChange}
             />
    case "selected":
      return <ViewTodo
               todo={todo}
               mode={mode}
               onModeChange={onModeChange}
               className="bg-sky-100"
             />
    case "editing": return <EditTodo />
  }

  function handleSubmit(todo: Todo, e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    updateTodoMutation.mutate(todo)
    onModeChange(mode, "normal")
  }

  function EditTodo(){
     const [ text, setText ] = useState(todo.text)
     return <TodoForm
              todo={omit(todo, "text")}
              onExit={() => onModeChange(mode, "normal")}
              onSubmit={handleSubmit}
              text={text}
              setText={setText}
            />
  }

  function ViewTodo({ todo, mode, onModeChange, className }: Props & { className?: string }): JSX.Element {
    return (
      <div
        className={`flex text-lg cursor-pointer ${className}`}
        onClick={handleClick}>
      <span 
        className="flex items-center py-2" 
        onClick={handleClickCheckbox}
      >
      <Checkbox
        key={todo.id}
        active={todo.active}
        color={todo.color}
      />
      </span>
      <p
        className="pl-1  w-full flex items-center py-2"
        onClick={handleClickLabel}
      >
        {todo.text}
      </p>
      </div>
    )

    function handleClickLabel(e: React.MouseEvent<HTMLInputElement>) {
      if (!(e.shiftKey || e.ctrlKey))
        onModeChange(mode, "editing")
    }

    function handleClickCheckbox(e: React.MouseEvent<HTMLInputElement>) {
      if (!(e.shiftKey || e.ctrlKey))
        updateTodoMutation.mutate({ ...todo, active: !todo.active })
    }

    function handleClick(e: React.MouseEvent<HTMLDivElement, MouseEvent>): void {
      if (e.shiftKey) {
        updateTodoMutation.mutate({ ...todo, color: todo.color === "gray" ? "blue" : "gray" })
      }
      else if (e.ctrlKey) {
        onModeChange(mode, mode === "normal" ? "selected" : "normal")
      }
    }
  }
}

