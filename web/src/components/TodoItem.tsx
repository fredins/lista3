import { useMutation, useQueryClient } from '@tanstack/react-query'

import EditTodo from './EditTodo'
import Checkbox from './Checkbox'
import { updateTodo } from '../api'
import { map } from 'ramda'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  todo: Todo
  mode: Mode
  onModeChange: (mode: Mode, requestedMode: Mode) => void
}

export default function TodoItem({ todo, mode, onModeChange }: Props) {
  const queryClient = useQueryClient()

  const updateTodoMutation = useMutation(updateTodo, {
    onMutate: async (todo: Todo) => {
      await queryClient.cancelQueries(["todos"])
      queryClient.setQueryData(["todos"], 
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
               className="bg-white"
             />
    case "selected":
      return <ViewTodo
               todo={todo}
               mode={mode}
               onModeChange={onModeChange}
               className="bg-sky-100"
             />
    case "editing":
      return <EditTodo
               todo={todo}
               onExit={() => onModeChange(mode, "normal")}
               onSubmit={handleSubmit}
             />
  }

  function handleSubmit(todo: Todo, e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    updateTodoMutation.mutate(todo)
    onModeChange(mode, "normal")
  }

  function ViewTodo({ todo, mode, onModeChange, className }: Props & { className?: string }): JSX.Element {
    return (
      <div
        className={`text-lg flex items-center py-1 cursor-pointer ${className}`}
        onClick={handleClick} >
        <Checkbox
          key="test"
          active={todo.active}
          onClick={handleClickCheckbox}
          color={todo.color}
        />
        <p
          className="pl-1  w-full"
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

