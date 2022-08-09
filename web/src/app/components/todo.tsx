import { useMutation, useQueryClient } from '@tanstack/react-query'

import * as db from '../types/DB'
import { Color } from '../types/DB'
import { caseOf_ } from '../helpers/unsorted'
import { Mode } from '../types/todo'
import EditTodo from './editTodo'
import Checkbox from './checkbox'
import * as api from '../apis/todoApi'
import { updateTodo } from '../helpers/todo'
import RenderCounter from '../helpers/renderCounter'

type Props = {
  todo: db.Todo
  mode: Mode
  onModeChange: (mode: Mode, requestedMode: Mode) => void
}

export default function Todo({ todo, mode, onModeChange }: Props) {
  const qc = useQueryClient()

  const mut = useMutation(api.updateTodo, {
    onMutate: async (todo: db.Todo) => {
      await qc.cancelQueries(["todos"])
      qc.setQueryData(["todos"], (prev: db.Todo[] | undefined) => prev ?
        updateTodo(todo, prev) : undefined)
    },
  })

  return caseOf_(mode,
    {
      k: "normal", v:
        <ViewTodo
          todo={todo}
          mode={mode}
          onModeChange={onModeChange}
          className="bg-white"
        />
    },
    {
      k: "selected", v:
        <ViewTodo
          todo={todo}
          mode={mode}
          onModeChange={onModeChange}
          className="bg-sky-100"
        />
    },
    {
      k: "editing", v:
        <EditTodo
          text={todo.text}
          color={todo.color}
          onExit={() => onModeChange(mode, "normal")}
          onSubmit={handleSubmit}
        />
    }
  )

  function handleSubmit(text: string, color: Color, e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    mut.mutate({ ...todo, text: text, color: color })
    onModeChange(mode, "normal")
  }
}

function ViewTodo({ todo, mode, onModeChange, className }: Props & { className?: string }): JSX.Element {
  const qc = useQueryClient()

  const mut = useMutation(api.updateTodo, {
    onMutate: async (todo: db.Todo) => {
      await qc.cancelQueries(["todos"])
      qc.setQueryData(["todos"], (prev: db.Todo[] | undefined) => prev ?
        updateTodo(todo, prev) : undefined)
    },
  })


  return (
    <div
      className={`flex items-center py-1 cursor-pointer ${className}`}
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
      <RenderCounter />
    </div>
  )

  function handleClickLabel(e: React.MouseEvent<HTMLInputElement>) {
    if (!(e.shiftKey || e.ctrlKey))
      onModeChange(mode, "editing")
  }

  function handleClickCheckbox(e: React.MouseEvent<HTMLInputElement>) {
    if (!(e.shiftKey || e.ctrlKey))
      mut.mutate({ ...todo, active: !todo.active })
  }

  function handleClick(e: React.MouseEvent<HTMLDivElement, MouseEvent>): void {
    if (e.shiftKey) {
      mut.mutate({ ...todo, color: todo.color === "gray" ? "blue" : "gray" })
    }
    else if (e.ctrlKey) {
      onModeChange(mode, mode === "normal" ? "selected" : "normal")
    }
  }

}
