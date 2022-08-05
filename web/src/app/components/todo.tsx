import * as db from '../types/DB'
import { caseOf_ } from '../helpers/unsorted'
import { Mode, TM } from '../types/todo'
import RenderCounter from '../helpers/renderCounter'
import AddTodo from './addTodo'
import CheckBox from './checkbox'

type Props = {
  todo: db.Todo
  mode: Mode
  onChange: (newTm: TM) => void
}

export default function Todo({ todo, mode, onChange }: Props) {
  return caseOf_(mode,
    {
      k: "normal", v:
        <ViewTodo
          todo={todo}
          mode={mode}
          onChange={onChange}
          className="bg-white"
        />
    },
    {
      k: "selected", v:
        <ViewTodo
          todo={todo}
          mode={mode}
          onChange={onChange}
          className="bg-sky-100"
        />
    },
    {
      k: "editing", v:
        <AddTodo
          text={todo.text}
          color={todo.color}
          onExit={() => onChange({ todo: todo, mode: "normal" })}
          onSubmit={handleSubmit}
        />
    }
  )

  function handleSubmit(text: string, color: db.Color, e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    onChange({ todo: { ...todo, text: text, color: color }, mode: "normal" })
  }
}



function ViewTodo({ todo, mode, onChange, className }: Props & { className?: string }): JSX.Element {
  return (
    <div
      className={`flex items-center py-1 ${className}`}
    >
      <CheckBox
        key="test"
        active={todo.active}
        onChangeActive={() => onChange({ todo: { ...todo, active: !todo.active }, mode: mode })}
        color={todo.color}
      />
      <p
        onClick={handleClick}
        className="pl-1  w-full"
      >
        {todo.text}
      </p>
      <RenderCounter />
    </div>
  )


  function handleClick(e: React.MouseEvent<HTMLDivElement, MouseEvent>): void {
    if (e.shiftKey) {
      onChange({ todo: { ...todo, color: todo.color === "gray" ? "blue" : "gray" }, mode: mode })
    }
    else if (e.ctrlKey) {
      onChange({ todo: todo, mode: mode === "selected" ? "normal" : "selected" })
    }
    else {
      onChange({ todo: todo, mode: "editing" })
    }
  }
}
