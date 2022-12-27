import { useState } from 'react'
import { curry } from 'ramda'

type Props = {
  todo: Todo
  onExit: () => void
  onSubmit: (todo: Todo, e: React.FormEvent<HTMLFormElement>) => void
}

export default function EditTodo({todo, onExit, onSubmit }: Props): JSX.Element {
  const [text, setText] = useState(todo.text)
  const [color, setColor] = useState<Color>(todo.color)

  return (
    <form
      className="mt-2 mb-6 border-none"
      onSubmit={curry(onSubmit)({...todo, text: text, color: color})}
    >
      <div
        className="static flex items-center border border-gray-300 rounded p-2 bg-white"
      >
        <input
          value={text}
          onChange={e => setText(e.target.value)}
          className="outline-none"
          autoFocus
        />
      </div>

      <div
        className="flex flex-row justify-between space-x-1 mt-1"
      >
        <select
          className="bg-white border border-gray-300 rounded-sm drop-shadow-sm outline-none"
          defaultValue={color}
          onChange={e => setColor(e.target.value as Color)}
        >
          <option value="gray">grå</option>
          <option value="blue">blå</option>
          <option value="yellow">gul</option>
          <option value="red">röd</option>
          <option value="green">grön</option>
        </select>

        <div
          className="space-x-1"
        >
          <button
            className="bg-white px-2 border border-gray-300 rounded-sm drop-shadow-sm"
            type="button"
            onClick={onExit}
          >Avbryt</button>
          <button
            className="bg-white px-2 border border-gray-300 rounded-sm drop-shadow-sm"
          >Spara
          </button>
        </div>
      </div>
    </form>
  )
}
