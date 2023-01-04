import { Dispatch, SetStateAction, useState } from 'react'
import { curry } from 'ramda'

type Props = {
  todo: Omit<Todo, "text">
  onExit: () => void
  onSubmit: (todo: Todo, e: React.FormEvent<HTMLFormElement>) => void
  text : string
  setText : Dispatch<SetStateAction<string>>
  
}

export default function TodoForm({todo, onExit, onSubmit, text, setText }: Props): JSX.Element {
  const [color, setColor] = useState<Color>(todo.color)

  return (
    <form
      className="mt-2 mb-6 border-none text-lg"
      onSubmit={curry(onSubmit)({...todo, text: text, color: color})}
    >
    <div
      className="static flex items-center border border-zinc-300 rounded p-2 bg-white"
    >
    <input
      value={text}
      onChange={e => setText(e.target.value)}
      className="outline-none shadow-none border-none"
      autoFocus
    />
    </div>

    <div
      className="flex flex-row justify-between space-x-1 mt-1"
    >
    <select
      className="border border-zinc-300 rounded-sm drop-shadow-sm outline-none 
                 bg-white"
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
      className="px-2 border border-zinc-300 rounded-sm drop-shadow-sm"
      type="button"
      onClick={onExit}
    >Avbryt</button>
    <button
      className="px-2 border border-zinc-300 rounded-sm drop-shadow-sm"
    >Spara
    </button>
    </div>
    </div>
    </form>
  )
}
