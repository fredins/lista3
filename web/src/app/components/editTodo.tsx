import { useState } from 'react'
import { curry } from 'ramda'

import * as db from '../types/DB'

type Props = {
  text: string
  color: db.Color
  onExit: () => void
  onSubmit: (text: string, color: db.Color, e: React.FormEvent<HTMLFormElement>) => void
}

export default function EditTodo({ text, color, onExit, onSubmit }: Props): JSX.Element {
  const [text_, setText] = useState(text)
  const [color_, setColor] = useState<db.Color>(color)

  return (
    <form
      className="mt-2 mb-6 border-none"
      onSubmit={curry(onSubmit)(text_, color_)}
    >
      <div
        className="static flex items-center border border-gray-300 rounded p-2 bg-white"
      >
        <input
          value={text_}
          onChange={e => setText(e.target.value)}
          className="outline-none"
          autoFocus
        />
      </div>

      <div
        className="flex flex-row justify-between space-x-1 mt-1"
      >
        <select
          className="bg-white text-sm border border-gray-300 rounded-sm drop-shadow-sm outline-none"
          defaultValue={color_}
          onChange={e => setColor(e.target.value as db.Color)}
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
            className="bg-white px-2 text-sm border border-gray-300 rounded-sm drop-shadow-sm"
            type="button"
            onClick={onExit}
          >Avbryt</button>
          <button
            className="bg-white px-2 text-sm border border-gray-300 rounded-sm drop-shadow-sm"
          >Spara
          </button>
        </div>
      </div>
    </form>
  )
}
