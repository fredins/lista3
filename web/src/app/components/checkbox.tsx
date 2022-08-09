import { GoCheck } from 'react-icons/go'

import { caseOf_ } from "../helpers/unsorted"
import { Color } from "../types/DB"

type Props = {
  active: boolean
  onClick: (e: React.MouseEvent<HTMLInputElement>) => void
  color: Color
}

export default function Checkbox({ active, onClick, color }: Props): JSX.Element {
  return (
    <label
      className={`
        border rounded-[1px] inline-block w-4 h-4 min-w-[1rem] mx-2 
        shadow-inner-eq cursor-pointer
        ${caseOf_(color,
        { k: "gray", v: "border-gray-300 shadow-gray-200" },
        { k: "blue", v: "border-sky-300 shadow-sky-200" },
        { k: "yellow", v: "border-yellow-300 shadow-yellow-200" },
        { k: "red", v: "border-red-300 shadow-red-200" },
        { k: "green", v: "border-green-300 shadow-green-200" },
      )}

       `
      }
    >
      {!active && <GoCheck size="14px" />}
      <input
        type="checkbox"
        className="hidden"
        onClick={onClick}
      />
    </label>
  )
}
