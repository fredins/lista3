import { GoCheck } from 'react-icons/go'

import { caseOf_ } from "../helpers/unsorted"
import { Color } from "../types/DB"

type Props = {
  active: boolean
  onChangeActive: () => void
  color: Color
}

export default function CheckBox({ active, onChangeActive, color }: Props): JSX.Element {
  return (
    <label
      className={`
        border rounded-[1px] inline-block w-4 h-4 min-w-[1rem] mx-2 shadow-inner-eq
        ${caseOf_(color,
        { k: "gray", v: "border-gray-300 shadow-gray-200" },
        { k: "blue", v: "border-sky-300 shadow-sky-200" },
        { k: "yellow", v: "border-lemon-300 shadow-lemon-200" },
        { k: "red", v: "border-red-300 shadow-red-200" },
        { k: "pink", v: "border-pink-300 shadow-pink-200" },
      )}

       `
      }
    >
    { active && <GoCheck size="14px"/> }
    <input
        type="checkbox"
        className="hidden"
        onChange={onChangeActive}
      />
    </label>
  )
}
