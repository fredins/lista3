type Props = {
  completed: boolean
  marked: boolean
  onChangeCompleted: () => void
  onChangeMarked: () => void
}

export default function CheckBox({completed, marked, onChangeCompleted, onChangeMarked }: Props): JSX.Element {

  return (
    <label
      className={`
        border-gray-500 border rounded-sm inline-block w-3 h-3 mx-10
        ${completed? "bg-sky-500" : "bg-pink-500"}
        ${marked? "outline  outline-offset-1 outline-px" : "bg-pink-500"}
        `
      }
    >
      <input
        type="checkbox"
        className="invisible"
        onChange={onChangeCompleted}
      />
    </label>
  )
}
