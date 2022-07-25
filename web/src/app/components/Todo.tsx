import { useState } from "react"
import CheckBox from "./Checkbox"
import { caseOf_ } from '../helpers/unsorted'

type Props = {
  id: number
  text: string
  completed: boolean
  marked: boolean
}

type Mode = "normal"
  | "editing"  // click
  | "selected" // pc: ctrl + click, mobile: right-left swipe
// marked pc: shift + click, mobile, left-right swipe 

export default function Todo({ id, text, completed, marked }: Props): JSX.Element {
  const [completed_, setCompleted] = useState(completed)
  const [marked_, setMarked] = useState(marked)
  const [mode, setMode] = useState<Mode>("normal")

  return (
    caseOf_(mode,
      { k: "normal", v: <Normal /> },
      { k: "selected", v: <Selected /> },
      { k: "editing", v: <Editing /> }
    )
  )

  function Normal(): JSX.Element {
    return (
      <div
        className="static flex items-center"
      >
        <CheckBox
          key="test"
          completed={completed_}
          marked={marked_}
          onChangeCompleted={() => setCompleted(!completed_)}
          onChangeMarked={() => setMarked(!marked_)}
        />
        <div
          onClick={handleClick}
          className="pl-1 text-2xl w-full bg-orange-50"
        >
          <label
            className="text-2xl"
          >
            {text}
          </label>
        </div>
      </div>
    )
  }

  // TODO
  function Editing(): JSX.Element {
    const [text_, setText] = useState(text)

    return (
      <div
        className="static flex items-center"
      >
      <input value={text_} onChange={e => setText(e.target.value)}/>

     </div>
   )
  }

  // TODO
  function Selected(): JSX.Element {
    return (<div>select</div>)
  }

  function handleClick(e: React.MouseEvent<HTMLDivElement, MouseEvent>): void {
   if (e.shiftKey)
     setMarked(!marked_)
   else if(e.ctrlKey)
     setMode("selected")
   else 
     setMode("editing")
  }
}


