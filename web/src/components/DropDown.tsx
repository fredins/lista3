import { useRef } from "react"
import { ClassName } from "../util"
import useOnClickOutside from "./useOnClickOutside"

type Props = {
  children : JSX.Element | JSX.Element[]
  onExit : () => void
}

export default function DropDown({ children, onExit, className }: Props & ClassName){
  const ref = useRef(null)
  useOnClickOutside(ref, onExit)
  
  return (
  <div
    ref={ref}
    className={`absolute p-2 border rounded-sm z-50 flex-row bg-white space-y-2
                drop-shadow ${className}
              `}
  >
  { children }
  </div>
  )
  
}
