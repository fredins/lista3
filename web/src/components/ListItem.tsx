import { just } from "../util"
import { useActiveList } from "./useActiveList"
import { BsThreeDots } from "react-icons/bs"
import { useState } from "react"
import DropDown from "./DropDown"

type Props = {
  list    : List
  onClick : (_:List) => void
  onDelete : () => void
}

export default function ListItem({ list, onClick, onDelete } : Props) {
  const { activeList } = useActiveList()
  const [showDropDown, setShowDropDown ] = useState<boolean>(false)

  return (
    <li 
      className={`
        flex justify-between items-center text-blue-600 text-lg 
         hover:text-blue-800 
         ${ activeList === just(list) ? "text-blue-800" : ""}`
      }
    >
    <span 
      className={`
        pl-1 pr-2 cursor-pointer hover:underline 
        underline-offset-1         `
      }
      onClick={e => {
        e.preventDefault()
        onClick(list)
      }}
    >
    { list.name }
    </span>
    <span 
      className={`
        pl-1 pr-2 cursor-pointer hover:text-blue-800 items-center`
      }
      onClick={e => {
       e.preventDefault()
       setShowDropDown(true)
      }}
    >
    { showDropDown && 
    <DropDown 
      onExit={() => setShowDropDown(false)}
      className='translate-x-[calc(28px-100%)] text-black'
    >
    <div 
      className="whitespace-nowrap"
      onClick={e => { 
        e.preventDefault() 
        onDelete()
      }}
    >Ta bort</div>
    </DropDown>
    }
    <BsThreeDots />
    </span>
    </li>
  )
}
