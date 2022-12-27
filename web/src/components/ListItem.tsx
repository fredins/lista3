import { just } from "../util"
import { useActiveList } from "./useActiveList"

type Props = {
  list    : List
  onClick : (_:List) => void
}

export default function ListItem({ list, onClick } : Props) {
  const { activeList } = useActiveList()

  return (
    <li
    className={
      `cursor-pointer hover:underline underline-offset-1 text-blue-600 text-lg
       hover:text-blue-800 ${ activeList === just(list) ? "text-blue-800" : ""}`
    }
    onClick={() => onClick(list)}
    >
    { list.name }
    </li>
  )
}
