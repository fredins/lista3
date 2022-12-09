type Props = {
  name       : string
  id         : Id
  onClick    : (_:Id) => void
  activeList : Id | undefined
}

export default function ListItem({ name, id, onClick, activeList } : Props) {

  return (
    <li
    className={
      `cursor-pointer hover:underline underline-offset-1 text-sm text-blue-600
       hover:text-blue-800 ${ activeList === id ? "text-blue-800" : ""}`
    }
    onClick={() => onClick(id)}
    >
    { name }
    </li>
  )
}
