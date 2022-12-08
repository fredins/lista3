type Props = {
  name : string
  id : Id
  onClick : (_:Id) => void
}

export default function ListItem({ name, id, onClick } : Props) {

  return (
    <li
    className="text-sm"
    onClick={() => onClick(id)}
    >
    { name }
    </li>
  )
}
