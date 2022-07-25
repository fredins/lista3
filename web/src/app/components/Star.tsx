import { AiFillStar, AiOutlineStar } from "react-icons/ai"

type Props = {
  className?: string
  stared: boolean
  onClick: () => void 
  size?: string | number | undefined
}

export default function Star({className, stared, onClick, size}: Props){
  return ( 
    <div 
      className={`inline-block ${size ? `w-[${size}] h-[${size}]` : ""} ${className ? className : ""}`}
      onClick={onClick}
    >
    { stared ? <AiFillStar size={size} /> : <AiOutlineStar size={size} /> }
    </div>
  )
}
