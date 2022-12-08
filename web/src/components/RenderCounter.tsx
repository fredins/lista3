import { useRef } from 'react'

export default function RenderCounter(){
  const cnt = useRef(0)
  cnt.current++
  return (
   <p> 
    {cnt.current}
   </p>
  )
}
