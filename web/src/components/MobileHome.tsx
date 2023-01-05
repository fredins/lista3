import { motion, AnimatePresence } from "framer-motion"
import { useSidebar } from "./useSidebar"
import { IoMdArrowBack } from 'react-icons/io'
import ListPanel from "./ListPanel"
import { useActiveList } from "./useActiveList"
import TodoList from "./TodoList"
import { memo } from "react"
import { Maybe } from "../util"

type Props = {
  lists       : Maybe<List[]>
  newList     : string
  setNewList  : (_: string) => void
  editing     : Maybe<Id>
  setEditing  : (_: Maybe<Id>) => void
  selected    : Id[]
  setSelected : (_: Id[]) => void
}

function MobileHome({
  lists, newList, setNewList, editing, setEditing, selected, setSelected
}: Props){
  const { sidebar, toggleSidebar } = useSidebar()
  const { activeList } = useActiveList()

  return ( 
  <div>
  <AnimatePresence>
  { sidebar &&  
  <>
  <motion.div 
    key='sidebar'
    initial={{translateX: '-100%' }}
    animate={{ translateX: 0 }}
    exit={{ translateX: '-100%' }}
    transition={{ ease: "easeOut", duration: 0.15 }}
    className='absolute inset-0 w-10/12 bg-white z-30 p-2'
    onClick={e => e.stopPropagation()}
  >
  <div 
    className="flex items-center cursor-pointer hover:underline 
               underline-offset-1 text-blue-600 hover:text-blue-800 text-xl
               pb-4 
              "
    onClick={toggleSidebar}
  >
  <IoMdArrowBack />
  <p>Tillbaka</p>
  </div>
  <ListPanel
    newList={newList}
    setNewList={setNewList}
    lists={lists ?? []}
    onNewActive={() => setEditing(undefined)}
  />
  
  </motion.div>
  <motion.div
    key='shadow'
    initial={{ opacity: 0 }}
    animate={{ opacity: 0.7 }}
    exit={{ opacity: 0 }}
    transition={{ ease: 'easeOut', duration: 0.15 }}
    className='absolute inset-0 bg-zinc-700 z-20'
    onClick={toggleSidebar}
  /> 
  </>
  }
  </AnimatePresence>

  { activeList &&
  <TodoList
    className='w-screen'
    editing={editing}
    setEditing={setEditing}
    selected={selected}
    setSelected={setSelected}
  />
  }
  </div>
  )
}

export default memo(MobileHome)
