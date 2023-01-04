import { fetchAllTodos, fetchLists } from '../api'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useAuth } from './Auth'
import { forEach, curry } from 'ramda'
import { useState } from 'react'
import TodoList from './TodoList'
import ListPanel from './ListPanel'
import MenuBar from './MenuBar'
import { useActiveList } from './useActiveList'
import SharePanel from './SharePanel'
import { AdaptiveView } from './MediaQuery'
import InvitationsPanel from './InvitationsPanel'
import { useSidebar } from './useSidebar'
import { motion, AnimatePresence } from "framer-motion";
import { IoMdArrowBack } from 'react-icons/io'

export default function Home() {
  const auth = useAuth()
  const queryClient = useQueryClient()
  const { activeList } = useActiveList()

  const { data : lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: fetchLists, 
    enabled: auth.isLoggedIn,
    onSuccess: curry(forEach<List>)
      (x => queryClient.fetchQuery(["todos", x.id], () => fetchAllTodos(x.id), { 
      cacheTime: 1000*60*20
      }))
  });

  const [editing, setEditing] = useState<Id>()
  const [selected, setSelected] = useState<Id[]>([])


  function DesktopHome() {
    return (
    <div>
    <div
      className="flex pt-2 justify-center "
    >
    { /* Left column */ }
    <div className='space-y-4 flex-row'>
    { /* List Panel */ }
    { auth.isLoggedIn &&
    <ListPanel
      className='md:border bg-zinc-50 rounded-sm drop-shadow-sm w-52'
      lists={lists ?? []}
      onNewActive={() => setEditing(undefined)}
    />
    }

    </div>
 
    { /* Middle column */ }
    { activeList &&
    <TodoList
      className='ml-4 h-fit w-4/12 max-w-4xl'
      editing={editing}
      setEditing={setEditing}
      selected={selected}
      setSelected={setSelected}
    />
    }

    { /* Right column */ }
    <div className='space-y-4 flex-row ml-4'>
    { /* Share Panel */ }
    { lists &&  
    <SharePanel className='w-fit h-fit' />
    }

    { /* Invitations Panel */ }
    <InvitationsPanel /> 
    </div>
    </div>
    </div>
    )
  }

  function MobileHome(){
    const { sidebar, toggleSidebar } = useSidebar()

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

  return (
  <div>
  <MenuBar />
  <AdaptiveView 
    desktop={<DesktopHome/>}
    mobile={<MobileHome />}
  />
  </div>
  )
}
