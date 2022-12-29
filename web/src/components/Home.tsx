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

export default function Home() {
  const auth = useAuth()
  const queryClient = useQueryClient()
  const { activeList } = useActiveList()

  const { data : lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: fetchLists, 
    enabled: auth.status === "loggedIn",
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
    <MenuBar />
    <div
      className="flex pt-2 justify-center "
    >
    { /* Left column */ }
    <div className='space-y-4 flex-row'>
    { /* ListPanel */ }
    { auth.status === "loggedIn" &&
    <ListPanel
      className=''
      lists={lists ?? []}
      onNewActive={() => setEditing(undefined)}
    />
    }

    { /* SharePanel */ }
    <div className='flex justify-end'>
    { lists &&  
    <SharePanel className='w-fit h-fit' />
    }
    </div>
    </div>
 
    { /* Right column */ }
    { activeList &&
    <TodoList
      className='ml-4 h-fit'
      editing={editing}
      setEditing={setEditing}
      selected={selected}
      setSelected={setSelected}
    />
    }
    </div>
    </div>
    )
  }

  return <AdaptiveView 
           desktop={<DesktopHome/>}
           mobile={<p>hello</p>}
         />
}
