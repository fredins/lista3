import { fetchLists } from '../api'
import { useQuery } from '@tanstack/react-query'
import { useAuth } from './Auth'
import { find } from 'ramda'
import { useState } from 'react'
import TodoList from './TodoList'
import ListPanel from './ListPanel'



export default function Home() {
  const auth = useAuth()

  const { data: lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: () => fetchLists(auth.sessionKey!)
  });

  const [activeList, setActiveList] = useState<Id>()
  const [editing, setEditing] = useState<Id>()
  const [selected, setSelected] = useState<Id[]>([])

  return (
    <div
      className="flex pt-2 justify-center"
    >
      {lists && 
        <ListPanel
          lists={lists ? lists : []}
          activeList={activeList}
          setActiveList={setActiveList}

        />
      }
      {lists && activeList &&
        <TodoList
          activeList={find(({ id }: List) => id === activeList)(lists)!}
          editing={editing}
          setEditing={setEditing}
          selected={selected}
          setSelected={setSelected}
        />
      }
    </div>
  )
}
