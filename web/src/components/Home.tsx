import { fetchLists } from '../api'
import { useQuery } from '@tanstack/react-query'
import { useAuth } from './Auth'
import { find } from 'ramda'
import { useState } from 'react'
import TodoList from './TodoList'
import ListPanel from './ListPanel'
import MenuBar from './MenuBar'

export default function Home() {
  const auth = useAuth()

  const { data: lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: fetchLists, 
    enabled: auth.status === "loggedIn",
    initialData: []
  });

  const [activeList, setActiveList] = useState<Id>()
  const [editing, setEditing] = useState<Id>()
  const [selected, setSelected] = useState<Id[]>([])

  return (
    <div>
    <MenuBar />
    <div
      className="flex pt-2 justify-center"
    >
    { auth.status === "loggedIn" &&
    <ListPanel
      lists={lists}
      activeList={activeList}
      setActiveList={setActiveList}

    />
    }
    {activeList &&
      <TodoList
        activeList={find(({ id }: List) => id === activeList)(lists)!}
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
