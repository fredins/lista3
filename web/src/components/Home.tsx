import { fetchAllTodos, fetchLists } from '../api'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useAuth } from './Auth'
import { forEach, curry } from 'ramda'
import { useState } from 'react'
import TodoList from './TodoList'
import ListPanel from './ListPanel'
import MenuBar from './MenuBar'
import { useActiveList } from './useActiveList'

export default function Home() {
  const auth = useAuth()
  const queryClient = useQueryClient()
  const { activeList } = useActiveList()

  const { data } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: fetchLists, 
    enabled: auth.status === "loggedIn",
    onSuccess: curry(forEach<List>)
      (x => queryClient.fetchQuery(["todos", x.id], () => fetchAllTodos(x.id), { 
      cacheTime: 1000*60*20
      }))
  });
  const lists = data ?? []

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
    />
    }
    { activeList &&
    <TodoList
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
