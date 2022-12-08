import { fetchLists, createList } from '../api'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { useAuth } from './Auth'
import { map, find } from 'ramda'
import { useMemo, useState } from 'react'
import ListItem from './ListItem'
import TodoList from './TodoList'



export default function Home() {
  const auth = useAuth()
  const queryClient = useQueryClient()

  const { data: lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: () => fetchLists(auth.sessionKey!)
  });


  const createListMutation = useMutation(
    () => createList(newList, auth.sessionKey!), {
    onSuccess: () => queryClient.invalidateQueries(["lists"]),
    onError: (error: Error) => console.log(error.message),
    onSettled: () => setNewList("")
  }
  )


  const [newList, setNewList] = useState("")
  const [activeList, setActiveList] = useState<Id>()
  const [editing, setEditing] = useState<Id>()
  const [selected, setSelected] = useState<Id[]>([])


  const listItems: JSX.Element[] = useMemo(
    () => map(({ id, name }) => (
      <ListItem
        key={id}
        name={name}
        id={id}
        onClick={id => {
          if (activeList === id) {
            return
          }
          setActiveList(id)
          queryClient.invalidateQueries(['todos'])
        }}
      />
    ), lists ? lists : []
    )
    , [lists, activeList, queryClient])

  return (
    <div 
      className="flex p-5"
    >
     <div>
      <form 
        className="flex"
        onSubmit={e => {
        e.preventDefault()
        createListMutation.mutate()
      }}>
        <input
          value={newList}
          onChange={e => setNewList(e.target.value)}
          className="bg-white px-2 text-sm border-l border-y border-gray-300 rounded-l-sm drop-shadow-sm w-24 outline-none"
        />
        <button
          className="bg-white px-2 text-sm border border-gray-300 rounded-r-sm drop-shadow-sm"
         > Ny lista </button>
      </form>
      <ul>{listItems}</ul>
      </div>
    { activeList && lists &&
      <TodoList 
        list={find(({ id }: List) => id === activeList)(lists)!}
        editing={editing}
        setEditing={setEditing}
        selected={selected}
        setSelected={setSelected}
      />
    }
    </div>
  )
}
