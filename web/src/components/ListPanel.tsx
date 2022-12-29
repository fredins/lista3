import { useMemo, useState } from 'react'
import ListItem from './ListItem'
import { append, map } from 'ramda'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { createList } from '../api'
import { ClassName, just, nil } from '../util'
import { useActiveList } from './useActiveList'

type Props = {
  lists : List[]
  onNewActive : () => void
}

export default function ListPanel({ lists, onNewActive, className } : Props & ClassName) {
  const queryClient = useQueryClient()
  const { activeList, setActiveList } = useActiveList()
  
  const [newList, setNewList] = useState("")

  const createListMutation = useMutation(
    () => createList(newList), {
    onMutate: async (prevLists: List[]) => {
      queryClient.setQueryData(["lists"], append({id: nil, name: newList}, prevLists)) 
      setNewList("")
      await queryClient.cancelQueries({ queryKey: ["lists"]})
    },
    onError: (error: Error, prevLists: List[]) => {
      queryClient.setQueryData(["lists"], prevLists)
      console.log(error.message)
    },
    onSettled: () => {
      queryClient.invalidateQueries(["lists"])
    }
  })

  const listItems: JSX.Element[] = useMemo(
    () => map(list => (
      <ListItem
        key={list.id}
        list={list}
        onClick={list1 => {
          if (activeList === just(list1)) return
          setActiveList(list) 
          onNewActive()
        }}
      />
    ), lists
    )
    , [lists, activeList, setActiveList, onNewActive])

  return (
     <div
      className={`p-2 border bg-zinc-50 rounded-sm drop-shadow-sm ${className}`}
     >
      <form 
        className="mb-2"
        onSubmit={e => {
        e.preventDefault()
        if(newList === "") return
        createListMutation.mutate(lists)
       }}
      >
        <input
          value={newList}
          onChange={e => setNewList(e.target.value)}
          className="px-2 text-lg border-l border-y border-gray-300 w-24 outline-none"
        />
        <button
          className="px-2 text-lg border border-gray-300 rounded-r-sm "
         > Ny lista </button>
      </form>
      <ul className="pl-2">
       {listItems}
      </ul>
      </div>

  )
}
