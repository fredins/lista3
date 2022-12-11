import { useMemo, Dispatch, useState } from 'react'
import ListItem from './ListItem'
import { append, map } from 'ramda'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { createList } from '../api'
import { nil } from '../util'



type Props = {
  lists         : List[]
  activeList    : Id | undefined
  setActiveList : Dispatch<Id>
}

export default function ListPanel({ lists, activeList, setActiveList } : Props ) {
  const queryClient = useQueryClient()
  
  const [newList, setNewList] = useState("")

  const createListMutation = useMutation(
    () => createList(newList), {
    onMutate: async (prevLists: List[]) => {
      await queryClient.cancelQueries({ queryKey: ["lists"]})
      queryClient.setQueryData(["lists"], append({id: nil, name: newList}, prevLists)) 
      setNewList("")
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
    () => map(({ id, name }) => (
      <ListItem
        key={id}
        activeList={activeList}
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
    ), lists
    )
    , [lists, activeList, queryClient, setActiveList])

  return (
     <div
      className="p-2 border bg-zinc-50 h-fit rounded-sm drop-shadow-sm"
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
          className="bg-white px-2 text-sm border-l border-y border-gray-300   w-24 outline-none"
        />
        <button
          className="bg-white px-2 text-sm border border-gray-300 rounded-r-sm "
         > Ny lista </button>
      </form>
      <ul className="pl-2">
       {listItems}
      </ul>
      </div>

  )
}
