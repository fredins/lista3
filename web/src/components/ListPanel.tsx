import { Dispatch, memo, SetStateAction, useMemo } from 'react'
import ListItem from './ListItem'
import { append, filter, map } from 'ramda'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { createList, deleteList } from '../api'
import { ClassName, just, nil } from '../util'
import { useActiveList } from './useActiveList'
import { useSidebar } from './useSidebar'

type Props = {
  lists       : List[]
  onNewActive : () => void
  newList     : string
  setNewList  : (_: string) => void
}

function ListPanel({ lists, onNewActive, newList, setNewList, className } : Props & ClassName) {
  const queryClient = useQueryClient()
  const { activeList, setActiveList } = useActiveList()
  const { showSidebar } = useSidebar()

  type CreateListMutationArg = {
    newList   : string 
    prevLists : List[]
  }

  const createListMutation = useMutation(
    ({ newList }: CreateListMutationArg) => createList(newList), {
    onMutate: async ({ newList, prevLists }) => {
      const optimisticList = {id: nil, name: newList}
      queryClient.setQueryData(["lists"], append(optimisticList, prevLists)) 
      setNewList("")
      setActiveList(optimisticList)
      await queryClient.cancelQueries({ queryKey: ["lists"]})
    },
    onError: (error: Error, { prevLists }) => {
      queryClient.setQueryData(["lists"], prevLists)
      console.log(error.message)
    },
    onSettled: () => {
      queryClient.fetchQuery(["lists"])
    },
    onSuccess: setActiveList
  })

  const deleteListMutation = useMutation(
    ({ list }: {list : List, prevLists: List[]}) => deleteList(list), {
    onMutate: ({ list, prevLists }) => {
      queryClient.setQueryData(["lists"], filter(({ id }) => id !== list.id, prevLists));
      queryClient.cancelQueries({ queryKey: ["lists"]})
      if(activeList === list) setActiveList(undefined)
   },
    onError: (error: Error, { prevLists }) => {
      queryClient.setQueryData(["lists"], prevLists)
    }
  })

  const listItems: JSX.Element[] = useMemo(
    () => map(list => (
      <ListItem
        key={list.id}
        list={list}
        onDelete={() => deleteListMutation.mutate({list: list, prevLists: lists})}
        onClick={list1 => {
          if (activeList === just(list1)) {
            showSidebar(false)
            return
          }
          setActiveList(just(list)) 
          onNewActive()
          showSidebar(false)
        }}
      />
    ), lists
    )
    , [lists, activeList, setActiveList, onNewActive, showSidebar, deleteListMutation])

  return (
     <div
      className={`p-2 ${className}`}
     >
     <form 
       className="flex mb-2"
       onSubmit={e => {
         e.preventDefault()
         if(newList === "") return
         createListMutation.mutate({newList, prevLists: lists})
         showSidebar(false)
      }}
     >
     <input
       value={newList}
       onChange={e => { 
        e.preventDefault()
        setNewList(e.target.value)
       }}
       className="px-2 text-lg border-l border-y border-zinc-300 outline-none w-full"
     />
     <button
       className="px-2 text-lg border border-zinc-300 rounded-r-sm max-w-fit whitespace-nowrap"
      > Ny lista </button>
     </form>
     <ul>
      {listItems}
     </ul>
     </div>

  )
}

export default memo(ListPanel)
