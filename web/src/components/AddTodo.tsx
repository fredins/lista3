import { AiOutlinePlus } from 'react-icons/ai'
import { useQueryClient, useMutation } from '@tanstack/react-query'

import TodoForm from './TodoForm'
import { createNewTodo } from '../api'
import { nil } from '../util'
import { useState } from 'react'

type Mode = "normal"
          | "selected"
          | "editing"

type Props = {
  listId : Id
  mode: Exclude<Mode, "selected">
  onModeChange: (mode: Mode, requestedMode: Mode) => void
}

export default function AddTodo({ listId, mode, onModeChange }: Props) {
  const queryClient = useQueryClient()
  const [ text, setText ] = useState("")
  
  const createTodoMutation = useMutation(newTodo => createNewTodo(newTodo), {
    onMutate: async (newTodo: NewTodo) => {
      queryClient.setQueryData(["todos", listId], (prev: Todo[] | undefined) => prev ? 
        [...prev, {...newTodo, id: "optimistic-todo"}] : undefined)
      await queryClient.cancelQueries(['todos'])
    },
    onSuccess: () => queryClient.invalidateQueries(["todos"])
  })

  switch (mode) {
    case "normal":
      return <Normal />

    case "editing":
      return <TodoForm
               todo={{ id:nil, active: true, color: "gray", listId: listId }}
               onExit={() => onModeChange(mode, "normal")}
               onSubmit={handleSubmit}
               text={text}
               setText={setText}
             />
  }


  function handleSubmit(todo : Todo, e: React.FormEvent<HTMLFormElement>){
    e.preventDefault()
    const {id, text, active, color, listId} = todo
    createTodoMutation.mutate(id === nil ? { text, active, color, listId } : todo)
    setText("")
  }  

  function Normal() {
    return (
      <div
        className="flex items-center text-gray-400 hover:text-sky-500 pb-2 cursor-pointer text-lg"
        onClick={() => onModeChange(mode, "editing")}
      >
        <AiOutlinePlus
          className='mx-2'

        />
        <p
          className='pl-1'
        >
          Lägg till uppgift
        </p>
      </div>
    )
  }
}

