import { AiOutlinePlus } from 'react-icons/ai'
import { useQueryClient, useMutation } from '@tanstack/react-query'

import EditTodo from './EditTodo'
import { createNewTodo } from '../api'
import { nil } from '../util'
import { useAuth } from './Auth'

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
  const { sessionKey } = useAuth()
  
  const createTodoMutation = useMutation(newTodo => createNewTodo(newTodo, sessionKey!), {
    onMutate: async (newTodo: NewTodo) => {
      await queryClient.cancelQueries(['todos'])
      queryClient.setQueryData(["todos"], (prev: Todo[] | undefined) => prev ? 
        [...prev, {...newTodo, id: nil}] : undefined)
    },
    onSuccess: () => queryClient.invalidateQueries(["todos"])
  })

  switch (mode) {
    case "normal":
      return <Normal />

    case "editing":
      return <EditTodo
               todo={{ id:nil, text: "", active: true, color: "gray", listId: listId }}
               onExit={() => onModeChange(mode, "normal")}
               onSubmit={handleSubmit}
             />
  }


  function handleSubmit(todo : Todo, e: React.FormEvent<HTMLFormElement>){
    e.preventDefault()
    const {id, text, active, color, listId} = todo
    createTodoMutation.mutate(id === nil ? { text, active, color, listId } : todo)
    onModeChange(mode, "normal")
  }  

  function Normal() {
    return (
      <div
        className='flex items-center text-gray-400 hover:text-sky-500 pt-2 cursor-pointer'
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

