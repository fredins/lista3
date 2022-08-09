import { AiOutlinePlus } from 'react-icons/ai'
import { useQueryClient, useMutation } from '@tanstack/react-query'

import EditTodo from './editTodo'
import { caseOf_ } from '../helpers/unsorted'
import { Color, NewTodo, Todo } from '../types/DB'
import { Mode } from '../types/todo'
import { postNewTodo } from '../apis/todoApi'
import { nil } from '../helpers/unsorted'



type Props = {
  mode: Exclude<Mode, "selected">
  onModeChange: (mode: Mode, requestedMode: Mode) => void
}

export default function AddTodo({ mode, onModeChange }: Props) {
  const qc = useQueryClient()
  
  const mut = useMutation(postNewTodo, {
    onMutate: async (newTodo: NewTodo) => {
      await qc.cancelQueries(['todos'])
      qc.setQueryData(["todos"], (prev: Todo[] | undefined) => prev ? 
        [...prev, {...newTodo, id: nil}] : undefined)
    },
    onSuccess: () => qc.invalidateQueries(["todos"])
  })

  return caseOf_(mode,
    {
      k:
        "normal",
      v:
        <Normal />
    },
    {
      k:
        "editing",
      v:
        <EditTodo
          text=""
          color="gray"
          onExit={() => onModeChange(mode, "normal")}
          onSubmit={handleSubmit}
        />
    },

  )

  function handleSubmit(text: string, color: Color, e: React.FormEvent<HTMLFormElement>){
    e.preventDefault()
    mut.mutate({ text: text, color: color, active: true })
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

