import { useState } from 'react'
import { useQuery } from 'react-query'
import { ReactQueryDevtools } from 'react-query/devtools'
import { map, curry, any, none } from 'ramda'

import { Mode, TM } from './types/todo'
import * as db from './types/DB'
import { getAllTodos } from './apis/todoApi'
import { caseOfDefaultB } from './helpers/unsorted'
import Todo from './components/todo'


export default function App(): JSX.Element {
  const [todos, setTodos] = useState<TM[]>([])

  useQuery<db.Todo[], Error>('todos', getAllTodos, {
    onSuccess: xs =>
      setTodos(map(x => ({ todo: x, mode: "normal" }), xs))
  })

  return (
    <>
      <div
        className="relative mx-4 max-w-xs p-2 divide-y divide-gray-300"
      >
        {
          map(tm =>
            <Todo
              key={tm.todo.id}
              todo={tm.todo}
              mode={tm.mode}
              onChange={handleChange}
            />
            , todos)
        }
      </div>

      <ReactQueryDevtools />
    </>
  );

  function handleChange(newTm: TM) {
    if (!(newTm.mode === "selected" && any(x => x.mode === "editing", todos))) {
      const eq = (x: TM) => x.todo.id === newTm.todo.id
      let f: (x: TM) => TM
      if (newTm.mode === "editing") 
        f = x => eq(x) ? newTm : { todo: x.todo, mode: "normal" }
      else
        f = x => eq(x) ? newTm : x
      setTodos(map(f, todos))
    }
  }
}
