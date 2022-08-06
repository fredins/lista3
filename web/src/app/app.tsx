import { memo, useCallback, useState } from 'react'
import { useQuery } from 'react-query'
import { ReactQueryDevtools } from 'react-query/devtools'
import { map, any } from 'ramda'

import { TM } from './types/todo'
import * as db from './types/DB'
import { getAllTodos } from './apis/todoApi'
import UnmemoizedTodoList from './components/todoList'



export default function App(): JSX.Element {
  const [todos, setTodos] = useState<TM[]>(() => [])

  useQuery<db.Todo[], Error>('todos', getAllTodos, {
    onSuccess: xs =>
      setTodos(map(x => ({ todo: x, mode: "normal" }), xs))
  })

  // Needs to be locally defined. Perhaps because of shallow copy
  const TodoList = memo(UnmemoizedTodoList)

  return (
    <>
      <TodoList todos={todos} onChange={handleChange}/>
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
