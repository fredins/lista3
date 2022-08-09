import { useState } from 'react'
import { ReactQueryDevtools } from '@tanstack/react-query-devtools'

import { Id } from './types/DB'
import UnmemoizedTodoList from './components/todoList'

/* TODO
 
 - use postition field to sort TodoList
 - add feature to move todos
 
 - add multi list support with list name as key

 - memoize things

 - add option to remove todo by first selecting

 - add mobile getstures for selecting and changing color
 
*/

export default function App(): JSX.Element {
  const [editing, setEditing] = useState<Id>()
  const [selected, setSelected] = useState<Id[]>(() => [])

  return (
    <>
      <UnmemoizedTodoList
        editing={editing}
        setEditing={setEditing}
        selected={selected}
        setSelected={setSelected}
      />
      <ReactQueryDevtools />
    </>
  );
}
