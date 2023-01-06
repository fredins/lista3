import { memo, useMemo } from "react"
import { Maybe } from "../util"
import { useAuth } from "./Auth"
import InvitationsPanel from "./InvitationsPanel"
import ListPanel from "./ListPanel"
import SharePanel from "./SharePanel"
import TodoList from "./TodoList"
import { useActiveList } from "./useActiveList"

type Props = {
  lists       : Maybe<List[]>
  newList     : string
  setNewList  : (_: string) => void
  editing     : Maybe<Id>
  setEditing  : (_: Maybe<Id>) => void
  selected    : Id[]
  setSelected : (_: Id[]) => void
}

function DesktopHome({
  lists, newList, setNewList, editing, setEditing, selected, setSelected
}: Props){

  const { isLoggedIn } = useAuth()
  const { activeList } = useActiveList()

  return (
  <div>
  <div
    className="flex pt-2 justify-center "
  >
  { /* Left column */ }
  <div className='space-y-4 flex-row'>
  { /* List Panel */ }
  <ListPanel
    className='md:border bg-zinc-50 rounded-sm drop-shadow-sm w-52'
    newList={newList}
    setNewList={setNewList}
    lists={lists ?? []}
    onNewActive={() => setEditing(undefined)}
  />
  </div>

  { /* Middle column */ }
  { activeList &&
  <TodoList
    className='ml-4 h-fit w-4/12 max-w-4xl'
    editing={editing}
    setEditing={setEditing}
    selected={selected}
    setSelected={setSelected}
  />
  }

  { /* Right column */ }
  <div className='space-y-4 flex-row ml-4'>
  { /* Share Panel */ }
  { lists &&  
  <SharePanel className='w-fit h-fit border-b border drop-shadow-sm' />
  }

  { /* Invitations Panel */ }
  <InvitationsPanel
    className="border-b border drop-shadow-sm"
    setNewList={setNewList}
  /> 
  </div>
  </div>
  </div>
  )
}

export default memo(DesktopHome)
