import { useMemo, createContext, useState, useContext } from 'react'
import { just, Maybe } from '../util'

export type {
  ActiveListContext
}

export {
  ActiveListProvider, 
  useActiveList,
}

type ActiveListContext = {
  activeList    : Maybe<List>
  setActiveList : (_: List) => void
}

const activeListContext = createContext<ActiveListContext>(null!)

function ActiveListProvider(props: { children: React.ReactNode }){
  const [ activeList, setActiveList_ ] = useState<Maybe<List>>(undefined)

  const setActiveList = (activeList : List) => setActiveList_(just(activeList))

  const contextValue = useMemo(() => ({activeList, setActiveList}), [activeList])
  

  return <activeListContext.Provider value={contextValue} children={props.children} />
}

function useActiveList(){
  return useContext(activeListContext)
}

