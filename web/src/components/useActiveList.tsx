import { useMemo, createContext, useState, useContext } from 'react'
import { Maybe } from '../util'

type ActiveListContext = {
  activeList    : Maybe<List>
  setActiveList : (_: Maybe<List>) => void
}

const activeListContext = createContext<ActiveListContext>(null!)

function ActiveListProvider(props: { children: React.ReactNode }){
  const [ activeList, setActiveList ] = useState<Maybe<List>>(undefined)

  const contextValue = useMemo(
    () => ({activeList, setActiveList}), [activeList]
  )
  
  return <activeListContext.Provider 
          value={contextValue} 
          children={props.children} 
        /> 
}

function useActiveList(){
  return useContext(activeListContext)
}

export type {
  ActiveListContext
}

export {
  ActiveListProvider, 
  useActiveList,
}
