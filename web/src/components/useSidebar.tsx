import { useMemo, createContext, useState, useContext } from 'react'

export type {
  SideBarContext
}

export {
  SidebarProvider, 
  useSidebar,
}

type SideBarContext = {
  sidebar    : boolean
  showSidebar : (_: boolean) => void
  toggleSidebar : () => void
}

const sidebarContext = createContext<SideBarContext>(null!)

function SidebarProvider(props: { children: React.ReactNode }){
  const [ sidebar, setSidebar] = useState<boolean>(true)
  const showSidebar = (b: boolean) => setSidebar(b)

  const contextValue = useMemo(() => ({
    sidebar, 
    showSidebar, 
    toggleSidebar: () => setSidebar(!sidebar)
  }), [sidebar ])
  

  return <sidebarContext.Provider value={contextValue} children={props.children} />
}

function useSidebar(){
  return useContext(sidebarContext)
}

