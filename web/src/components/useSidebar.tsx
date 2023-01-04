import { useMemo, createContext, useState, useContext } from 'react'

type SideBarContext = {
  sidebar    : boolean
  showSidebar : (_: boolean) => void
  toggleSidebar : () => void
}

const sidebarContext = createContext<SideBarContext>(null!)

function SidebarProvider(props: { children: React.ReactNode }){
  const [ sidebar, setSidebar] = useState<boolean>(false)
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

export type {
  SideBarContext
}

export {
  SidebarProvider, 
  useSidebar,
}
