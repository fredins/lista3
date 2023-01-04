import { useMemo, createContext, useState, useContext } from 'react'



type AuthContext = {
  login: (_ : UserDetails) => void
  logout: () => void
} & AuthContextState

type AuthContextState = {
  isLoggedIn   : boolean 
  userDetails? : UserDetails
}

const authContext = createContext<AuthContext>(null!)

function useAuth(){
  return useContext(authContext)
}

function AuthProvider(props: { children: React.ReactNode }) {
  const [state, setState] = useState<AuthContextState>({
    isLoggedIn: false
  })

  const login = (userDetails : UserDetails) => setState({ 
      isLoggedIn: true,
      userDetails: userDetails,
    })

  const logout = () => setState({ isLoggedIn: false })
  

  const contextValue = useMemo(
    () => ({
      ...state,
      login,
      logout,
    }),
    [state],
  )


  return <authContext.Provider value={contextValue} children={props.children} />
}
export type {
  AuthContext,
  AuthContextState, 
}

export {
  AuthProvider,
  useAuth
}
