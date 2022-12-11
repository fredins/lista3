import { useMemo, createContext, useState, useContext } from 'react'

export type {
  AuthContext,
  AuthContextState, 
}

export {
  authContext,
  AuthProvider,
  useAuth
}

type AuthContext = {
  login: (_ : UserDetails) => void
  logout: () => void
} & AuthContextState

type AuthContextState = {
  status       : 'loggedOut' | 'loggedIn'
  userDetails? : UserDetails
}

const authContext = createContext<AuthContext>(null!)

function useAuth(){
  return useContext(authContext)
}

function AuthProvider(props: { children: React.ReactNode }) {
  const [state, setState] = useState<AuthContextState>({
    status: 'loggedOut'
  })

  const login = (userDetails : UserDetails) => {
    console.log("login!")
    setState({ 
      status: 'loggedIn',
      userDetails: userDetails,
    })
  }

  const logout = () => {
    console.log("logout!")
    setState({ status: 'loggedOut' })
  }

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

