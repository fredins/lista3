import { createReactRouter, createRouteConfig, useMatch, Outlet, RouterProvider} from '@tanstack/react-router'
import { useContext, useEffect } from 'react'
import { authContext, useAuth } from './Auth'
import type { AuthContext } from './Auth'
import { authenticate, fetchLists } from '../api'
import Home from './Home'
import Spinner from './Spinner'
import { TanStackRouterDevtools } from '@tanstack/react-router-devtools'
import { ReactQueryDevtools } from '@tanstack/react-query-devtools'

export {
  Routes
}


declare module '@tanstack/react-router' {
  interface RouterContext {
    // auth: AuthContext
  }
}

const rootRoute = createRouteConfig({
  component: () => {
    return (
      <>
        <Outlet /> {/* Start rendering router matches */}
        <TanStackRouterDevtools position="bottom-right" />
        <ReactQueryDevtools />
      </>
    )

}})

const withAuth = rootRoute.createRoute({
  id: "withAuth",
  beforeLoad: () => {
     if (router.options.context.auth.status === 'loggedOut') {
        console.log("context status is: " + router.options.context.auth.status)
        throw router.navigate({ to: "/auth" })
     }
   },
})

//-------------------------------------------
// Routes
//-------------------------------------------


const homeRoute = withAuth.createRoute({ 
 path: "/",
 component:Home
})

function Auth(){
  const { 
    params: {
      sessionKey
    },
    loaderData: {
      userDetails
    }
  } = useMatch(authRoute.id)
  
  const auth = useAuth()

  useEffect(() => {
    auth.login(userDetails, sessionKey)
  }, [auth, sessionKey, userDetails])

  return (
    <>
      Auth successful! <br />
      <router.Link to="/"> Close </router.Link>
    </>)
}

const authRoute = rootRoute.createRoute({
  path: "/auth/$sessionKey",
  component:Auth,
  parseParams: ({ sessionKey }) => ({
    sessionKey: sessionKey
  }),
  stringifyParams: ({ sessionKey }) => ({ sessionKey: `${sessionKey}`}),
  loader: async ({ params: { sessionKey } }) => {
    return {
      userDetails: await authenticate(sessionKey),
    }
  }
})

const authRoute1 = rootRoute.createRoute({
  path: "/auth",
  component: () => {
    return (
      <>
        <router.Link to="/">Home</router.Link> <br />
        <a href="https://dev.fredin.org/login">Login with google</a>
      </>
    )

  }
})


const routeConfig = rootRoute.addChildren([
  withAuth.addChildren([homeRoute]),
  authRoute,
  authRoute1
])

const router = createReactRouter({ 
  routeConfig, 
  defaultPreload: "intent",
  context: {
    auth: {
      status: 'loggedOut',
    } as AuthContext,
  },
  defaultPendingComponent: () => (
    <div className={`p-2 text-2xl`}>
      <Spinner />
    </div>
  ),
})


function Routes(){
  return (
    <RouterProvider 
      router={router}
      defaultPreload="intent"
      context={{ auth: useAuth() }}
    /> 
  )
}

