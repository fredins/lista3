import { createReactRouter, createRouteConfig, useMatch, Outlet, RouterProvider} from '@tanstack/react-router'
import { useEffect } from 'react'
import { useAuth } from './Auth'
import type { AuthContext } from './Auth'
import { authenticate } from '../api'
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

/*
const withAuth = rootRoute.createRoute({
  id: "withAuth",
  beforeLoad: () => {
     if (router.options.context.auth.status === 'loggedOut') {
        console.log("context status is: " + router.options.context.auth.status)
        throw router.navigate({ to: "/auth" })
     }
   },
})
*/

//-------------------------------------------
// Routes
//-------------------------------------------


const homeRoute = rootRoute.createRoute({ 
 path: "/",
 component:Home
})


const authRoute = rootRoute.createRoute({
  path: "/auth/$sessionKey",
  parseParams: ({ sessionKey }) => ({
    sessionKey: sessionKey
  }),
  stringifyParams: ({ sessionKey }) => ({ sessionKey: `${sessionKey}`}),
  loader: async ({ params: { sessionKey } }) => {
    return {
      userDetails: await authenticate(sessionKey),
    }
  },
  component: function Auth() {
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
      authenticate(sessionKey)
      window.close()
    }, [auth, sessionKey, userDetails])

    return <>Lyckad inloggning!</>
    }
})


const routeConfig = rootRoute.addChildren([
  homeRoute,
  authRoute,
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

