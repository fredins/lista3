import { useEffect } from 'react'
import { FaCog } from 'react-icons/fa'
import { GoPerson } from 'react-icons/go'
import { FiMenu } from 'react-icons/fi'
import { authenticate } from '../api'
import { useAuth } from './Auth'
import { Maybe, just } from '../util'
import { Mobile } from './MediaQuery'
import { useSidebar } from './useSidebar'
import { useActiveList } from './useActiveList'

export default function MenuBar(){
  const auth = useAuth()
  const { toggleSidebar } = useSidebar()

  // Automatically login user if sessionKey is present
  useEffect(() => {
    if(auth.status === "loggedIn") return
    const msessionKey = getCookie("sessionKey")
    if(!msessionKey) return

    async function updateContext(sessionKey: string) {
      const userDetails = await authenticate(sessionKey)
      auth.login(userDetails)
    }

    updateContext(msessionKey)
  }, [auth])

  const { activeList } = useActiveList()

  return (
  <div
   className="bg-zinc-50 w-full flex justify-between px-4 text-xl py-1 
             border-b border-zinc-300"
  >

  { /* Left */ }
  <div
  className="flex"
  >
  { /* TODO display hamburger icon on media query breakpoint */ }
  <Mobile>
  <>
  <div 
    className="flex items-center cursor-pointer text-blue-600
               hover:text-blue-800"
    onClick={toggleSidebar}
  >
  <FiMenu/> 
  </div>

  { activeList && <p className='ml-3'> { activeList.name } </p> }
  </>
  </Mobile> 
  </div>


  { /* Right */ }
  <div 
  className="flex space-x-6 items-center"
  >
  <div 
  onClick={login}
  className="flex items-center space-x-1 cursor-pointer hover:underline 
             underline-offset-1 text-blue-600 hover:text-blue-800"
  >
  <GoPerson />
  <p>{ auth.status === "loggedOut" ? "Logga in" : auth.userDetails!.name }</p>
  </div>

  <div className="flex items-center space-x-1 cursor-pointer hover:underline 
                  underline-offset-1 text-blue-600 hover:text-blue-800"
  >
  <FaCog />
  <p>Inställningar</p>
  </div>

  </div>
  </div>
  )

  

  async function login(){
    if (auth.status === "loggedIn") return
    const mcookie = getCookie("sessionKey")
    const msessionKey = mcookie ? mcookie : await createNewSession()
    if(!msessionKey) return 
    const userDetails = await authenticate(msessionKey)
    auth.login(userDetails) 
  }     
  
  async function createNewSession(): Promise<Maybe<string>>{
    const win = window.open("https://lista.fredin.org/server/login", "_blank", "toolbar=0,location=0,menubar=0")
    let msessionKey: Maybe<string>;
    window.addEventListener("message", event => {
      if(event.origin !== "https://lista.fredin.org") return
      msessionKey = just(event.data)
    })
    return new Promise(resolve => {
      const interval = setInterval(() => {
        if (win?.closed) resolve(undefined)
        if(!msessionKey) return
        resolve(msessionKey)
        clearInterval(interval)
        win?.close()
      }, 500)
   })
  }
}

function getCookie(key: string): Maybe<string> {
  var match = document.cookie.match(new RegExp('(^| )' + key + '=([^;]+)'));
  return match ? just(match[2]) : undefined;
}



