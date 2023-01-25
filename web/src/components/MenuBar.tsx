import { useEffect } from 'react'
import { FaCog } from 'react-icons/fa'
import { BsPersonXFill, BsPersonCheckFill } from 'react-icons/bs'
import { FiMenu } from 'react-icons/fi'
import { authenticate } from '../api'
import { useAuth } from './Auth'
import { Maybe, just } from '../util'
import { Desktop, Mobile } from './MediaQuery'
import { useSidebar } from './useSidebar'
import { useActiveList } from './useActiveList'

export default function MenuBar(){
  const auth = useAuth()
  const { toggleSidebar } = useSidebar()

  // Automatically login user if sessionKey is present
  useEffect(() => {
    if(auth.isLoggedIn) return
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
   className="bg-zinc-50 w-full flex justify-between px-4 text-3xl md:text-xl py-1 
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

  { activeList && <p className='ml-3 text-2xl'> { activeList.name } </p> }
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
  { auth.isLoggedIn ? <BsPersonCheckFill /> : <BsPersonXFill /> }
  <Desktop>
  <p>{ auth.isLoggedIn ? auth.userDetails!.name : "Logga in"}</p>
  </Desktop>
  </div>

  <div className="flex items-center space-x-1 cursor-pointer hover:underline 
                  underline-offset-1 text-blue-600 hover:text-blue-800"
  >
  <FaCog />
  <Desktop>
  <p>Inställningar</p>
  </Desktop> 
  </div>

  </div>
  </div>
  )

  

  async function login(){
    if (auth.isLoggedIn) return
    const mcookie = getCookie("sessionKey")
    const msessionKey = mcookie ? mcookie : await createNewSession()
    if(!msessionKey) return 
    const userDetails = await authenticate(msessionKey)
    auth.login(userDetails) 
  }     
  
  async function createNewSession(): Promise<Maybe<string>>{
    const win = window.open("https://server-lista.fredin.org/server/login", "_blank", "toolbar=0,location=0,menubar=0")
    let msessionKey: Maybe<string>;
    window.addEventListener("message", event => {
      if(event.origin !== "https://server-lista.fredin.org") return
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



