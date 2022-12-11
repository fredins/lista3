import { head } from 'ramda'
import { useEffect } from 'react'
import { FaCog } from 'react-icons/fa'
import { GoPerson } from 'react-icons/go'
import { authenticate } from '../api'
import { useAuth } from './Auth'

export default function MenuBar(){
  const auth = useAuth()

  // Automatically login user if sessionKey is present
  useEffect(() => {
    if(auth.status === "loggedIn") return
    const msessionKey = head(getCookie("sessionKey"))
    if(!msessionKey) return

    async function updateContext(sessionKey: string) {
      const userDetails = await authenticate(sessionKey)
      auth.login(userDetails)
    }

    updateContext(msessionKey)
  }, [auth])

  return (
   <div
    className="bg-zinc-50 w-full h-6 flex justify-between px-4 text-sm"
   >

   { /* Left */ }
   <div
   className="flex"
   >
   { /* TODO display hamburger icon on media query breakpoint */ }
   </div>


   { /* Right */ }
   <div 
   className="flex space-x-4 items-center"
   >
   <div 
   onClick={login}
   className="flex items-center space-x-1 cursor-pointer hover:underline 
              underline-offset-1 text-sm text-blue-600 hover:text-blue-800"
   >
   <GoPerson />
   <p>{ auth.status === "loggedOut" ? "Logga in" : auth.userDetails!.name }</p>
   </div>

   <div className="flex items-center space-x-1 cursor-pointer hover:underline 
                   underline-offset-1 text-sm text-blue-600 hover:text-blue-800"
   >
   <FaCog />
   <p>Inställngar</p>
   </div>

   </div>
   </div>
  )


  async function login(){
    if (auth.status === "loggedIn") return

    const msessionKey = head(getCookie("sessionKey"))
    const sessionKey = msessionKey ? msessionKey : await createNewSession()
    const userDetails = await authenticate(sessionKey)
    auth.login(userDetails) 
}    

  async function createNewSession(): Promise<string>{
    const win = window.open("https://dev.fredin.org/login", "_blank", "toolbar=0,location=0,menubar=0")
    return new Promise(resolve => {
      const interval = setInterval(() => {
        if(!win!.closed) return
        const msessionKey = head(getCookie("sessionKey"))
        if(!msessionKey) return
        resolve(msessionKey)
        clearInterval(interval)
      }, 500)
   })
  }
}


function getCookie(key: string): string[] {
  var match = document.cookie.match(new RegExp('(^| )' + key + '=([^;]+)'));
  return match ? [match[2]] : [];
}

