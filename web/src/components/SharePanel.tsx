import { useQueryClient } from "@tanstack/react-query";
import { map } from "ramda";
import { useEffect, useMemo, useState } from "react";
import { ClassName, Maybe } from "../util";
import { useActiveList } from "./useActiveList";


export default function SharePanel( { className } : ClassName ){
  const queryClient = useQueryClient()
  const lists: Maybe<List[]> = queryClient.getQueryData(["lists"])
  if (!lists) throw new Error("SharePanel: lists undefined")
  const { activeList } = useActiveList()
  useEffect(
    () => { if (activeList) setShareList(JSON.stringify(activeList)) }, 
    [activeList]
  )
  const [ shareList, setShareList ] = useState<Maybe<string>>()
  
  const listOptions = useMemo(() => (
    map<List, JSX.Element>(list => (
      <option key={list.id} value={JSON.stringify(list)}>{list.name}</option>
    ), lists)
    ), [lists]
  )
  
  return (
  <div className={`border-b border drop-shadow-sm ${className}`}>
  <div
  className="bg-zinc-100 h-8 border-b px-2 flex flex-col justify-center text-lg"
  >
  Bjud in medlem
  </div>
  <form className="p-2 text-lg space-y-3">
  <div className="space-y-0">
  <label htmlFor="lista-select">Lista </label>
  <select 
    required={true}
    className="border border-zinc-300 rounded-sm drop-shadow-sm outline-none
               bg-white block pl-2 py-[3px]" 
    value={shareList} 
    onChange={e => setShareList(e.target.value)}
  >
  <option value="">-- Välj lista --</option>
  { listOptions }
  </select>
  </div>
  <div className="space-y-0">
  <label htmlFor="email-input">Mejladdress </label>
  <input 
   id="email-input"
   className="block px-2 text-lg border border-zinc-300 outline-none
              w-64"
   type='email'
   required={true}
  /> 
  </div>
  <div className="flex justify-end">
  <button
    className="px-2 border border-zinc-300 rounded-sm drop-shadow-sm 
               float-right"
  > Dela
  </button>
  </div>
  </form>
  </div>
  )
}
