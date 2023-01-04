import { ClassName } from "../util";


export default function InvitationsPanel({ className } : ClassName){
  return (
  <div className={`border-b border drop-shadow-sm ${className}`}>
  <div
  className="bg-zinc-100 h-8 border-b px-2 flex flex-col justify-center text-lg"
  >
  Medlemsförfrågningar
  </div>
  <div className="p-2">Inga förfrågningar</div>
  </div>
  )
}
