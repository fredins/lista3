
type Props = {
  invitation : InvitationDetails
  onAccept : (_: InvitationDetails) => void
  onReject : (_: InvitationDetails) => void
}
export default function InvitationsItem({invitation, onAccept, onReject} : Props)
: JSX.Element {
  return (
  <li
    className="p-2"
  >
  <p className="mb-2 font-bold">{invitation.listName}</p>
  <p className="mb-2 text-zinc-500 font-sans">från {invitation.owner}</p>
  <div
    className="flex justify-end space-x-2"
  >
  <button
    className="px-2 border border-zinc-300 rounded-sm drop-shadow-sm"
    onClick={e => { 
      e.preventDefault() 
      onReject(invitation)
    }}
  >
  Avböj
  </button>
  <button 
    className="px-2 border border-zinc-300 rounded-sm drop-shadow-sm"
    onClick={e => { 
      e.preventDefault() 
      onAccept(invitation)
    }}
  >Gå med
  </button>
  </div>

  </li>
  )
}
