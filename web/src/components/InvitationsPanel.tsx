import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { append, filter, isEmpty, map } from "ramda";
import { useMemo } from "react";
import { acceptInvitation, fetchInvitations, rejectInvitation } from "../api";
import { ClassName, Maybe } from "../util";
import { useAuth } from "./Auth";
import InvitationsItem from "./InvitationItem";
import { useActiveList } from "./useActiveList";


type Props = {
  setNewList : (_: string) => void
}

export default function InvitationsPanel({ setNewList, className } 
: Props & ClassName){

  const auth = useAuth()
  const queryClient = useQueryClient()
  const { setActiveList } = useActiveList()

  const { data : invitations } = useQuery<InvitationDetails[]>({
    queryKey: ["invitations"],
    queryFn: fetchInvitations, 
    enabled: auth.isLoggedIn,
  });

  type AcceptInvitationMutationArg = { 
    invitation: InvitationDetails
    prevLists: List[] 
  }

  const acceptInvitationMutation = 
    useMutation<List, Error, AcceptInvitationMutationArg, void>(
      ({ invitation }) => acceptInvitation(invitation.id), {

    onMutate: async ({ invitation, prevLists }) => {
      const optimisticList = { 
        id: invitation.listId,
        name: invitation.listName 
      } 
      queryClient.setQueryData(["lists"], append(optimisticList, prevLists))
      if (invitations) 
        queryClient.setQueryData(["invitations"], 
          filter(i => i.id !== invitation.id, invitations))
      setNewList("")
      setActiveList(optimisticList)
      await queryClient.cancelQueries({ queryKey: ["lists"]})
    },

    onError: (error: Error, { prevLists }) => {
      queryClient.setQueryData(["lists"], prevLists)
      console.log(error.message)
    },

    onSettled: () => {
      queryClient.fetchQuery(["lists"])
    },

    onSuccess: setActiveList
  })

  const rejectInvitationsMutation = useMutation(
    (invitation : InvitationDetails) => rejectInvitation(invitation.id), {
    onMutate: invitation => {
      if (invitations) {
        const xs = filter(i => i.id !== invitation.id, invitations)
        queryClient.setQueryData(["invitations"], xs)
    }}})
  
  const invitationItems : Maybe<JSX.Element[]> = useMemo(
    () => invitations && !isEmpty(invitations) ? map(i => (
      <InvitationsItem 
        invitation={i}
        onAccept={i => acceptInvitationMutation.mutate({
          invitation: i,
          prevLists: queryClient.getQueryData(["lists"]) ?? []
        })}
        onReject={ rejectInvitationsMutation.mutate }
        key={i.id} 
      />
    ), invitations) : undefined
    , [invitations, acceptInvitationMutation, queryClient, rejectInvitationsMutation]
  )

  return (
  <div className={`border-b border drop-shadow-sm ${className}`}>
  <div className="bg-zinc-100 h-8 border-b px-2 flex flex-col justify-center 
                  text-lg"
  >Medlemsförfrågningar
  </div>
  { invitationItems  ? 
  <ul className="divide-y divide-gray-300">{ invitationItems }</ul>
  : 
  <div className="p-2">Inga förfrågningar</div>
  }
  </div>
  )
}
