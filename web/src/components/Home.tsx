import { useQuery, useQueryClient } from "@tanstack/react-query";
import { curry, forEach } from "ramda";
import { useState } from "react";
import { fetchAllTodos, fetchLists } from "../api";
import { Maybe } from "../util";
import { useAuth } from "./Auth";
import MenuBar from "./MenuBar";
import MobileHome from "./MobileHome";
import { AdaptiveView } from "./MediaQuery";
import DesktopHome from "./DesktopHome";

export default function Home() {
  const { isLoggedIn } = useAuth()
  const queryClient = useQueryClient()

  const { data : lists } = useQuery<List[]>({
    queryKey: ["lists"],
    queryFn: fetchLists,
    enabled: isLoggedIn,
    onSuccess: curry(forEach<List>)
      (x => queryClient.fetchQuery(["todos", x.id], () => fetchAllTodos(x.id), { 
      cacheTime: 1000*60*20
      }))
  });

  const [editing, setEditing] = useState<Maybe<Id>>()
  const [selected, setSelected] = useState<Id[]>([])
  const [newList, setNewList] = useState("")

  return (
  <div>
  <MenuBar />
  <AdaptiveView 
    desktop={ <DesktopHome
                lists={lists}
                newList={newList}
                setNewList={setNewList}
                editing={editing}
                setEditing={setEditing}
                selected={selected}
                setSelected={setSelected}
              />
    }
    mobile={ <MobileHome
               lists={lists}
               newList={newList}
               setNewList={setNewList}
               editing={editing}
               setEditing={setEditing}
               selected={selected}
               setSelected={setSelected}
             />
    }
  />
  </div>
  )
}
