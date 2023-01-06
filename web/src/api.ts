// Production
// const server = "https://lista.fredin.org/server"
// Development
const server = ""

const privateServer = `${server}/private`

async function authenticate(sessionKey : Id): Promise<UserDetails> {
  const res = await fetch(`${server}/authenticate?sessionKey=${sessionKey}`)
  return res.json()
}

async function fetchLists(): Promise<[List]> {
  const res = await fetch(`${privateServer}/lists`, {
    credentials: 'include'
  })
  return res.json()
}

async function createList(name: string): Promise<List> {
  const res = await fetch(`${privateServer}/newList?name=${name}`, {
    credentials: 'include'
  })
  if (res.status === 412){
    throw new Error(`List with name ${name} already exists ` + res.status)
  }
  if (!res.ok){
    throw new Error(`Error status: ` + res.status)
  }
  return res.json()
}

async function deleteList({ id } : List): Promise<Response> {
  return fetch(`${privateServer}/deleteList?listId=${id}`) 
}

async function fetchAllTodos(listId: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}`, {
    credentials: 'include'
  })
  return res.json()
}

async function fetchActiveTodos(listId: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}&active=True`, {
    credentials: 'include'
  })
  return res.json()
}

async function fetchCompletedTodos(listId: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}&active=False`, {
    credentials: 'include'
  })
  return res.json()
}

async function createTodo(newTodo: NewTodo): Promise<Response> {
  return fetch(`${privateServer}/newTodo`,
    {
      method: 'POST', 
      headers: { 
        'Content-Type': "application/json",
      }, 
      body: JSON.stringify(newTodo),
      credentials: 'include'
    }
  )
}

async function updateTodo(todo: Todo): Promise<Response> {
  return fetch(`${privateServer}/updateTodo`,
    {
      method: 'POST', 
      headers: { 
        'Content-Type': "application/json",
        Origin: "127.0.0.1"
      }, 
      body: JSON.stringify(todo),
      credentials: 'include'
    }
  )
}


async function deleteTodo({ id }: Todo): Promise<Response> {
  return fetch(`${privateServer}/deleteTodo?todoId=${id}`,{
      credentials: 'include'
    }
  )
}



async function createInvitation(listId : Id, email : string): Promise<Response>{
  return fetch(`${privateServer}/newInvitation?listId=${listId}&email=${email}`, {
    credentials: 'include'
  })
}


async function fetchInvitations(): Promise<InvitationDetails[]>{
  const res = await fetch(`${privateServer}/invitations`, {
    credentials: 'include'
  })

  return res.json()
}


async function acceptInvitation(invitationsId : Id): Promise<List>{
  const res = await fetch(`${privateServer}/answerInvitation?invitationsId=${invitationsId}&accept=true`, {
    credentials: 'include'
  })
  return res.json()
}

async function rejectInvitation(invitationsId : Id): Promise<Response>{
  return fetch(`${privateServer}/answerInvitation?invitationsId=${invitationsId}&accept=false`, {
    credentials: 'include'
  })
}

export {
  authenticate,
  fetchLists,
  createList,
  fetchAllTodos,
  fetchActiveTodos,
  fetchCompletedTodos,
  createTodo,
  updateTodo,
  deleteList,
  createInvitation,
  fetchInvitations,
  acceptInvitation,
  rejectInvitation,
  deleteTodo,
}
