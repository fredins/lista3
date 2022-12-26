export {
  authenticate,
  fetchLists,
  createList,
  fetchAllTodos,
  fetchActiveTodos,
  fetchCompletedTodos,
  createNewTodo,
  updateTodo,
}

// Production
const server = "https://lista.fredin.org/server"
// Development
// const server = ""

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

async function createList(name: string) {
  const { status, ok } = await fetch(`${privateServer}/newList?name=${name}`, {
    credentials: 'include'
  })
  if (status === 412){
    throw new Error(`List with name ${name} already exists ` + status)
  }
  if (!ok){
    throw new Error(`Error status: ` + status)
  }
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

async function createNewTodo(newTodo: NewTodo): Promise<Response> {
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


