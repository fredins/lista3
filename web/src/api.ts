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

const server = "http://dev.fredin.org"
const privateServer = `${server}/private`

async function authenticate(sessionKey : Id): Promise<UserDetails> {
  const res = await fetch(`${server}/authenticate?sessionKey=${sessionKey}`)
  return res.json()
}

async function fetchLists(sessionKey : Id): Promise<[List]> {
  const res = await fetch(`${privateServer}/lists?sessionKey=${sessionKey}`)
  return res.json()
}

async function createList(name: string, sessionKey : Id) {
  const { status, ok } = await fetch(`${privateServer}/newList?name=${name}&sessionKey=${sessionKey}`)
  if (status === 412){
    throw new Error(`List with name ${name} already exists ` + status)
  }
  if (!ok){
    throw new Error(`Error status: ` + status)
  }
}

async function fetchAllTodos(listId: Id, sessionKey: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}&sessionKey=${sessionKey}`)
  return res.json()
}

async function fetchActiveTodos(listId: Id, sessionKey: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}&active=True?sessionKey=${sessionKey}`)
  return res.json()
}

async function fetchCompletedTodos(listId: Id, sessionKey: Id): Promise<Todo[]> {
  const res = await fetch(`${privateServer}/todos?listId=${listId}&active=False?sessionKey=${sessionKey}`)
  return res.json()
}

async function createNewTodo(newTodo: NewTodo, sessionKey : Id): Promise<Response> {
  return fetch(`${privateServer}/newTodo?sessionKey=${sessionKey}`,
    {
      method: 'POST', 
      headers: { 
        'Content-Type': "application/json",
        Origin: "127.0.0.1"
      }, 
      body: JSON.stringify(newTodo)
    }
  )
}

async function updateTodo(todo: Todo, sessionKey : Id): Promise<Response> {
  return fetch(`${privateServer}/updateTodo?sessionKey=${sessionKey}`,
    {
      method: 'POST', 
      headers: { 
        'Content-Type': "application/json",
        Origin: "127.0.0.1"
      }, 
      body: JSON.stringify(todo)
    }
  )
}


