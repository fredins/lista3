import { Todo, NewTodo } from "../types/DB"
import { ServerURL } from './URIs'


export async function getAllTodos(): Promise<Todo[]> {
  return fetch(ServerURL + "/todos").then(xs => xs.json())
}

export async function getActiveTodos(): Promise<Todo[]> {
  return fetch(ServerURL + "/todos?active=true").then(xs => xs.json())
}

export async function getCompletedTodos(): Promise<Todo[]> {
  return fetch(ServerURL + "/todos?active=false").then(xs => xs.json())
}

export async function postNewTodo(newTodo: NewTodo): Promise<Response> {
  return fetch(ServerURL + "/newTodo",
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

export async function updateTodo(todo: Todo): Promise<Response> {
  return fetch(ServerURL + "/updateTodo",
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
