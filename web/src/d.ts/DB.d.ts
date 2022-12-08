type Id = string

type Color = "gray"
           | "blue"
           | "yellow"
           | "red"
           | "green"

type Todo = {
  id     : Id
  text   : string
  active : boolean
  color  : Color
  listId : Id
}

type UserDetails = {
  name  : string
  email : string
}

type List = {
  id   : Id
  name : string
}

type TodosRequest = {
  text   : string
  color  : Color
  listId : Id
}


type NewList = Omit<List, "id">

type NewTodo = Omit<Todo, "id">

type Id_ = { id : Id } 


