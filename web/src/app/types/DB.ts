export type Id = string

export type Color = "gray"
                  | "blue"
                  | "yellow"
                  | "red"
                  | "green"

export type Todo = {
  id: Id
  text: string
  active: boolean
  color: Color
}

export type NewTodo = Omit<Todo, "id">
