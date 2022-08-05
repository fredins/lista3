export type Id = string

export type Color = "gray"
                  | "blue"
                  | "yellow"
                  | "red"
                  | "pink"

export type Todo = {
  id: Id
  text: string
  active: boolean
  color: Color
}

export type NewTodo = Omit<Todo, "id">
