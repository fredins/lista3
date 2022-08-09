import { Todo } from './DB'

export type Mode = "normal"
  | "editing"  // click 
  | "selected" // pc: ctrl + click, mobile: right-left swipe
               // marked pc: shift + click, mobile, left-right swipe 

export type TM = { todo: Todo, mode: Mode }

export type Change = "changeTodo"
  | "changeMode"
  | "changeTodoAndMode"
