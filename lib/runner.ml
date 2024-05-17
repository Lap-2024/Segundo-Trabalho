module MyStack = struct

  let empty = []
  
  let push n st:int list = n :: st

  let pop st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | _ :: t -> t

  let dup st:int list = 
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h :: t -> h :: st

  let swp st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> h2 :: h1 :: t
  
  let over st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> h2 :: h1 :: h2 :: t

  let add st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> (h2+h1) :: t

  let sub st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> (h2-h1) :: t

  let mul st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> (h1*h2) :: t

  let div st:int list = 
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> if h1 = 0 then failwith "Division by zero" else (h2/h1) :: t

  let cmp st:int list =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h1 :: [] -> failwith "Unexpected empty stack"
    | h1 :: h2 :: t -> if h1 = h2 then 1 :: t else 0 :: t

  let return st:int =
    match st with
    | [] -> failwith "Unexpected empty stack"
    | h :: t when t = [] -> h
    | h :: t -> failwith "Stack not empty"

end
let run lines label =
  assert false (* COMPLETAR AQUI *)
