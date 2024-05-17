module type Stack = sig
  
  (*[empty]* Cria uma list vazia*)
  val empty: int list
  (*[push n] Coloca o valor n(de tipo inteiro) no topo da pilha.*)
  val push: int -> int list -> int list

  (*[pop] Retira (e descarta) o valor do topo da pilha*)
  val pop: int list -> int list

  (*[dup] Duplica o valor do topo da pilha. Uma pilha com um único valor n passa a ter dois valores n.*)
  val dup: int list -> int list
  
  (*[swp] Troca os dois valores do topo da pilha. Uma pilha com dois valores n e m (n no topo) passa a ter dois valores n e m (m no topo).*)
  val swp: int list -> int list
  
  (*[over] Duplica o segundo valor do topo da pilha. Uma pilha com dois valores n e m (n no topo) passa a ter três valores m, n, m.*)
  val over: int list -> int list
  
  (*[add] Retira os dois valores do topo da pilha, soma-os e coloca o resultado no topo da pilha.*)
  val add: int list -> int list
  
  (*[sub] Retira os dois valores do topo da pilha, subtrai os dois valores do topo da pilha e coloca o resultado no topo da pilha. O valor que estava no topo da pilha é o subtraendo e o valor que estava imediatamente abaixo é o minuendo.*)
  val sub: int list -> int list
  
  (*[mul] Retira os dois valores do topo da pilha, multiplica-os e coloca o resultado no topo da pilha.*)
  val mul: int list -> int list
  
  (*[div] Retira os dois valores do topo da pilha, divide-os e coloca o resultado no topo da pilha. O valor que estava no topo da pilha é o divisor e o valor que estava imediatamente abaixo é o dividendo.*)
  val div: int list -> int list
  
  (*[cmp] Retira os dois valores do topo da pilha, compara-os e coloca o resultado da comparação no topo da pilha. O resultado da comparação é um inteiro que indica se os valores são iguais (1) ou diferentes (0).*)
  val cmp: int list -> int list
  
  (*[return] Termina a execução do programa e devolve o valor no topo da pilha como resultado. A pilha só pode ter um valor no topo quando esta instrução é executada ou o interpretador termina em erro.*)
  val return: int list -> int
end
module MyStack: Stack = struct

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
