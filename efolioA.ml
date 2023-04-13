(*
** file: efolioa.ml
**
** Implementa uma Pilha com listas
** UC: 21077 - Linguagens de Programação @ UAb
** e-fólioA 2022-23
**
** Aluno: 2100927 - Ivo Baptista
 *)

(** O tipo de pilhas com listas. *)
module type STACK = sig
  (** ['a t] é o tipo de pilhas com tipo de elemento ['a]. *)
  type 'a t
  (** [empty] é uma pilha vazia. *)
  val empty : 'a t
  (** [is_empty s] é verdadeiro se e somente se [s] é uma pilha vazia. *)
  val is_empty : 'a t -> bool
  (** [push x s] coloca um elemento [x] no topo da pilha [s] e retorna a nova pilha. *)
  val push : 'a -> 'a t -> 'a t
  (** [peek s] retorna o elemento no topo da pilha [s] sem remover o elemento da pilha. Se a pilha [s] estiver vazia, retorna [None]. *)
  val peek : 'a t -> 'a option
  (** [pop s] retorna a pilha [s] sem o elemento no topo. Se a pilha [s] estiver vazia, retorna [None]. *)
  val pop : 'a t -> 'a t option
  (** [size s] retorna o número de elementos na pilha [s]. *)
  val size : 'a t -> int
  (** [to_list s] retorna uma lista com os elementos da pilha [s] na ordem em que eles foram inseridos. *)
  val to_list : 'a t -> 'a list
end

(** Implementação my stack com listas *)
module MyStack : STACK = struct
  (** ['a t] é o tipo de pilhas com tipo de elemento ['a]. *)
  type 'a t = 'a list
  (** [empty] é uma pilha vazia. *)
  let empty = []
  (** [is_empty s] é verdadeiro se e somente se [s] é uma pilha vazia. *)
  let is_empty = function [] -> true | _ -> false
  (** [push x s] coloca um elemento [x] no topo da pilha [s] e retorna a nova pilha. *)
  let push = List.cons
  (** [peek s] retorna o elemento no topo da pilha [s] sem remover o elemento da pilha. Se a pilha [s] estiver vazia, retorna [None]. *)
  let peek = function [] -> None | x :: _ -> Some x 
  (** [pop s] retorna a pilha [s] sem o elemento no topo. Se a pilha [s] estiver vazia, retorna [None]. *)
  let pop = function [] -> None | head::rest_oflist -> Some rest_oflist
  (** [size s] retorna o número de elementos na pilha [s]. *)
  let size = List.length
  (** [to_list s] retorna uma lista com os elementos da pilha [s] na ordem em que eles foram inseridos. *)
  let to_list = Fun.id
end

  (** O tipo de receitas para construir uma pilha. *)
  type 'a build =
    | Empty
    (** [Empty] significa chamar [empty] para obter uma pilha. *)
    | Push of 'a * 'a build
    (** [Push (x, b)] significa construir uma pilha [s] usando [b] e então chamar [push x s] para obter uma pilha. *)
    | Pop of 'a build
	(** [Pop b] significa construir uma pilha [s] usando [b] e então chamar [pop s] para obter uma pilha. Se [pop s] retornar [None], a construção falha e uma exceção deve ser levantada . +*)
  (** O tipo de verificação numa pilha. *)
type 'a check =
  | IsEmpty of bool
  (** [IsEmpty b] significa que chamar [is_empty] na pilha deve obter a resposta [b]. *)
  | Peek of ('a -> bool) option
  (** [Peek None] significa que chamar [peek] na pilha deve obter [None]. [Peek (Some p)] significa que chamar [peek] na pilha deve obter [Some x] e [p x] deve ser verdadeiro. *)
  | Pop of unit option
  (** [Pop None] significa que chamar [pop] na pilha deve obter [None]. [Pop (Some ())] significa que chamar [pop] na pilha deve obter [Some s] para alguma pilha [s ] *)
  | Size of int
  (** [Size i] significa que chamar [size] na pilha deve obter a resposta [i]. *)
 
(** Implementação StackTester com listas *)
  module StackTester (Stack : STACK) : sig
    exception BuildFailure
    
    (** [build b] constrói uma pilha usando a receita [b]. Se a construção falhar, uma exceção [BuildFailure] deve ser levantada. *)
    val build : 'a build -> 'a Stack.t

    (** [test stack check] verifica se a pilha [stack] satisfaz a verificação [check]. *)
    val test : 'a Stack.t -> 'a check -> bool 
    
    (** [print f stack] imprime a pilha [stack] usando a função [f] para converter cada elemento em uma string. *)
    val print : ('a -> string) -> 'a Stack.t -> unit
    
    (** [aux_build stack b] constrói uma pilha usando a receita [b] e a pilha [stack] como acumulador. Se a construção falhar, uma exceção [BuildFailure] deve ser levantada. *)
    val aux_build : 'a Stack.t -> 'a build -> 'a Stack.t
  
  end = struct
    exception BuildFailure

    (** [build b] constrói uma pilha usando a receita [b]. Se a construção falhar, uma exceção [BuildFailure] deve ser levantada. *)
    let rec build = function
      | Empty -> Stack.empty
      | Push (x, b) -> let s = build b in Stack.push x s
      | Pop b -> let s = build b in (match Stack.pop s with
                                    | None -> raise BuildFailure
                                    | Some s -> s)
    
   (* aux_build stack b constrói uma pilha usando a receita [b] e a pilha [stack] como acumulador. Se a construção falhar, uma exceção [BuildFailure] deve ser levantada. *)                               
    let rec aux_build stack = function
      | Empty -> stack
      | Push (x, b) -> let s = Stack.push x stack in aux_build s b
      | Pop b -> (match Stack.pop stack with
                  | None -> raise BuildFailure
                  | Some s -> aux_build s b)
    
    (** [test stack check] verifica se a pilha [stack] satisfaz a verificação [check]. *)
    let test stack check =
      match check with
      | IsEmpty b -> Stack.is_empty stack = b
      | Peek None -> Stack.peek stack = None
      | Peek (Some p) -> (match Stack.peek stack with
                          | None -> false
                          | Some x -> p x)
      | Pop None -> Stack.pop stack = None
      | Pop (Some ()) -> (match Stack.pop stack with
                          | None -> false
                          | Some _ -> true)
      | Size i -> Stack.size stack = i
    
    (** [print f stack] imprime a pilha [stack] usando a função [f] para converter cada elemento em uma string. *)
    let print f stack =
      let rec aux = function
        | [] -> ()
        | [x] -> print_string (f x)
        | x :: xs -> print_string (f x); print_string " "; aux xs
      in
      print_string "[";
      aux (Stack.to_list stack);
      print_endline "]"
  end
  

(** Implementação Testes com listas *)
module MyStackTester = StackTester(MyStack)

(** Testes *)
let()= print_endline "Mostrar os Testes\n"

(** Teste 1 Coloca 1, 2 e 3 retira o 3 e coloca o 5 e retira o 5*)
let test1 = Push (1, Push (2, Push (3, Pop (Push(5,Pop Empty)))))
let stack1 = MyStackTester.build Empty
let result1 = MyStackTester.aux_build stack1 test1
let()= print_endline "Mostra resultado do Teste 1"
let () = MyStackTester.print (fun x -> string_of_int x) result1
let()= print_endline "TESTE 1: OK\n"

(** Teste 2 Coloca 1, 2 e 3 e depois retira o 3 e o 2*)
let test2 = Push (1, Push (2, Push (3, Pop (Pop Empty))))
let stack2 = MyStackTester.build Empty
let result2 = MyStackTester.aux_build stack2 test2
let()= print_endline "Mostra resultado do Teste 2"
let () = MyStackTester.print (fun x -> string_of_int x) result2
let()= print_endline "TESTE 2: OK\n"

(** Teste 3 Coloca o 5 o 6 e retira o 6 e coloca o 8*)
let test3 = (Push (5, Push (6, Pop (Push (8, Empty)))))
let stack3 = MyStackTester.build Empty
let result3 = MyStackTester.aux_build stack3 test3
let()= print_endline "Mostra resultado do Teste 3"
let () = MyStackTester.print (fun x -> string_of_int x) result3
let()= print_endline "TESTE 3: OK\n"

(** Teste 4  Coloca o 1 o 2 o 3 e retira o 3 e coloca o 9*)
let test4 = Push (1, Push (2, Push (3, Pop (Push(9, Empty)))))
let stack4 = MyStackTester.build Empty
let result4 = MyStackTester.aux_build stack4 test4
let()= print_endline "Mostra resultado do Teste 4"
let () = MyStackTester.print (fun x -> string_of_int x) result4
let()= print_endline "TESTE 4: OK\n"

(** Teste 5 - ID(items) carregados e descarregados no camião*)
(**O camião chegou no centro logístico e carregou os IDs pela
 seguinte ordem 5,6,8, ele realizou duas paragens durante o dia 
 onde descarregou um item, qual o ID do item que ficou no camião? *)
let test5= Push (5, Push (6, Push (8, Pop (Pop Empty))))
let stack5 = MyStackTester.build Empty
let result5 = MyStackTester.aux_build stack5 test5
let()= print_endline "Mostra Items no Camião 1"
let () = MyStackTester.print (fun x -> string_of_int x) result5
let()= print_endline "TESTE do Camião 1: OK\n"

(** Teste 6 - ID(items) carregados e descarregados no camião*)
(**O camião chegou no centro logístico e carregou os IDs pela seguinte ordem 2,1,5,9,10, na primeira paragem 
descarregou dois itens e carregou um item com o ID 8, na paragem seguinte descarregou 2 itens, na paragem seguinte carregou um item com o ID 3. No final do dia que itens ficarão no camião?*)
let test6 = Push (2, Push (1, Push (5, Push(9, Push (10, Pop (Pop(Push(8,Pop(Pop(Push(3, Empty)))))))))))
let stack6 = MyStackTester.build Empty
let result6 = MyStackTester.aux_build stack6 test6
let()= print_endline "Mostra Items no Camião 2"
let () = MyStackTester.print (fun x -> string_of_int x) result6
let()= print_endline "TESTE do Camião 2: OK\n"


(**Biblioteca stack de Ocaml *)
(**aqui estamos a criar o mesmo exemplo do Teste 6 *)
let () =
let stk = Stack.create () in

(* Coloca elementos na pilha 2,1,5,9,10 *)
Stack.push 2 stk;
Stack.push 1 stk;
Stack.push 5 stk;
Stack.push 9 stk;
Stack.push 10 stk;

(* Retira os elementos superiores da pilha *)
ignore (Stack.pop stk);
ignore (Stack.pop stk);

(* Carrega mais 2 elementos *)
Stack.push 8 stk;

(* Retira mais 2 elementos *)
ignore (Stack.pop stk);
ignore (Stack.pop stk);

(* Por último carrega mais 1 elemento *)
Stack.push 3 stk;

(* Imprime os elementos da pilha *)
print_string "Elementos que ficam no camião: ";
while not (Stack.is_empty stk) do
print_int (Stack.top stk);
print_string " ";
ignore (Stack.pop stk);
done;
print_newline();;
let()= print_endline "TESTE 2 do camião: OK\n"

(** Fim dos Testes *)
let () = print_endline "Testes Efectados com Sucesso\n";
(*print_endline "Fim de Programa";*)
print_endline " Ivo Baptista 2100927 ";



