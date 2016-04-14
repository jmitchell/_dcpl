Require Import Coq.Numbers.BinNums.
Require Import Coq.Lists.List.
Require Import Coq.Vectors.Vector.


Module TerseAbstractSyntax.     (* Based on Fig 2.12 *)
  Inductive intLit : Type :=
  | Number : BinNums.Z -> intLit.

  Inductive arithmeticOperator : Type :=
  | Add | Sub | Mul | Div | Rem.

  Inductive relationalOperator : Type :=
  | Lt | Eq | Gt.

  Inductive commandSeq : Type :=
  | CommandSequence : list command -> commandSeq

  with command : Type :=
  | IntVal : intLit -> command
  | Pop
  | Swap
  | ArithOp : arithmeticOperator -> command
  | RelOp : relationalOperator -> command
  | NumGet
  | Select
  | Execute
  | ExecutableSequence : commandSeq -> command.

  Inductive prog : intLit -> Type :=
  | Postfix : forall (numArgs : intLit), commandSeq -> prog numArgs.
End TerseAbstractSyntax.

Import TerseAbstractSyntax.

Module Semantics.
  Module SmallStep.             (* Based on Fig 3.3 *)

    Module Domains.
      Inductive value : Type :=
      | IntLit : intLit -> value
      | CommandSeq : commandSeq -> value.

      Definition stack : Type := list value.

      Definition literalOnTop (s : stack) : Prop :=
        match s with
          | List.nil => False
          | h :: _ => match h with
                       | IntLit _ => True
                       | _ => False
                     end
        end.

      Inductive finalStack : stack -> Type :=
      | FinalStack : forall (s : stack) {pf : literalOnTop s}, finalStack s.

      Definition inputs : nat -> Type := Vector.t intLit.

      Definition ansExp : Type := intLit.
    End Domains.

    (* Properties applicable to SOSs:
         - 'strongly normalizing' or 'terminating'
     *)
    Module SOS.
      Import Domains.

      (* Properties that applicable to configurations:
         - 'reducible'
         - 'irreducible'
         - 'normal form'
       *)
      Inductive configuration : commandSeq -> stack -> Type :=
      | CF : forall (cs : commandSeq) (st : stack), configuration cs st.

      (* TODO: => : see sec 3.2.3 *)
      (* Properties applicable to transition relations:
         - 'transition path'
         - 'deterministic' vs 'nondeterministic'
       *)


      Inductive finalConfiguration : Type :=
      | FC : forall {cs : commandSeq} {st : stack}
               (cf : configuration cs st) {fs : finalStack st},
               finalConfiguration.

      (* TODO: [inputFunction] and [outputFunction] *)

      (* Which is better for [inputFunction]?

           - Use types to disallow unproductive input configurations
           - Allow them, following the book and create a safe interface later
       *)
      (* Fixpoint inputFunction {n : nat} (p : prog (toIntLit n)) (args : inputs n) : configuration _ _ := *)
      (*   match p with *)
      (*       Postfix _ cmds => match n with *)
      (*                            Number n' => if length args = n' *)
      (*                                        then CF cmds args *)
      (*                                        else CF nil nil *)
      (*                        end *)
      (*   end. *)

    End SOS.
  End SmallStep.
End Semantics.
