Require Import Coq.Numbers.Integer.BigZ.BigZ.
Require Import Coq.Lists.List.
Import ListNotations.

Module TerseAbstractSyntax.     (* Based on Fig 2.12 *)
  Inductive intLit : Type :=
  | Number : bigZ -> intLit.

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

  Inductive prog : Type :=
  | Postfix : intLit -> commandSeq -> prog.
End TerseAbstractSyntax.

Import TerseAbstractSyntax.

Module Semantics.
  Module SmallStep.             (* Based on Fig 3.3 *)

    Module Domains.
      Inductive value : Type :=
      | IntLit : intLit -> value
      | CommandSeq : commandSeq -> value.

      Definition stack : Type := list value.

      Definition v (n : bigZ) : value := IntLit (Number n).

      Inductive finalStack : stack -> Type :=
      | FinalStack : forall (s : stack)
                       {p1 : length s > 1}
                       {p2 : exists n, nth 0 s (v BigZ.zero) = v n},
                       finalStack s.

      Check FinalStack [v BigZ.zero].

      Definition inputs : Type := list intLit.

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
      | CF : forall (c : commandSeq) (s : stack), configuration c s.

      (* TODO: => : see sec 3.2.3 *)
      (* Properties applicable to transition relations:
         - 'transition path'
         - 'deterministic' vs 'nondeterministic'
       *)

      Inductive finalConfiguration : Type :=
      | FC : forall {s : stack} {fs : finalStack s}
               (cf : configuration (CommandSequence []) s),
               finalConfiguration.

      Check FC (CF (CommandSequence [])
                   [v BigZ.zero]).
      Check FC (CF (CommandSequence [])
                   []).         (* TODO: prove this is impossible. *)


      (* TODO: [inputFunction] and [outputFunction] *)

    End SOS.
  End SmallStep.
End Semantics.
