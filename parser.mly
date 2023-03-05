%{
(* Grammar for ILAsm for Menhir *)
%}

(* Symbols *)

%token LBrace RBrace Plus Comma Eq LPar RPar LBrack RBrack 
%token Colon DColon Times Amper Ellipsis Slash Bang

(* Metadata *)

%token <Lexis.meta> Meta

(* Prefixes *)

%token <Lexis.prefix> Prefix

(* Instructions *)

%token <Lexis.instr> Instr

(* Identifiers and literals *)

%token <string> Ident
%token <string> Nat
%token <string> Real
%token <string> QString
%token <string> SQString

(* Virtual tokens *)

%token <string> Space
%token <string> EOL    (* End of Line *)
%token EOF             (* End of File *)

(* Entries *)

%start main
%type <unit> main

%%

(* LR(1) Grammar *)

main:
  EOF {}
