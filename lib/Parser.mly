%{
    open Ast
%}

%token EOF LAMBDA
%token <string> NAME
%token <string> VAR
%token DOT EQUAL LET
%token LPAR RPAR

%nonassoc LAMBDA
%right APPLICATION
%left VAR  LPAR NAME
%left LASSOC

%start <ast_expr list> main

%% 

main: 
    | letop+ EOF            { $1 }
    | ast EOF               { [Ast($1)] }
    | EOF                   {  []  }
letop: 
    | LET NAME EQUAL ast    { Let ($2, $4) }
ast: 
    | VAR                   { Variable $1 }
    | NAME                  { Substitution $1 }
    | LAMBDA VAR DOT ast  
      %prec APPLICATION     { Abstraction ($2, $4) }
    | ast ast %prec LASSOC  { Application ($1, $2) }
    | LPAR ast RPAR         { $2 }