%{
    open Ast
%}

%token EOF LAMBDA
%token <string> NAME
%token <string> VAR
%token DOT EQUAL
%token LPAR RPAR 

%nonassoc LAMBDA LPAR
%left EQUAL
%right APPLICATION
%left VAR
%left NAME
%left LASSOC

%start <expr list> main

%% 

main: 
    | ast EOF              { [Ast($1)] }
    | letop+ EOF           {  $1  }
    | EOF                  {  []  }

letop: 
    | NAME EQUAL ast       { Let ($1, $3) }

ast: 
    | LPAR ast RPAR        { $2 }
    | VAR                  { Variable $1 }
    | NAME                 { Subs $1 }
    | LAMBDA VAR DOT ast  %prec APPLICATION  { Abstraction ($2, $4) }
    | ast ast %prec LASSOC { Application ($1, $2) }