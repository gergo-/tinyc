/*
 * Based on:
 * https://www.iro.umontreal.ca/~felipe/IFT2030-Automne2002/Complements/tinyc.c,
 * Copyright (C) 2001 by Marc Feeley, All Rights Reserved.
 *
 * This Prolog version by Gergö Barany <gergo@tud.at>, 2023. All rights unclear.
 */

:- set_prolog_flag(double_quotes, chars).

/*---------------------------------------------------------------------------*/

/* Lexer. */

tokens([]) -->
    [].
tokens(Tokens) -->
    ([' '] | ['\n']),
    tokens(Tokens).
tokens([Symbol | Tokens]) -->
    [Symbol],
    { member(Symbol, ['{', '}', '(', ')', +, -, <, ;, =]) },
    tokens(Tokens).
tokens([int(Int) | Tokens]) -->
    int(Int),
    tokens(Tokens).
tokens([Keyword | Tokens]) -->
    chars(Chars),
    { member(Chars, ["do", "else", "if", "while"]) },
    { atom_chars(Keyword, Chars) },
    tokens(Tokens).
tokens([id(Id) | Tokens]) -->
    id(Id),
    tokens(Tokens).

chars([]) -->
    [].
chars([C | Cs]) -->
    [C],
    chars(Cs).

int(Int) -->
    { Digits = [_|_] },
    digits(Digits),
    { number_chars(Int, Digits) }.

digits([D | Ds]) -->
    [D],
    { member(D, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) },
    digits(Ds).
digits([]) -->
    [].

id(Name) -->
    [Name],
    { member(Name, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]) }.

chars_tokens(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars).


/*---------------------------------------------------------------------------*/

/* Parser. */

program(program(S)) -->
    statement(S).
statement(if(Condition, TrueBody)) -->
    [if],
    paren_expr(Condition),
    statement(TrueBody).
statement(if(Condition, TrueBody, FalseBody)) -->
    [if],
    paren_expr(Condition),
    statement(TrueBody),
    [else],
    statement(FalseBody).
statement(while(Condition, Body)) -->
    [while],
    paren_expr(Condition),
    statement(Body).
statement(do_while(Body, Condition)) -->
    [do],
    statement(Body),
    [while],
    paren_expr(Condition),
    [;].
statement(statements(Statements)) -->
    ['{'],
    statements(Statements),
    ['}'].
statement(expr_statement(Expr)) -->
    expr(Expr),
    [;].
statement(skip) -->
    [;].

paren_expr(Expr) -->
    ['('],
    expr(Expr),
    [')'].

expr(Expr) -->
    test(Expr).
expr(id(Id) = Expr) -->
    [id(Id)],
    [=],
    expr(Expr).

test(Expr) -->
    sum(Expr).
test(X < Y) -->
    sum(X),
    [<],
    sum(Y).

sum(Sum) -->
    term(X),
    sum_rest(X, Sum).

sum_rest(Accumulator, Result) -->
    sum_op(Op),
    term(Term),
    { Accumulator1 =.. [Op, Accumulator, Term] },
    sum_rest(Accumulator1, Result).
sum_rest(Accumulator, Accumulator) -->
    \+ sum_op(_).

sum_op(Op) -->
    [Op],
    { member(Op, [+, -]) }.

term(id(Id)) -->
    [id(Id)].
term(int(Int)) -->
    [int(Int)].
term(Expr) -->
    paren_expr(Expr).

statements([]) -->
    [].
statements([S | Ss]) -->
    statement(S),
    statements(Ss).

tokens_ast(Tokens, AST) :-
    phrase(program(AST), Tokens).


/*---------------------------------------------------------------------------*/

/* Code generator. */

:- dynamic here/1.

code(id(Id)) -->
    g(ifetch),
    id_index(Id).
code(int(Int)) -->
    g(ipush),
    g(Int).
code(X + Y) -->
    code(X),
    code(Y),
    g(iadd).
code(X - Y) -->
    code(X),
    code(Y),
    g(isub).
code(X < Y) -->
    code(X),
    code(Y),
    g(ilt).
code(id(Id) = Expr) -->
    code(Expr),
    g(istore),
    id_index(Id).
code(if(Condition, TrueBody)) -->
    code(Condition),
    g(jz),
    hole(JumpToEnd),
    code(TrueBody),
    { fix(JumpToEnd) }.
code(if(Condition, TrueBody, FalseBody)) -->
    code(Condition),
    g(jz),
    hole(JumpToFalse),
    code(TrueBody),
    g(jmp),
    hole(JumpToEnd),
    { fix(JumpToFalse) },
    code(FalseBody),
    { fix(JumpToEnd) }.
code(while(Condition, Body)) -->
    { here(Start) },
    code(Condition),
    g(jz),
    hole(Break),
    code(Body),
    g(jmp),
    fix_loop(Start),
    { fix(Break) }.
code(do_while(Body, Condition)) -->
    { here(Start) },
    code(Body),
    code(Condition),
    g(jnz),
    fix_loop(Start).
code(skip) -->
    [].
code(statements([])) -->
    [].
code(statements([Stmt | Stmts])) -->
    code(Stmt),
    code(statements(Stmts)).
code(expr_statement(Expr)) -->
    code(Expr),
    g(ipop).
code(program(P)) -->
    code(P),
    [halt].


g(Item) -->
    { retract(here(Pos)) },
    [Item],
    { NewPos is Pos + 1 },
    { asserta(here(NewPos)) }.

id_index(Id) -->
    { atom_codes(Id, [CharCode]) },
    { VarNumber is CharCode - 0'a },
    g(VarNumber).

hole(src_offset(Src, Offset)) -->
    { here(Src) },
    g(Offset).

fix(src_offset(Src, Offset)) :-
    here(Dst),
    Offset is Dst - Src + 1.

fix_loop(Start) -->
    { here(End) },
    g(Offset),
    { Offset is Start - End + 1 }.

ast_code(AST, Code) :-
    retractall(here(_)),
    asserta(here(0)),
    phrase(code(AST), CodeList),
    retractall(here(_)),
    Code =.. [code | CodeList].


/*---------------------------------------------------------------------------*/

/* Virtual machine. */

:- dynamic globals/1.


run(Code, Globals) :-
    globals(Globals),
    run(Code, 0, [], Globals), !.

run(Code, PC, Stack, Globals) :-
    opcode(Code, PC, Opcode),
    has_arg(Opcode),
    inline_arg(Code, PC, Arg),
    instr_with_arg(Opcode, Arg, Stack, NewStack, Globals),
    NewPC is PC + 2,
    !,
    run(Code, NewPC, NewStack, Globals).
run(Code, PC, Stack, Globals) :-
    opcode(Code, PC, Opcode),
    is_stack(Opcode),
    stack_instr(Opcode, Stack, NewStack),
    NewPC is PC + 1,
    !,
    run(Code, NewPC, NewStack, Globals).
run(Code, PC, Stack, Globals) :-
    opcode(Code, PC, Opcode),
    is_jump(Opcode),
    inline_arg(Code, PC, Arg),
    jump_instr(Opcode, Arg, Stack, NewStack, JumpOffset),
    NewPC is PC + JumpOffset,
    !,
    run(Code, NewPC, NewStack, Globals).
run(Code, PC, _Stack, _Globals) :-
    opcode(Code, PC, Opcode),
    Opcode = halt.

opcode(Code, PC, Opcode) :-
    I1 is PC + 1,
    arg(I1, Code, Opcode).

inline_arg(Code, PC, Arg) :-
    I2 is PC + 2,
    arg(I2, Code, Arg).

has_arg(ifetch).
has_arg(istore).
has_arg(ipush).

instr_with_arg(ifetch, Index, Stack, [Value | Stack], Globals) :-
    get_global(Globals, Index, Value).
instr_with_arg(istore, Index, [Value | Stack], [Value | Stack], Globals) :-
    set_global(Globals, Index, Value).
instr_with_arg(ipush, Value, Stack, [Value | Stack], _Globals).

is_stack(ipop).
is_stack(iadd).
is_stack(isub).
is_stack(ilt).

stack_instr(ipop, [_Value | Stack], Stack).
stack_instr(iadd, [B, A | Stack], [Sum | Stack]) :-
    Sum is A + B.
stack_instr(isub, [B, A | Stack], [Diff | Stack]) :-
    Diff is A - B.
stack_instr(ilt, [B, A | Stack], [Result | Stack]) :-
    (   A < B
    ->  Result = 1
    ;   Result = 0 ).

is_jump(jmp).
is_jump(jz).
is_jump(jnz).

jump_instr(jmp, Offset, Stack, Stack, Offset).
jump_instr(jz, Offset, [Value | Stack], Stack, FinalOffset) :-
    (   Value = 0
    ->  FinalOffset = Offset
    ;   FinalOffset = 2 ).
jump_instr(jnz, Offset, [Value | Stack], Stack, FinalOffset) :-
    (   Value \= 0
    ->  FinalOffset = Offset
    ;   FinalOffset = 2 ).


globals(Globals) :-
    length(Values, 26),
    maplist(=(0), Values),
    Globals =.. [globals | Values].

get_global(Globals, Index, Value) :-
    Index1 is Index + 1,
    arg(Index1, Globals, Value).

set_global(Globals, Index, Value) :-
    Index1 is Index + 1,
    nb_setarg(Index1, Globals, Value).


/*---------------------------------------------------------------------------*/

/* Main program. */

:- initialization(main).

main :-
    read_chars(Chars),
    compile(Chars, Code),
    run(Code, Globals),
    print_globals(Globals),
    halt.

read_chars(Chars) :-
    prompt(_, ''),
    read_stream_to_codes(user_input, Codes),
    atom_codes(A, Codes),
    atom_chars(A, Chars).

compile(Chars, Code) :-
    chars_tokens(Chars, Tokens),
    tokens_ast(Tokens, AST),
    ast_code(AST, Code),
    !.

print_globals(Globals) :-
    % failure-driven loop
    arg(Index, Globals, Value),
    Value \= 0,
    NameCode is 0'a + Index - 1,
    atom_codes(Name, [NameCode]),
    format('~w = ~w~n', [Name, Value]),
    fail.
print_globals(_Globals).
