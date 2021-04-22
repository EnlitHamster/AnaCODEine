parser grammar FlexPars;

options
{
    tokenVocab = FlexLex;
}

single_input : NEWLINE | simple_stmt | compound_stmt NEWLINE ;
file_input : ( NEWLINE | stmt )* EOF;
eval_input : testlist NEWLINE* EOF;

decorator : DECORATOR dotted_name ( OPEN_PAREN ( arglist )? CLOSE_PAREN )? NEWLINE ;
decorators : decorator+ ;
decorated : decorators ( classdef | funcdef | async_funcdef ) ;

async_funcdef : ASYNC funcdef;
funcdef : DEF NAME parameters ( ARROW test )? COLON suite;

parameters : OPEN_PAREN ( typedargslist )? CLOSE_PAREN ;
typedargslist
    : ( tfpdef ( ASSIGN test )? ( COMMA tfpdef ( ASSIGN test )? )*
        ( COMMA ( '*' ( tfpdef )? ( COMMA tfpdef ( ASSIGN test )? )* ( COMMA ( '**' tfpdef ( COMMA )? )? )?
        | '**' tfpdef ( COMMA )? )? 
        )?
    | '*' ( tfpdef )? ( COMMA tfpdef ( ASSIGN test )? )* ( COMMA ( '**' tfpdef ( COMMA )? )? )?
    | '**' tfpdef ( COMMA )?
    ) ;
tfpdef : NAME ( COLON test )? ;
varargslist
    : ( vfpdef ( ASSIGN test )? ( COMMA vfpdef ( ASSIGN test )? )*
        ( COMMA ( '*' ( vfpdef )? ( COMMA vfpdef ( ASSIGN test )? )* ( COMMA ( '**' vfpdef ( COMMA )? )? )?
        | '**' vfpdef ( COMMA )? )? 
        )?
    | '*' ( vfpdef )? ( COMMA vfpdef ( ASSIGN test )? )* ( COMMA ( '**' vfpdef ( COMMA )? )? )?
    | '**' vfpdef ( COMMA )?
    ) ;
vfpdef : NAME ;

stmt : simple_stmt | compound_stmt ;
simple_stmt : small_stmt ( SEMI_COLON small_stmt )* ( SEMI_COLON )? NEWLINE;
small_stmt 
    : ( expr_stmt | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | nonlocal_stmt | assert_stmt )
    ;

expr_stmt 
    : testlist_star_expr 
        ( annassign 
        | augassign ( yield_expr | testlist ) 
        | ( ASSIGN ( yield_expr | testlist_star_expr ) )*
        )
    ;

annassign : COLON test ( ASSIGN test )? ;
augassign 
    : ( ADD_ASSIGN 
      | SUB_ASSIGN 
      | MULT_ASSIGN 
      | AT_ASSIGN 
      | DIV_ASSIGN 
      | MOD_ASSIGN 
      | AND_ASSIGN 
      | OR_ASSIGN 
      | XOR_ASSIGN 
      | LEFT_SHIFT_ASSIGN
      | RIGHT_SHIFT_ASSIGN
      | POWER_ASSIGN
      | IDIV_ASSIGN
    ) ;

testlist_star_expr : ( test | star_expr ) ( COLON ( test | star_expr ) )* ( COLON )? ;

del_stmt : DEL exprlist ;
pass_stmt : PASS ;
flow_stmt 
    : BREAK 
    | CONTINUE 
    | RETURN ( testlist )? 
    | RAISE ( test ( FROM test )? )? 
    | yield_expr 
    ;
import_stmt : import_name | import_from ;
import_name : IMPORT dotted_as_names ;
import_from 
    : ( FROM ( ( DOT | ELLIPSIS )* dotted_name | ( DOT | ELLIPSIS )+ ) 
        IMPORT ( '*' | OPEN_PAREN import_as_names CLOSE_PAREN | import_as_names )
    ) ;
import_as_name : NAME ( AS NAME )? ;
dotted_as_name : dotted_name ( AS NAME )? ;
import_as_names : import_as_name ( COMMA import_as_name )* ( COMMA )? ;
dotted_as_names : dotted_as_name ( COMMA dotted_as_name )* ;
dotted_name : NAME ( DOT NAME )* ;
global_stmt : GLOBAL NAME ( COMMA NAME )* ;
nonlocal_stmt : NONLOCAL NAME ( COMMA NAME )* ;
assert_stmt : ASSERT test ( COMMA test )? ;

compound_stmt : if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt ;
async_stmt : ASYNC ( funcdef | with_stmt | for_stmt ) ;
if_stmt : IF test THEN suite ( ELIF test THEN suite )* ( ELSE THEN suite )? ;
while_stmt : WHILE test THEN suite ( ELSE THEN suite )? ;
for_stmt : FOR exprlist IN testlist COLON suite ( ELSE THEN suite )? ;
try_stmt 
    : ( TRY THEN suite 
        ( ( except_clause THEN suite )+ 
          ( ELSE THEN suite)?
          ( FINALLY THEN suite )?
          | FINALLY THEN suite
        )
    ) ;
with_stmt : WITH with_item ( COMMA with_item )* THEN suite ;
with_item : test ( AS expr )? ;
except_clause : EXCEPT ( test ( AS NAME )? )? ;
suite : simple_stmt | NEWLINE INDENT stmt+ DEDENT ;

test : or_test ( IF or_test ELSE test )? | lambdef ;
test_nocond : or_test | lambdef_nocond ;
lambdef : LAMBDA ( varargslist )? THEN test ;
lambdef_nocond : LAMBDA ( varargslist )? THEN test_nocond ;
or_test : and_test ( OR and_test )* ;
and_test : not_test ( AND not_test)* ;
not_test : NOT not_test | comparison ;
comparison : expr ( comp_op expr )* ;
comp_op : LESS_THAN | GREATER_THAN | EQUALS | GT_EQ | LT_EQ | NOT_EQ_1 | NOT_EQ_2 | IN | NOT IN | IS | IS NOT ;
star_expr : STAR expr ;
expr : xor_expr ( OR_OP xor_expr )* ;
xor_expr : and_expr ( XOR and_expr )* ;
and_expr : shift_expr ( AND_OP shift_expr )* ;
shift_expr : arith_expr ( ( LEFT_SHIFT | RIGHT_SHIFT ) arith_expr )* ;
arith_expr : term ( ( ADD | MINUS ) term )* ;
term : factor ( ( STAR | AT | DIV | MOD | IDIV ) factor )* ;
factor : ( ADD | MINUS | NOT_OP ) factor | power ;
power : atom_expr ( POWER factor )? ;
atom_expr
    : OPEN_PAREN ( yield_expr | testlist_comp )? CLOSE_PAREN
    | OPEN_BRACK ( testlist_comp )? CLOSE_BRACK
    | OPEN_BRACE ( dictorsetmaker )? CLOSE_BRACE
    | NAME
    | NUMBER
    | STRING+
    | ELLIPSIS
    | NONE
    | TRUE
    | FALSE
    ;
testlist_comp : ( test | star_expr ) ( comp_for | ( COMMA (test | star_expr ) )* ( COMMA )? ) ;
trailer : OPEN_PAREN ( arglist )? CLOSE_PAREN | OPEN_BRACK subscriptlist CLOSE_BRACK | DOT NAME ;
subscriptlist : subscript ( COMMA subscript )* ( COMMA )? ;
subscript : test | ( test )? COLON ( test )? ( sliceop )? ;
sliceop : COLON ( test )? ;
exprlist : ( expr | star_expr ) ( COMMA ( expr | star_expr ) )* ( COMMA )? ;
testlist : test ( COMMA test )* ( COMMA )? ;
dictorsetmaker
    : ( ( ( test COMMA test | '**' expr ) 
          ( comp_for | ( COMMA ( test COLON test | '**' expr ) )* ( COMMA )? ) )
      | ( ( test | star_expr )
          ( comp_for | ( COMMA ( test | star_expr ) )* ( COMMA )? ) )
    ) ;

classdef : CLASS NAME ( OPEN_PAREN ( arglist )? CLOSE_PAREN )? COLON suite ;

arglist : argument ( COMMA argument )* ( COMMA )? ;

argument
    : test ( comp_for )?
    | test ASSIGN test
    | '**' test
    | '*' test
    ;

comp_iter : comp_for | comp_if ;
comp_for : ( ASYNC )? FOR exprlist IN or_test ( comp_iter ) ;
comp_if : IF test_nocond ( comp_iter )? ;

encoding_decl: NAME ;

yield_expr : YIELD ( yield_arg ) ;
yield_arg : FROM test | testlist ;