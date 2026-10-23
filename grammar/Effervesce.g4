grammar Effervesce;

// Effervesce describes a system as typed effects (capabilities), how they are
// provided (given / via), who consumes them (components), and the rules that
// must hold (always / never / must). One description drives three checkers:
//
//   - type check  : every used capability is handled (effect-row soundness)
//   - model check : every `always` invariant holds over reachable states
//   - policy check: every `never` / `must` predicate holds over the description
//
// The language is check-only. Execution is a later, optional adaptor.

system
    : 'system' ID '{' decl* '}'
    ;

decl
    : capabilityDecl
    | givenDecl
    | handlerDecl
    | componentDecl
    | stateDecl
    | actionDecl
    | invariantDecl
    | policyDecl
    ;

// ── Capabilities (typed effects) ──────────────────────────────────
//
// A capability is a typed effect: a named thing your system can ask for, with
// typed operations. `Read<A>` is a capability family over A.

capabilityDecl
    : 'capability' ID typeParams? '{' opDecl* '}'
    ;

opDecl
    : ID '(' params? ')' ('->' type_)? ';'?
    ;

// ── Wiring: who provides what ─────────────────────────────────────

// The environment provides these capabilities directly (terminal).
givenDecl
    : 'given' effectRow ';'?
    ;

// `E via R` interprets capability E in terms of the row R. The redundant
// handler name is gone; the capability being handled is the name.
handlerDecl
    : effectRef 'via' effectRow? '{' opImpl* '}'
    ;

// A component consumes an effect row (`needs`) and may concurrently provide
// capabilities to other components (`provides`). Its body invokes effects; the
// type checker verifies every invocation is in `needs`, and that a `provides`
// row is reachable by peers the same way `given`/`via` is.
componentDecl
    : 'component' ID componentClause* '{' opImpl* '}'
    ;

componentClause
    : 'needs' effectRow
    | 'provides' effectRow
    ;

opImpl
    : ID '(' params? ')' ('->' type_)? '=' body
    ;

// ── State and transitions (model checking) ────────────────────────

stateDecl
    : 'state' ID ':' type_ '=' expr ';'?
    ;

actionDecl
    : 'action' ID '{' step* '}'
    ;

step
    : ID ':=' expr ';'?     // state update
    | expr ';'?             // effect invocation
    ;

// A property that must hold in every reachable state.
invariantDecl
    : 'always' expr ';'?
    ;

// A rule over the description itself: `never` forbids, `must` requires.
policyDecl
    : ('never' | 'must') predicate ';'?
    ;

predicate
    : 'component' 'needs' effectRef       // component needs X
    | 'not' predicate
    | predicate 'and' predicate
    | predicate 'or' predicate
    | '(' predicate ')'
    ;

// ── Effect rows and types ──────────────────────────────────────────

effectRow
    : effectRef (',' effectRef)*
    ;

effectRef
    : ID typeArgs?
    ;

typeParams
    : '<' ID (',' ID)* '>'
    ;

typeArgs
    : '<' type_ (',' type_)* '>'
    ;

type_
    : ID
    | ID '<' type_ (',' type_)* '>'
    ;

params
    : param (',' param)*
    ;

param
    : ID (':' type_)?
    ;

// ── Expressions (minimal; the first checker treats these as opaque) ─

body
    : expr            // a single operation needs no braces
    | block           // several operations need braces
    ;

block
    : '{' expr (';' expr)* ';'? '}'
    ;

expr
    : literal
    | ID '(' args? ')'       // operation / function call
    | expr '.' ID            // field access
    | ID
    | '(' expr ')'
    | 'not' expr
    | expr ('*' | '/' | '%') expr
    | expr ('+' | '-') expr
    | expr ('==' | '!=' | '<' | '<=' | '>' | '>=') expr
    | expr ('and' | 'or') expr
    ;

args
    : expr (',' expr)*
    ;

literal
    : INT
    | STRING
    | 'true'
    | 'false'
    | 'none'
    ;

// ── Lexical ────────────────────────────────────────────────────────

ID      : [a-zA-Z_][a-zA-Z0-9_]* ;
INT     : [0-9]+ ;
STRING  : '"' (~["\\] | '\\' .)* '"' ;

COMMENT       : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '/*' .*? '*/' -> skip ;

WS      : [ \t\r\n]+ -> skip ;
