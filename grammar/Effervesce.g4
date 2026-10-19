grammar Effervesce;

// Effervesce is a language-agnostic language for describing a system as typed
// effects (capabilities), handlers (interpretations), components (consumers),
// state, transitions, invariants, and policies. One description drives three
// checkers:
//
//   - type check  : every used effect is handled (effect-row soundness)
//   - model check : invariants hold over reachable states
//   - policy check: forbid/require predicates over the description hold
//
// The language is check-only. Execution is a later, optional adaptor.

system
    : 'system' ID '{' decl* '}'
    ;

decl
    : effectDecl
    | handlerDecl
    | componentDecl
    | assumeDecl
    | stateDecl
    | actionDecl
    | invariantDecl
    | policyDecl
    ;

// ── Capabilities ───────────────────────────────────────────────────

// A named capability with typed operations. `Read<A>` is a capability family
// parameterized by A.
effectDecl
    : 'effect' ID typeParams? '{' opDecl* '}'
    ;

opDecl
    : ID '(' params? ')' '->' type_ ';'?
    ;

// ── Components ─────────────────────────────────────────────────────

// A component consumes an effect row and exposes typed operations. Its body
// invokes effects; the type checker verifies every invocation is in `uses`.
componentDecl
    : 'component' ID '{' ('uses' effectRow)? opBody* '}'
    ;

opBody
    : 'op' ID '(' params? ')' '->' type_ '=' block
    ;

// ── Handlers ───────────────────────────────────────────────────────

// A handler interprets one effect in terms of a (possibly empty) effect row.
// An empty row means the handler is terminal: nothing further is required.
handlerDecl
    : 'handler' ID ':' effectRef '=>' effectRow? '{' opImpl* '}'
    ;

opImpl
    : ID '(' paramNames? ')' '=' block
    ;

// A capability the environment provides directly (terminal by fiat).
assumeDecl
    : 'assume' '{' effectRow '}' ';'?
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

invariantDecl
    : 'invariant' ID '{' expr '}'
    ;

// ── Policies (predicates over the description) ─────────────────────

policyDecl
    : 'policy' ID '{' ('forbid' | 'require') predicate ';' '}'
    ;

predicate
    : 'component' 'uses' effectRef       // component uses X
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
    : ID ':' type_ (',' ID ':' type_)*
    ;

paramNames
    : ID (',' ID)*
    ;

// ── Expressions (minimal; the first checker treats these as opaque) ─

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
