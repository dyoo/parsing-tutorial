#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/match
         racket/generator)

;; We'd like to parse lines of the form:
;;
;; line from 0,0 to 0,10
;; line from 0,10 to 10,10
;; ...
;;

;; into the following structures.
(struct point (x y) #:transparent)
(struct line (p1 p2) #:transparent)


;; Let's first specify the grammar.
;;
;;    file : command+
;;    command: 'line' 'from' point 'to' point
;;    point: int ',' int


;; We can look at this and see that our grammar is made up of
;; non-terminal rules, as well as terminal tokens.
;; Let's define the tokens of our language.  Here we go:
(define-tokens graphics-tokens (EOF LINE FROM TO COMMA INT))
;;
;; This will define several functions for constructing tokens:
;;
;;     token-EOF, token-LINE, token-FROM, token-TO, token-COMMA, and token-INT
;;
;; which are all functions that take a single "value" argument.


;; Let's see how to tokenize an input stream.  We use the tools
;; provided by the parser-tools/lex and parser-tools/lex-sre libraries.


;; tokenize/1: input-port -> token
;; Tokenizes a single token from an input port.
(define tokenize/1
  (lexer 
   ["line" (token-LINE lexeme)]
   ["from" (token-FROM lexeme)]
   ["to" (token-TO lexeme)]
   ["," (token-COMMA lexeme)]
   [(:+ numeric)
    (token-INT (string->number lexeme 10))]
   [whitespace
    ;; just skip whitespace and continue.
    (tokenize/1 input-port)]
   [(eof) (token-EOF eof)]))
   
;; We get a function that consumes an input port, and produces a single
;; token.
;;
;; This doesn't automatically fit the parser, which expects a function that
;; can be called multiple times to get tokens.  But we can construct the expected
;; input pretty easily.
(define (make-token-stream ip)
  (define (f)
    (tokenize/1 ip))
  f)

;; Let's try it out.
(define sample-token-thunk (make-token-stream 
                            (open-input-string "line from 0,0 to 0,10")))
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk)
(sample-token-thunk) ;; should match 10
(sample-token-thunk) ;; should be eof
(sample-token-thunk) ;; should be eof





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aside: we can test the parser independently of the lexer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Since the token constructors are available,
;; we can test our parser independently of a dedicated lexer.
;; Here's a quick-and-dirty approach that uses s-expressions as
;; an input, and produces a lexer.

;; Treat the following sexp->tokens function as magic at the moment: we'll
;; replace this with a real lexer that doesn't use s-expressions as an
;; intermediate format, in a moment.

;; sexp->tokens: sexp -> (listof token)
(define (sexp->tokens sexp)
  (define (dispatch x acc)
    (match x
      ['line (cons (token-LINE "line") acc)]
      ['from (cons (token-FROM "from") acc)]
      ['to (cons (token-TO "to") acc)]
      [(list 'unquote elt) (dispatch elt (cons (token-COMMA ",") acc))]
      [(? number?) (cons (token-INT x) acc)]))
  (reverse (foldl dispatch (list) sexp)))

;; Now for our sample data:
(define sample-tokens
  (sexp->tokens '(line from 0,0 to 0,10
                  line from 0,10 to 10,10
                  line from 10,10 to 10,0
                  line from 10,0 to 0,0)))
;; Parsers and lexers typically don't consume the whole file at once: rather, they do it by
;; demand.  This design means that the parser won't accept lists of tokens as inputs, but
;; instead it will use a function that it calls to pull elements on demand.


;; We can use sequence->generator to get us such a function.
;; tokens-thunk: (listof token) -> (-> token)
(define (tokens->thunk tokens)
  (sequence->generator 
   (sequence-append tokens 
                    (forever-sequence (token-EOF eof)))))


;; helper: given v, produces a sequence with nothing but v.
(define (forever-sequence v)
  (in-cycle (list v)))

;; sample-tokens-thunk: -> token
(define sample-tokens-thunk
  (tokens->thunk sample-tokens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; Now that we can create a source of tokens at will,
;; let's get to the parser part of things.

;; parse-graphics: (-> token) -> (listof line)
(define parse-graphics
  (parser 
   ;; We declare the terminal elements of our grammar to be
   ;; the tokens defined by graphics-tokens.
   (tokens graphics-tokens)
   
   (error 
    (lambda (tok-ok? tok-name tok-val start-pos end-pos)
      (error 'parser)))
   
   (end EOF)
   (start file)
   
   ;; Along with the terminals, we have to describe how
   ;; the non-terminal rules work.
   (grammar 
    
    ;; Each rule describes a shape, as well as an action to
    ;; perform when it recognizes a shape.
    [file [(command+) 
           $1]]
    
    ;; For example, when the parser recognizes a command followed
    ;; immediately by another bunch of commands, we can collect
    ;; them together into a single command list:
    [command+ [(command command+) 
               (cons $1 $2)]
              
              [() 
               (list)]]
    
    [command [(LINE FROM point TO point)
              (line $3 $5)]]
    
    [point [(INT COMMA INT)
            ;; The "value" of a terminal is its token-value.
            ;; So when we refer to the INT components here via $1 and $3,
            ;; we get their values.
            (point $1 $3)]])))
  

;; At this point, we can call parse-graphics on our sample-tokens-thunk, and get back four lines.
(define parsed-lines
  (parse-graphics sample-tokens-thunk))


(define another-parsed-lines
  (parse-graphics 
   (make-token-stream 
    (open-input-string
     "line from 0,0 to 0,10
      line from 0,10 to 10,10
      line from 10,10 to 10,0
      line from 10,0 to 0,0"))))