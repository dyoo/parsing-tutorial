#lang racket

(require parser-tools/lex
         parser-tools/yacc
         rackunit)

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


;; tokenize-graphics/1: input-port -> token
;; Tokenizes a single token from an input port.
(define tokenize-graphics/1
  (lexer 
   ["line" (token-LINE lexeme)]
   ["from" (token-FROM lexeme)]
   ["to" (token-TO lexeme)]
   ["," (token-COMMA lexeme)]
   [(repetition 1 +inf.0 numeric)
    (token-INT (string->number lexeme 10))]
   [whitespace
    ;; Skip whitespace and continue.
    (tokenize-graphics/1 input-port)]
   [(eof) (token-EOF eof)]))
   

;; We get a function that consumes an input port, and produces a single
;; token.
;;
;; This doesn't automatically fit the parser, which expects a function that
;; can be called multiple times to get tokens.
;;
;; But we can construct the expected input pretty easily.

;; token-generate: input-port (input-port -> token) -> (-> token)
(define (token-generate tokenize/1 ip)
  (define (f)
    (tokenize/1 ip))
  f)


;; Let's try it out.
(define sample-token-thunk 
  (token-generate tokenize-graphics/1
                  (open-input-string "line from 0,42")))
(check-equal? (sample-token-thunk)
              (token-LINE "line"))
(check-equal? (sample-token-thunk)
              (token-FROM "from"))
(check-equal? (sample-token-thunk)
              (token-INT 0))
(check-equal? (sample-token-thunk)
              (token-COMMA ","))
(check-equal? (sample-token-thunk)
              (token-INT 42))
(check-equal? (sample-token-thunk)
              (token-EOF eof))



;; Now that we can create a source of tokens at will,
;; let's get to the parser part of things.

;; parse-graphics: (-> token) -> (listof line)
(define parse-graphics
  (parser 
   ;; We declare the terminal elements of our grammar to be
   ;; the tokens defined by graphics-tokens.
   (tokens graphics-tokens)
      
   ;; The parser will try to match the rule for 'file'.
   (start file)
   
   ;; ... and the parser succeeds when a parse for 'file' ends
   ;; with the EOF token:
   (end EOF)
   
   
   ;; We have to describe how the non-terminal rules work.
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
            (point $1 $3)]])

   
   ;; Finally, we should tell the parser how to report errors as they occur.
   (error 
    (lambda (tok-ok? tok-name tok-value)
      (error 'parse-graphics)))))
  

;; At this point, we can call parse-graphics on a generator of tokens,
;; and get back a list of commands.
(define parsed-lines
  (parse-graphics 
   (token-generate tokenize-graphics/1
                   (open-input-string
                    #<<EOF
line from 0,0 to 0,10
line from 0,10 to 10,10
line from 10,10 to 10,0
line from 10,0 to 0,0
EOF
))))