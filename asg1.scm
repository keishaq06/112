#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.8 2019-01-11 17:38:01-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;


;; yes its very messy code i didnt really prioritize  writing clean code
;;
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
;; create symbol table and define functions
(define *symbol-table* (make-hash))
(define (symbol-get key)
        (hash-ref *symbol-table* key))

(define (symbol-put! key value)
    (hash-set! *symbol-table* key value)
)

;;create array table and define functions
(define *array-table* (make-hash))
(define (array-get key index)
    (define vec (hash-ref *array-table* key))
    (vector-ref vec index))


(define (array-make size)
    (make-vector size 0))



(define (array-set! key value)
    (hash-set! *array-table* key value))

(define (sbDim listRef)
  (array-set! (car(cdr(car listRef))) 
    (array-make  (cadr(cdar listRef))) ;;2nd parameter       
  )

)

;; asub, name of the array, and index
;; index - 1
(define (sbAsub listRef)
 ;;   (array-get (car listRef) (- (cadr listRef) 1))   

    (if (symbol? (cadr listRef))
        (begin
          (array-get (car listRef) (variable-get (cadr listRef)) )
          
          )
        (array-get (car listRef) (cadr listRef) )

      
      
    )
)



;; create variable table and define functions
(define *variable-table* (make-hash))
(define (variable-put! key value)
   (hash-set! *variable-table* key value)   
)
(define (variable-get key)
    (hash-ref *variable-table* key 0)  
)

(define (exprVars expr)
    (cond
        [(and (symbol? (cadr expr)) (symbol? (car(cddr expr))))
         (list (car expr) (variable-get (cadr expr))
               (variable-get (car(cddr expr))))
        ]
        [(symbol? (cadr expr))
         (list (car expr) (variable-get (cadr expr)) (car(cddr expr)))

        ]
        [(symbol? (car(cddr expr)))
         (list (car expr) (cadr expr) (variable-get(car(cddr expr))))
        ]
        [else
            expr
        ]
      
      
      
    )  
  
  
)

(define (sbLet listRef)

  (cond
    [(symbol? (car(cdr listRef)))
     (variable-put! (car listRef) (variable-get (cadr listRef)))

     
     
     ]

    [(symbol? (car listRef))
        (if(pair? (cadr listRef))
            (if(hash-has-key? fnhash   (car(cadr listRef) ))
                (begin
                  
                  (if (eqv? (length (cadr listRef)) 2)
                    (variable-put! (car listRef)
                                   (evalexpr(cadr listRef)))
                   (variable-put! (car listRef)
                                  (evalexpr(exprVars(cadr listRef)))) 
                    
                    )
                  
                  

                  
                  )
                (displayln "other func"))
            (variable-put! (car listRef) (cadr listRef))
        )
    ] 
    [else
        (let [(index (cadr(cdr(car listRef))))
              (value (cadr listRef))]
            (symbol-put! "letIndex" index)
            (symbol-put! "letValue" value)
            
            (cond
              [(symbol? index)
               (symbol-put! "letIndex" (variable-get index))
              ]
              [else
                (symbol-put! "letIndex" index)
              ]
              
              
            )
          
          
          
        )

        (vector-set! (hash-ref *array-table* (car(cdr(car listRef))))
                     (symbol-get "letIndex")
                        (cadr listRef) ) 
        
        

    ]
    
    
  )
)
  
      
(define (sbIf listRef)
    (when (evalexpr (car listRef))
        
        (sbGoto (cdr listRef))  
      
      
    )
) 
  
(define (sbInput listRef)
    (cond
         [(null? listRef)
        ;;(displayln "no more to assign")
            (void) ;;no more memory locations -> do nothing
         ]
         [else
    
            (let ((theInput (read)))
        ;;(displayln theInput)
            (cond
                [(eof-object? theInput) ;;check for end of file (input)
                    (variable-put! eof 1) 
                    (/ 0.0 0.0)    ;;return nan

                ]
                [(number? theInput) ;;check for number input
                    (variable-put! (car listRef) theInput)          
                    (displayln theInput)

                ]
                [else (displayln "invalid value")
                    (/ 0.0 0.0)
                ]    
            ))
        (sbInput (cdr listRef)) ]
    
        )
       
)
  
  


(define fnhash (make-hash)) ;;copied provided code
(for-each
    (lambda (item) (hash-set! fnhash (car item) (cadr item)))
    `((+ ,+)(- ,-)(* ,*)(/ ,/)(<= , <=)
      (<> , (lambda(x y ) (not(= x y))))
      (>= ,>=) (> , >)(< ,<)(% , (lambda (x y)
    (- x (* (trunc (/ x y)) y))))
      (^ , (lambda (x y) (expt x y)))
      (atan , atan)(sqrt , sqrt)(exp , exp)
      (log , log)(sin , sin)(cos , cos)(tan , tan)
      (acos , acos)(abs , abs)
      (asin , asin)(floor , floor)
      (ceil , ceiling)(round , round)(= , =)
    )
)

(define (evalexpr expr) ;;copied mackey's provided code

    (cond [(number? expr) (+ 0.0 expr)]
          [(symbol? expr) (variable-get expr)]
          (else (let ((fn (hash-ref fnhash (car expr)))
                      (args (map evalexpr (cdr expr))))
                     (apply fn args)))))

(define (sbPrint listRef)
    (cond 
      [(null? listRef)
        (display "\n")
      ]
      [(pair? (car listRef))
        (if (eqv? (caar listRef) 'asub)

          (displayln (sbAsub  (cdar listRef)))
          (begin
            (displayln (evalexpr (car listRef)))
            (sbPrint (cdr listRef))
          )
          
          
        )
      ;; (displayln (evalexpr (car listRef)))
      ;; (sbPrint (cdr listRef))

      ]
      [(symbol? (car listRef))
       (display( variable-get (car listRef)))
       (display " ")
       (sbPrint (cdr listRef))
        
      ]
      [else
        (display (car listRef))
        (display " ")
        (sbPrint (cdr listRef))

      ]

  
  
    )
)

(define (sbGoto labelLine)
 ;;   (displayln "in sbgoto")
   ;; (displayln labelLine)
   ;; (displayln "done")
    (define line (hash-ref labelTable (car labelLine) "not here"))
   ; (display (car line))
    (execLine (symbol-get "program")(- (car line) 1))
)
(define bigLine "================================================ \n")


(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (^       ,expt)(atan , atan)(asin , asin )(abs , abs)
        (ceil    ,ceiling) (acos , acos)
        (exp     ,exp)
        (floor   ,floor) (round , round)
        (log     ,log)
        (sqrt    ,sqrt)
        (print   ,sbPrint)
        (goto    , sbGoto)
        (dim , sbDim) (asub , sbAsub) (let , sbLet)
        (input   ,sbInput)
        (- ,-)(/ ,/)(<= , <=)(>= ,>=) (> , >)(< ,<)(+ ,+)
        (* , *)(if , sbIf)

    )
)


(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (nan , (/ 0.0 0.0))
        (eof , 0.0)
        (pi  , (acos -1.0))
        (e   , (exp 1.0))
    


     ))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))
(define labelTable (make-hash))

(define (isLabel listRef)
  (define afterNum (cdr listRef))
  (cond 
          ((null? afterNum) ;; empty line, do nothing
            #f) 
          ((pair? (car afterNum))  ;; first element of 
            #f)
          (else
            #t
          )
  )     
)

(define (applyFunction name parameters)
    ((hash-ref *symbol-table* name "key not here") parameters) 
  
  
)
(define (evalStatement program instr lineNum)
    (cond
        [(eqv? (caar instr) 'goto)
            (applyFunction (caar instr) (cdar instr))
        ]
        [(hash-has-key? fnhash (caar instr))
            (displayln (evalexpr (car instr)))
        ]
           
        [else
            (applyFunction (caar instr) (cdar instr))
            (execLine program (+ 1 lineNum))
        ]
    )
)
  
  
  
  
  

(define (execLine program lineNum)
    (if (< lineNum  (length program)) 
        (let ([line (list-ref program lineNum)]) 

         (cond 
              [;;immediate skip if no instruction
                   (or  (= (length line) 1) 
                        (and (isLabel line)(= (length line) 2))
                   )
                        (execLine program (+ 1 lineNum)) 
              ]
              [;; if label, instruction is at cddr, cdr if not label
                    (isLabel line)
                        (evalStatement program (cddr line) lineNum)    
              ]

              [ else
                (evalStatement program (cdr line) lineNum) 
              ]
        )
  ) (void))
    
    
)    
      
      
      
(define (storeLines filename program)
    (displayln bigLine)
    (printf "~a: ~s~n" *run-file* filename)
    (displayln bigLine)
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (displayln bigLine)
    (displayln "Labels:")
    (displayln bigLine)
        

    (map (lambda (line)
        (if (isLabel line)
            (begin
                (hash-set! labelTable (cadr line) line)
                (displayln (cadr line))
            )
            (void)
          
        )       
    ) program)    
       
           
           


    (display "\n")
)
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                 (program (readlist-from-inputfile sbprogfile)))
              (storeLines sbprogfile program)
              (displayln bigLine)
              (displayln "program executing")
              (displayln bigLine)
              (hash-set! *symbol-table* "program" program)
              (execLine program 0)
              )
        )
    )

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))




