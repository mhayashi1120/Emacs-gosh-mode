(define-module emacs.gosh-mode
  (use srfi-1)
  (use srfi-14)
  (use srfi-43)
  (use file.util)
  (use util.match)
  (export 
   print-object
   print-signature print-signatures
   read-details
   )
  )
(select-module emacs.gosh-mode)

;;TODO (eval-when (:compile-toplevel :load-toplevel))

(define-constant serialize-identifier 'g2e)
(define-constant serialize-string-abbrev "...")
(define-constant serialize-string-maxsize 50)

;; Emacs 23 or later accept -536870912 to 536870911
(define-constant min-number -134217728)
(define-constant max-number 134217727)

(define-method print-object ((obj <top>))
  (error (format "TODO assert ~s" obj)))

(define-method print-object ((obj <char-set>))
  (serialize-print 'charset (x->string obj)))

(define-method print-signatures ((obj <generic>))
  (define (print-method obj)
	(display "(lambda (")
	(display (string-join
			  (let1 i 1
				(map
				 (lambda (arg)
				   (begin0
					 (format "arg~a~a" i (ref arg 'name))
					 (inc! i)))
				 (ref obj 'specializers)))
			  " "))
	(display ")")
	(display ")"))

  (let loop ((ms (ref obj 'methods)))
	(display "(")
	(display (ref obj 'name))
	(display " ")
	(print-method (car ms))
	(display ")")
	(when (pair? (cdr ms))
	  (display " ")
	  (loop (cdr ms)))))

(define-method print-signature ((obj <procedure>))
  (display "(")
  (display (ref obj 'info))
  (display " ")
  (display "(lambda ")
  (if (> (ref obj 'required) 0)
	(begin
	  (display "(")
	  (display (string-join
				(map
				 (lambda (i)
				   (format "arg~a" i))
				 (iota (ref obj 'required) 1))
				" "))
	  ;;TODO optional rest not exactly correct
	  (when (ref obj 'optional)
		(display " . rest"))
	  (display ")"))
	(when (ref obj 'optional)
	  (display "rest")))
  (display ")")
  (display ")"))

;;TODO print quoted huge expression

(define-method print-object ((obj <pair>))
  (display "(")
  (let loop ((s obj))
	(print-object (car s))
	(cond
	 ((pair? (cdr s))
	  (display " ")
	  (loop (cdr s)))
	 ((null? (cdr s)))
	 (else
	  (display " . ")
	  (print-object (cdr s)))))
  (display ")"))

(define-method print-object ((obj <vector>))
  (display "[")
  (vector-map
   (lambda (idx item)
	 (when (> idx 0)
	   (display " "))
	 (print-object item))
   obj)
  (display "]"))

(define-method print-object ((obj <keyword>))
  (display (string-append ":" (keyword->string obj))))

(define-method print-object ((obj <symbol>))
  (display obj))

(define-method print-object ((obj <null>))
  (display '()))

(define-method print-object ((obj <char>))
  ;;TODO check valid or not
  (display "?\\")
  (display obj))

(define-method print-object ((obj <boolean>))
  (serialize-print 'boolean (x->string obj)))

(define-method print-object ((obj <string>))
  (if (< (string-length obj) serialize-string-maxsize)
	(display obj)
	(serialize-print 'string obj)))

(define-method print-object ((obj <regexp>))
  (serialize-print 'regexp (regexp->string obj)))

(define-method print-object ((obj <number>))
  (serialize-print 'number (number->string obj)))

(define-method print-object ((obj <integer>))
  (if (and (<= min-number obj) (<= obj max-number))
	(display (number->string obj))
	(serialize-print 'number (number->string obj))))

(define (serialize-print type string)
  (let* ((max (- serialize-string-maxsize (string-length serialize-string-abbrev)))
		 (len (string-length string)))
	(let1 str
		(string->printable-string
		 (if (> len serialize-string-maxsize)
		   (string-append (string-copy string 0 max) serialize-string-abbrev)
		   string))
	  ;;FIXME " -> \"
	  ;;TODO newline -> \n
	  (display (cons serialize-identifier
					 (cons type (string-append "\"" str "\"")))))))

;;TODO only read instructed module (define-module hoge) (select-module hoge)
(define-method read-details ((module <symbol>))
  (if-let1 file (find-module-file module)
	(read-details file)
	'()))

(define-method read-details ((file <string>))
  (call-with-input-file file read-details))

;;TODO group by module
(define-method read-details ((port <port>))
  (define (next)
	(read port))

  (let loop ((s (next))
			 (details '())
			 (module 'user))

	(define (parse-toplevel module sexp)
	  (let1 def (parse-toplevel-def sexp)
		(cond
		 ((not def)
		  (let1 def2 (parse-toplevel-def2 sexp)
			(cond
			 ((not def2)
			  details)
			 (else
			  (cons def2 details)))))
		 ((and (pair? def) (not (symbol? (car def))))
		  ;; when not a single definition (ex: syntax)
		  (append (map (^x (cons module x)) def) details))
		 (else
		  (cons (cons module def) details)))))

	(match s
	  [(? eof-object? _)
	   (reverse details)]
	  [(? (^x (not (pair? x))) _)
	   (loop (next) details module)]
	  [('define-module mod . defs)
	   (map
		(lambda (s)
		  (set! details (parse-toplevel mod s)))
		defs)
	   ;;recursive
	   (loop (next) details module)]
	  [('select-module module . _)
	   (loop (next) details module)]
	  [_
	   (loop (next) (parse-toplevel module s) module)])))

(define (find-module-file module)
  (let* ((path (string-append (module-name->path module) ".scm"))
		 (file (fold 
				(lambda (dir r)
				  (let1 file (build-path dir path)
					(or r
						(and (file-exists? file)
							 file))))
				#f *load-path*)))
	file))

(define (parse-toplevel-def2 sexp)
  (match sexp
	[('define-in-module module (name . args) . _)
	 (if-let1 def (parse-closure name args)
	   (cons module def)
	   #f)]
	[('define-in-module module name value)
	 (if-let1 def (parse-closure name value)
	   (cons module def)
	   #f)]
	[('define-in-module module name . value)
	 (parse-closure name value)]
	[_ #f]
	))

;;TODO define-values
(define (parse-toplevel-def sexp)
  (match sexp
	[((or 'define 'define-constant) (name . args) . _)
	 ;; ex: (define (sym . args)) 
	 (parse-closure name args)]
	[((or 'define 'define-constant) name 
	  ((or 'lambda '^) args . _))
	 ;; ex: (define sym (lambda ())) 
     ;;    unable handle (define sym (if anycond (lambda ()) (lambda ())) )
	 (parse-closure name args)]
	[((or 'define 'define-constant) name 
	  (or (? (.$ not pair?) value)
		  (? (^x (eq? (car x) 'quote)) value)))
     ;; ex: (define sym 1000), (define sym '(a b))
	 ;; only atom object (ex:number, symbol) or quoted expression
	 (list name value)]
	[((or 'define 'define-constant) name . _)
	 ;;TODO accept ^a ^b...
	 (list name)]
	[('define-class name supers slot-specs . _)
	 (parse-class name slot-specs)]
	[('define-macro (name . formals) . _)
	 (parse-macro name formals)]
	[('define-macro name . _)
	 (parse-macro name '())]
	[('define-syntax name ('syntax-rules args . rules))
	 (parse-syntax name args rules)]
	[('define-condition-type name _ _ . field-specs)
	 (parse-condition name field-specs)]
	[('define-method name args . _)
	 (parse-generic name args)]
	;;TODO
	[_ #f]))

;; TODO check implements is some degree of correct.
(define (parse-syntax name args rules)
  ;; reverse: apply to cons loop mecanism
  (fold
   (lambda (rule r)
	 (match rule
	   [(_ . (((or 'syntax-errorf 'syntax-error) . _) . _))
		;; ignore common syntax error rule
		r]
	   [else
		(cons
		 (list name 
			   ;;TODO syntax-rules args
			   (list 'syntax 
					 (cdar rule)))
		 r)]))
   '() rules))

(define (parse-condition name field-specs)
  (list name
		(list 'class
			  (fold
			   (lambda (field r)
				 (if (pair? field)
				   (cons (car field) r)
				   r))
			   '() field-specs))))

(define (parse-class name args)
  (list name 
		(list 'class 
			  (fold 
			   (lambda (arg r)
				 (if (pair? arg)
				   (cons (car arg) r)
				   r))
			   '() args))))

(define (parse-macro name args)
  (list name (list 'syntax args)))

(define (parse-generic name args)
  (list name
		(list 'lambda
			  (map-args
			   (lambda (arg)
				 (match arg
				   [(name class) 
					(string->symbol
					 (string-append 
					  (x->string name)
					  (x->string class)))]
				   [name name]))
			   args))))

(define (parse-closure name args)
  (list name
		(list 'lambda
			  (cond
			   ((symbol? args)
				args)
			   ((or (pair? args)
					(null? args))
				(map-args
				 (lambda (arg)
				   (match arg
					 [(? keyword? name) name]
					 [(name (_ . _))
					  ;; ignore procedure call or lambda
					  (list name)]
					 [(name default)
					  (list name default)]
					 [name
					  name]))
				 args))
			   (else
				(errorf "assert ~a" (class-of args)))))))

;; map args (can accept dot list)
(define (map-args proc args)
  (let loop ((lis args)
			 (res '()))
	(cond
	 ((null? lis)
	  (reverse res))
	 ((pair? lis)
	  (loop (cdr lis) (cons (proc (car lis)) res)))
	 (else
	  (append (reverse res) (proc lis))))))

(define (string->printable-string string)
  (string-join 
   (map char->printable-string (string->list string))
   ""))

(define (char->printable-string c)
  (let1 i (char->integer c)
	(if (< i 31)
	  (string-append (format "\\~3,'0o" i))
	  (x->string c))))
  
;;TODO for testing

;; (define-constant c1 10000)
;; (define c2 10000)

;; (define-macro (m1 v1 v2 . vars)
;;   `(apply list ,v1 ,v2 ',vars))

;; (define-macro m2
;;   (lambda ()))

;; (define-syntax s2
;;   (syntax-rules ()
;;     [(s2 var . else) 
;; 	 (let ((var "A")) 
;; 	   . else)]))

;; (define (f1)
;;   )
;; (define (f2 :key (k1 #f) (k2 (lambda () 5)))
;;   )

;; (define-class <cl1> ()
;;   ((pid       :init-value -1 :getter process-pid)
;;    :metaclass <instance-pool-meta>))

;; (define-class <cl2> ()
;;   ((pid       :init-value -1 :getter process-pid)))

;; (define-condition-type <cd1-error> <error> #f)

;; (define hoge 
;;   (if #t
;; 	(lambda () 3)
;; 	(lambda () 5)))

;; (define hoge2
;;   (lambda () 3))

;; (define hoge3
;;   (lambda (a b) (list a b)))

;; (define hoge4 500)
