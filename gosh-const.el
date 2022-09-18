;;; gosh-const.el --- gosh-mode constant settings.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:

(defvar current-language-environment)



(defconst gosh-const-procedure-keyword-list
  '(
    ;; define methods
    "define-condition-type"
    "define-in-module" "define-record-type" "define-values"

    "export" "export-if-defined" "export-all"

    "require"
    "dynamic-wind"

    "^"
    "^a" "^b" "^c" "^d" "^e" "^f" "^g" "^h" "^i"
    "^j" "^k" "^l" "^m" "^n" "^o" "^p" "^q" "^r"
    "^s" "^t" "^u" "^v" "^w" "^x" "^y" "^z"

    ;; keywords special indent function by `scheme-indent-function' property
    "and-let*" "and-let1" "begin0" "call-with-client-socket"
    "call-with-input-conversion" "call-with-input-file"
    "call-with-input-process" "call-with-input-string"
    "call-with-iterator" "call-with-output-conversion"
    "call-with-output-file" "call-with-output-string"
    "call-with-temporary-file" "call-with-values"
    "dolist" "dotimes" "do-generator"
    "ecase" "guard"
    "if" "if-let1" "if-match"
    "glet" "glet1" "glet*"
    "let*-values" "let-args"
    "let-keywords*" "let-match" "let-optionals*" "let-syntax"
    "let-values" "let/cc" "let1" "letrec-syntax" "make" "match"
    "multiple-value-bind" "parameterize" "parse-options" "receive"
    "rlet1" "rxmatch-case" "rxmatch-cond" "rxmatch-if" "rxmatch-let"
    "select-module"
    "syntax-rules" "unwind-protect" "unless" "until" "when" "while"
    "with-builder" "with-error-handler" "with-error-to-port"
    "with-input-conversion" "with-input-from-port"
    "with-input-from-process" "with-input-from-string" "with-iterator"
    "with-locking-mutex" "with-module" "with-output-conversion"
    "with-output-to-port" "with-output-to-process"
    "with-output-to-string" "with-port-locking" "with-signal-handlers"
    "with-string-io" "with-time-counter"
    ))

(defconst gosh-defined-procedure-keyword-regexp
  (eval-when-compile
    (concat "(\\("
            (regexp-opt gosh-const-procedure-keyword-list)
            "\\)\\_>")))

(defconst gosh-const-generic-keyword-list
  '(
    "=>" "<>"
    ))

(defconst gosh-defined-generic-keyword-regexp
  (eval-when-compile
    (concat "\\(\\_<"
            (regexp-opt gosh-const-generic-keyword-list)
            "\\)\\_>")))

(defconst gosh-basic-syntax-keyword-list
  '(
    "extend" "use" "import" "provide"
    ))

(defconst gosh-basic-syntax-keyword-regexp
  (eval-when-compile
    (concat "("
            (regexp-opt gosh-basic-syntax-keyword-list t)
            "[ \t]+\\(\\(?:\\sw\\|\\.\\)+\\)?")))

(defconst gosh-regexp-literal-regexp
  ;; for `font-lock-syntactic-keywords' (Emacs 23 or earlier)
  ;; last slash or `i' must let separated subexp.
  "\\(#\\)\\(/\\)\\(\\(?:\\\\.\\|[^/]\\)*?\\)\\(?:/\\(i\\)\\|\\(/\\)\\)")

(defconst gosh-regexp-vector-prefix
  "\\(#[usUS]\\)\\(8\\|16\\|32\\|64\\)")

(defconst *gosh-scheme-r5rs-info*
  '((define (syntax (identifier value) undefined) "define a new variable")
    (set! (syntax (identifier value) undefined) "set the value of a variable")
    (let (syntax (vars body ...)) "bind new local variables in parallel")
    (let* (syntax (vars body ...)) "bind new local variables sequentially")
    (letrec (syntax (vars body ...)) "bind new local variables recursively")
    (lambda (syntax (params body ...)) "procedure syntax")
    (if (syntax (cond then else)) "conditional evaluation")
    (cond (syntax (clause ...)) "try each clause until one succeeds")
    (case (syntax (expr clause ...)) "look for EXPR among literal lists")
    (delay (syntax (expr)) "create a promise to evaluate EXPR")
    (and (syntax (expr ...)) "evaluate EXPRs while true, return last")
    (or (syntax (expr ...)) "return the first true EXPR")
    (begin (syntax (expr ...)) "evaluate each EXPR in turn and return the last")
    (do (syntax (vars finish body ...)) "simple iterator")
    (quote (syntax (expr)) "represent EXPR literally without evaluating it")
    (quasiquote (syntax (expr)) "quote literals allowing escapes")
    (unquote (syntax (expr)) "escape an expression inside quasiquote")
    (unquote-splicing (syntax (expr)) "escape and splice a list expression inside quasiquote")
    (define-syntax (syntax (identifier body ...) undefined) "create a macro")
    (let-syntax (syntax (syntaxes body ...)) "a local macro")
    (letrec-syntax (syntax (syntaxes body ...)) "a local macro")
    (syntax-rules (syntax (literals clauses ...) undefined) "simple macro language")
    (eqv? (lambda (obj1 obj2) bool) "returns #t if OBJ1 and OBJ2 are the same object")
    (eq? (lambda (obj1 obj2) bool) "finer grained version of EQV?")
    (equal? (lambda (obj1 obj2) bool) "recursive equivalence")
    (not (lambda (obj) bool) "returns #t iff OBJ is false")
    (boolean? (lambda (obj) bool) "returns #t iff OBJ is #t or #f")
    (number? (lambda (obj) bool) "returns #t iff OBJ is a number")
    (complex? (lambda (obj) bool) "returns #t iff OBJ is a complex number")
    (real? (lambda (obj) bool) "returns #t iff OBJ is a real number")
    (rational? (lambda (obj) bool) "returns #t iff OBJ is a rational number")
    (integer? (lambda (obj) bool) "returns #t iff OBJ is an integer")
    (exact? (lambda (z) bool) "returns #t iff Z is exact")
    (inexact? (lambda (z) bool) "returns #t iff Z is inexact")
    (= (lambda (z1 z2 ...) bool) "returns #t iff the arguments are all equal")
    (< (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically increasing")
    (> (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically decreasing")
    (<= (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically nondecreasing")
    (>= (lambda (x1 x2 ...) bool) "returns #t iff the arguments are monotonically nonincreasing")
    (zero? (lambda (z) bool))
    (positive? (lambda (x1) bool))
    (negative? (lambda (x1) bool))
    (odd? (lambda (n) bool))
    (even? (lambda (n) bool))
    (max (lambda (x1 x2 ...) x3) "returns the maximum of the arguments")
    (min (lambda (x1 x2 ...) x3) "returns the minimum of the arguments")
    (+ (lambda (z1 ...) z))
    (* (lambda (z1 ...) z))
    (- (lambda (z1 ...) z))
    (/ (lambda (z1 ...) z))
    (abs (lambda (x1) x2) "returns the absolute value of X")
    (quotient (lambda (n1 n2) n) "integer division")
    (remainder (lambda (n1 n2) n) "same sign as N1")
    (modulo (lambda (n1 n2) n) "same sign as N2")
    (gcd (lambda (n1 ...) n) "greatest common divisor")
    (lcm (lambda (n2 ...) n) "least common multiple")
    (numerator (lambda (rational) n))
    (denominator (lambda (rational) n))
    (floor (lambda (x1) n) "largest integer not larger than X")
    (ceiling (lambda (x1) n) "smallest integer not smaller than X")
    (truncate (lambda (x1) n) "drop fractional part")
    (round (lambda (x1) n) "round to even (banker's rounding)")
    (rationalize (lambda (x1 y) n) "rational number differing from X by at most Y")
    (exp (lambda (z) z) "e^Z")
    (log (lambda (z) z) "natural logarithm of Z")
    (sin (lambda (z) z) "sine function")
    (cos (lambda (z) z) "cosine function")
    (tan (lambda (z) z) "tangent function")
    (asin (lambda (z) z) "arcsine function")
    (acos (lambda (z) z) "arccosine function")
    (atan (lambda (z) z) "arctangent function")
    (sqrt (lambda (z) z) "principal square root of Z")
    (expt (lambda (z1 z2) z) "returns Z1 raised to the Z2 power")
    (make-rectangular (lambda (x1 x2) z) "create a complex number")
    (make-polar (lambda (x1 x2) z) "create a complex number")
    (real-part (lambda (z) x1))
    (imag-part (lambda (z) x1))
    (magnitude (lambda (z) x1))
    (angle (lambda (z) x1))
    (exact->inexact (lambda (z) z))
    (inexact->exact (lambda (z) z))
    (number->string (lambda (z :optional radix) str))
    (string->number (lambda (str :optional radix) z))
    (pair? (lambda (obj) bool) "returns #t iff OBJ is a pair")
    (cons (lambda (obj1 obj2) pair) "create a newly allocated pair")
    (car (lambda (pair) obj))
    (cdr (lambda (pair) obj))
    (set-car! (lambda (pair obj) undefined))
    (set-cdr! (lambda (pair obj) undefined))
    (caar (lambda (pair) obj))
    (cadr (lambda (pair) obj))
    (cdar (lambda (pair) obj))
    (cddr (lambda (pair) obj))
    (caaar (lambda (pair) obj))
    (caadr (lambda (pair) obj))
    (cadar (lambda (pair) obj))
    (caddr (lambda (pair) obj))
    (cdaar (lambda (pair) obj))
    (cdadr (lambda (pair) obj))
    (cddar (lambda (pair) obj))
    (cdddr (lambda (pair) obj))
    (caaaar (lambda (pair) obj))
    (caaadr (lambda (pair) obj))
    (caadar (lambda (pair) obj))
    (caaddr (lambda (pair) obj))
    (cadaar (lambda (pair) obj))
    (cadadr (lambda (pair) obj))
    (caddar (lambda (pair) obj))
    (cadddr (lambda (pair) obj))
    (cdaaar (lambda (pair) obj))
    (cdaadr (lambda (pair) obj))
    (cdadar (lambda (pair) obj))
    (cdaddr (lambda (pair) obj))
    (cddaar (lambda (pair) obj))
    (cddadr (lambda (pair) obj))
    (cdddar (lambda (pair) obj))
    (cddddr (lambda (pair) obj))
    (null? (lambda (obj) bool) "returns #t iff OBJ is the empty list")
    (list? (lambda (obj) bool) "returns #t iff OBJ is a proper list")
    (list (lambda (obj ...) list) "returns a newly allocated list")
    (length (lambda (list) n))
    (append (lambda (list ...) list) "concatenates the list arguments")
    (reverse (lambda (list) list))
    (list-tail (lambda (list k) list) "returns the Kth cdr of LIST")
    (list-ref (lambda (list k) obj) "returns the Kth element of LIST")
    (memq (lambda (obj list)) "the sublist of LIST whose car is eq? to OBJ")
    (memv (lambda (obj list)) "the sublist of LIST whose car is eqv? to OBJ")
    (member (lambda (obj list)) "the sublist of LIST whose car is equal? to OBJ")
    (assq (lambda (obj list)) "the element of LIST whose car is eq? to OBJ")
    (assv (lambda (obj list)) "the element of LIST whose car is eqv? to OBJ")
    (assoc (lambda (obj list)) "the element of LIST whose car is equal? to OBJ")
    (symbol? (lambda (obj) bool) "returns #t iff OBJ is a symbol")
    (symbol->string (lambda (symbol) str))
    (string->symbol (lambda (str) symbol))
    (char? (lambda (obj) bool) "returns #t iff OBJ is a character")
    (char=? (lambda (ch1 ch2) bool))
    (char<? (lambda (ch1 ch2) bool))
    (char>? (lambda (ch1 ch2) bool))
    (char<=? (lambda (ch1 ch2) bool))
    (char>=? (lambda (ch1 ch2) bool))
    (char-ci=? (lambda (ch1 ch2) bool))
    (char-ci<? (lambda (ch1 ch2) bool))
    (char-ci>? (lambda (ch1 ch2) bool))
    (char-ci<=? (lambda (ch1 ch2) bool))
    (char-ci>=? (lambda (ch1 ch2) bool))
    (char-alphabetic? (lambda (ch) bool))
    (char-numeric? (lambda (ch) bool))
    (char-whitespace? (lambda (ch) bool))
    (char-upper-case? (lambda (ch) bool))
    (char-lower-case? (lambda (ch) bool))
    (char->integer (lambda (ch) int))
    (integer->char (lambda (int) ch))
    (char-upcase (lambda (ch) ch))
    (char-downcase (lambda (ch) ch))
    (string? (lambda (obj) bool) "returns #t iff OBJ is a string")
    (make-string (lambda (k :optional ch) str) "a new string of length k")
    (string (lambda (ch ...) str) "a new string made of the char arguments")
    (string-length (lambda (str) n) "the number of characters in STR")
    (string-ref (lambda (str i) ch) "the Ith character of STR")
    (string-set! (lambda (str i ch) undefined) "set the Ith character of STR to CH")
    (string=? (lambda (str1 str2) bool))
    (string-ci=? (lambda (str1 str2) bool))
    (string<? (lambda (str1 str2) bool))
    (string>? (lambda (str1 str2) bool))
    (string<=? (lambda (str1 str2) bool))
    (string>=? (lambda (str1 str2) bool))
    (string-ci<? (lambda (str1 str2) bool))
    (string-ci>? (lambda (str1 str2) bool))
    (string-ci<=? (lambda (str1 str2) bool))
    (string-ci>=? (lambda (str1 str2) bool))
    (substring (lambda (str start end) str))
    (string-append (lambda (str ...) str) "concatenate the string arguments")
    (string->list (lambda (str) list))
    (list->string (lambda (list) str))
    (string-copy (lambda (str) str))
    (string-fill! (lambda (str ch) undefined) "set every char in STR to CH")
    (vector? (lambda (obj) bool) "returns #t iff OBJ is a vector")
    (make-vector (lambda (len :optional fill) vec) "a new vector of K elements")
    (vector (lambda (obj ...) vec))
    (vector-length (lambda (vec) n) "the number of elements in VEC")
    (vector-ref (lambda (vec i) obj) "the Ith element of VEC")
    (vector-set! (lambda (vec i obj) undefined) "set the Ith element of VEC to OBJ")
    (vector->list (lambda (vec) list))
    (list->vector (lambda (list) vec))
    (vector-fill! (lambda (vec obj) undefined) "set every element in VEC to OBJ")
    (procedure? (lambda (obj) bool) "returns #t iff OBJ is a procedure")
    (apply (lambda ((lambda obj a) obj ...) a) "procedure application")
    (map (lambda ((lambda (obj1 . obj2) a) list ...) (list a)) "a new list of PROC applied to every element of LIST")
    (for-each (lambda ((lambda obj a) obj ...) undefined) "apply PROC to each element of LIST in order")
    (force (lambda (promise) obj) "force the delayed value of PROMISE")
    (call-with-current-continuation (lambda (proc) obj) "goto on steroids")
    (values (lambda (obj ...)) "send multiple values to the calling continuation")
    (call-with-values (lambda (producer consumer) obj))
    (dynamic-wind (lambda (before-thunk thunk after-thunk) obj))
    (scheme-report-environment (lambda (int) env) "INT should be 5")
    (null-environment (lambda (int) env) "INT should be 5")
    (call-with-input-file (lambda (path proc) input-port))
    (call-with-output-file (lambda (path proc) output-port))
    (input-port? (lambda (obj) bool) "returns #t iff OBJ is an input port")
    (output-port? (lambda (obj) bool) "returns #t iff OBJ is an output port")
    (current-input-port (lambda () input-port) "the default input for read procedures")
    (current-output-port (lambda () output-port) "the default output for write procedures")
    (with-input-from-file (lambda (path thunk) obj))
    (with-output-to-file (lambda (path thunk) obj))
    (open-input-file (lambda (path) input-port))
    (open-output-file (lambda (path) output-port))
    (close-input-port (lambda (input-port)))
    (close-output-port (lambda (output-port)))
    (read (lambda (:optional input-port) obj) "read a datum")
    (read-char (lambda (:optional input-port) ch) "read a single character")
    (peek-char (lambda (:optional input-port) ch))
    (eof-object? (lambda (obj) bool) "returns #t iff OBJ is the end-of-file object")
    (char-ready? (lambda (:optional input-port) bool))
    (write (lambda (object :optional output-port) undefined) "write a datum")
    (display (lambda (object :optional output-port) undefined) "display")
    (newline (lambda (:optional output-port) undefined) "send a linefeed")
    (write-char (lambda (char :optional output-port) undefined) "write a single character")
    (load (lambda (filename) undefined) "evaluate expressions from a file")
    (eval (lambda (expr env)))
    ))

(defconst *gosh-scheme-srfi-info*
  [
   ;; SRFI 0
   ("Feature-based conditional expansion construct"
    (cond-expand (syntax (clause ...))))

   ;; SRFI 1
   ("List Library"
    (xcons (lambda (object object) pair))
    (cons* (lambda (object ...) pair))
    (make-list (lambda (integer :optional object) list))
    (list-tabulate (lambda (integer procedure) list))
    (list-copy (lambda (list) list))
    (circular-list (lambda (object ...) list))
    (iota (lambda (integer :optional integer integer) list))
    (proper-list? (lambda (object) bool))
    (circular-list? (lambda (object) bool))
    (dotted-list? (lambda (object) bool))
    (not-pair? (lambda (object) bool))
    (null-list? (lambda (object) bool))
    (list= (lambda (procedure list ...) bool))
    (first (lambda (pair)))
    (second (lambda (pair)))
    (third (lambda (pair)))
    (fourth (lambda (pair)))
    (fifth (lambda (pair)))
    (sixth (lambda (pair)))
    (seventh (lambda (pair)))
    (eighth (lambda (pair)))
    (ninth (lambda (pair)))
    (tenth (lambda (pair)))
    (car+cdr (lambda (pair)))
    (take (lambda (pair integer) list))
    (drop (lambda (pair integer) list))
    (take-right (lambda (pair integer) list))
    (drop-right (lambda (pair integer) list))
    (take! (lambda (pair integer) list))
    (drop-right! (lambda (pair integer) list))
    (split-at (lambda (pair integer) list))
    (split-at! (lambda (pair integer) list))
    (last (lambda (pair) obj))
    (last-pair (lambda (pair) pair))
    (length+ (lambda (object) n))
    (concatenate (lambda (list) list))
    (append! (lambda (list ...) list))
    (concatenate! (lambda (list) list))
    (reverse! (lambda (list) list))
    (append-reverse (lambda (list list) list))
    (append-reverse! (lambda (list list) list))
    (zip (lambda (list ...) list))
    (unzip1 (lambda (list) list))
    (unzip2 (lambda (list) list))
    (unzip3 (lambda (list) list))
    (unzip4 (lambda (list) list))
    (unzip5 (lambda (list) list))
    (count (lambda ((lambda (obj1 . obj2)) list ...) n))
    (fold (lambda ((lambda (obj1 obj2 . obj3) a) object list ...) a))
    (unfold (lambda (procedure procedure procedure object :optional procedure) obj))
    (pair-fold (lambda ((lambda obj a) object list ...) a))
    (reduce (lambda ((lambda (obj1 obj2 . obj3) a) object list ...) a))
    (fold-right (lambda ((lambda (obj1 obj2 . obj3) a) object list ...) a))
    (unfold-right (lambda (procedure procedure procedure object :optional object) obj))
    (pair-fold-right (lambda ((lambda (obj1 obj2 . obj3) a) object list ...) a))
    (reduce-right (lambda ((lambda (obj1 obj2 . obj3) a) object list ...) a))
    (append-map (lambda ((lambda (obj1 . obj2)) list ...) list))
    (append-map! (lambda ((lambda (obj1 . obj2)) list ...) list))
    (map! (lambda ((lambda (obj1 . obj2)) list ...) list))
    (pair-for-each (lambda ((lambda (obj1 . obj2)) list ...) undefined))
    (filter-map (lambda ((lambda (obj1 . obj2)) list ...) list))
    (map-in-order (lambda ((lambda (obj1 . obj2)) list ...) list))
    (filter (lambda ((lambda (obj1 . obj2)) list) list))
    (partition (lambda ((lambda (obj) bool) list) list))
    (remove (lambda ((lambda (obj1) bool) list) list))
    (filter! (lambda ((lambda (obj1) bool) list) list))
    (partition! (lambda ((lambda (obj1) bool) list) list))
    (remove! (lambda ((lambda (obj1) bool) list) list))
    (find (lambda ((lambda (obj1) bool) list) obj))
    (find-tail (lambda ((lambda (obj1) bool) list) obj))
    (any (lambda ((lambda (obj1 . obj2) a) list ...) a))
    (every (lambda ((lambda (obj1 . obj2) a) list ...) a))
    (list-index (lambda ((lambda (obj1 . obj2)) list ...) (or bool integer)))
    (take-while (lambda ((lambda (obj)) list) list))
    (drop-while (lambda ((lambda (obj)) list) list))
    (take-while! (lambda ((lambda (obj)) list) list))
    (span (lambda ((lambda (obj)) list) list))
    (break (lambda ((lambda (obj)) list) list))
    (span! (lambda ((lambda (obj)) list) list))
    (break! (lambda ((lambda (obj)) list) list))
    (delete (lambda (object list :optional procedure) list))
    (delete-duplicates (lambda (list :optional procedure) list))
    (delete! (lambda (obj list :optional procedure) list))
    (delete-duplicates! (lambda (list :optional procedure) list))
    (alist-cons (lambda (obj1 obj2 alist) alist))
    (alist-copy (lambda (alist) alist))
    (alist-delete (lambda (obj alist) alist))
    (alist-delete! (lambda (obj alist) alist))
    (lset<= (lambda (procedure list ...) bool))
    (lset= (lambda (procedure list ...) bool))
    (lset-adjoin (lambda (procedure list object ...) list))
    (lset-union (lambda (procedure list ...) list))
    (lset-union! (lambda (procedure list ...) list))
    (lset-intersection (lambda (procedure list ...) list))
    (lset-intersection! (lambda (procedure list ...) list))
    (lset-difference (lambda (procedure list ...) list))
    (lset-difference! (lambda (procedure list ...) list))
    (lset-xor (lambda (procedure list ...) list))
    (lset-xor! (lambda (procedure list ...) list))
    (lset-diff+intersection (lambda (procedure list ...) list))
    (lset-diff+intersection! (lambda (procedure list ...) list))

    )

   ;; SRFI 2
   ("AND-LET*: an AND with local bindings, a guarded LET* special form"
    (and-let* (syntax (bindings body ...))))

   ()

   ;; SRFI 4
   ("Homogeneous numeric vector datatypes"

    (u8vector? (lambda (obj) bool))
    (make-u8vector (lambda (size integer) u8vector))
    (u8vector (lambda (integer ...) u8vector))
    (u8vector-length (lambda (u8vector) n))
    (u8vector-ref (lambda (u8vector i) int))
    (u8vector-set! (lambda (u8vector i u8value) undefined))
    (u8vector->list (lambda (u8vector) list))
    (list->u8vector (lambda (list) u8vector))

    (s8vector? (lambda (obj) bool))
    (make-s8vector (lambda (size integer) s8vector))
    (s8vector (lambda (integer ...) s8vector))
    (s8vector-length (lambda (s8vector) n))
    (s8vector-ref (lambda (s8vector i) int))
    (s8vector-set! (lambda (s8vector i s8value) undefined))
    (s8vector->list (lambda (s8vector) list))
    (list->s8vector (lambda (list) s8vector))

    (u16vector? (lambda (obj) bool))
    (make-u16vector (lambda (size integer) u16vector))
    (u16vector (lambda (integer ...)))
    (u16vector-length (lambda (u16vector) n))
    (u16vector-ref (lambda (u16vector i) int))
    (u16vector-set! (lambda (u16vector i u16value) undefined))
    (u16vector->list (lambda (u16vector) list))
    (list->u16vector (lambda (list) u16vector))

    (s16vector? (lambda (obj) bool))
    (make-s16vector (lambda (size integer) s16vector))
    (s16vector (lambda (integer ...) s16vector))
    (s16vector-length (lambda (s16vector) n))
    (s16vector-ref (lambda (s16vector i) int))
    (s16vector-set! (lambda (s16vector i s16value) undefined))
    (s16vector->list (lambda (s16vector) list))
    (list->s16vector (lambda (list) s16vector))

    (u32vector? (lambda (obj) bool))
    (make-u32vector (lambda (size integer) u32vector))
    (u32vector (lambda (integer ...) u32vector))
    (u32vector-length (lambda (u32vector) n))
    (u32vector-ref (lambda (u32vector i) int))
    (u32vector-set! (lambda (u32vector i u32value) undefined))
    (u32vector->list (lambda (u32vector) list))
    (list->u32vector (lambda (list) u32vector))

    (s32vector? (lambda (obj) bool))
    (make-s32vector (lambda (size integer) s32vector))
    (s32vector (lambda (integer ...) s32vector))
    (s32vector-length (lambda (s32vector) n))
    (s32vector-ref (lambda (s32vector i) int))
    (s32vector-set! (lambda (s32vector i s32value) undefined))
    (s32vector->list (lambda (s32vector) list))
    (list->s32vector (lambda (list) s32vector))

    (u64vector? (lambda (obj) bool))
    (make-u64vector (lambda (size integer) u64vector))
    (u64vector (lambda (integer ...) u64vector))
    (u64vector-length (lambda (u64vector) n))
    (u64vector-ref (lambda (u64vector i) int))
    (u64vector-set! (lambda (u64vector i u64value) undefined))
    (u64vector->list (lambda (u64vector) list))
    (list->u64vector (lambda (list) u64vector))

    (s64vector? (lambda (obj) bool))
    (make-s64vector (lambda (size integer) s64vector))
    (s64vector (lambda (integer ...) s64vector))
    (s64vector-length (lambda (s64vector) n))
    (s64vector-ref (lambda (s64vector i) int))
    (s64vector-set! (lambda (s64vector i s64value) undefined))
    (s64vector->list (lambda (s64vector) list))
    (list->s64vector (lambda (list) s64vector))

    (f32vector? (lambda (obj) bool))
    (make-f32vector (lambda (size integer) f32vector))
    (f32vector (lambda (number ...) f32vector))
    (f32vector-length (lambda (f32vector) n))
    (f32vector-ref (lambda (f32vector i) int))
    (f32vector-set! (lambda (f32vector i f32value) undefined))
    (f32vector->list (lambda (f32vector) list))
    (list->f32vector (lambda (list) f32vector))

    (f64vector? (lambda (obj) bool))
    (make-f64vector (lambda (size integer) f64vector))
    (f64vector (lambda (number ...) f64vector))
    (f64vector-length (lambda (f64vector) n))
    (f64vector-ref (lambda (f64vector i) int))
    (f64vector-set! (lambda (f64vector i f64value) undefined))
    (f64vector->list (lambda (f64vector) list))
    (list->f64vector (lambda (list) f64vector))
    )

   ;; SRFI 5
   ("A compatible let form with signatures and rest arguments"
    (let (syntax (bindings body ...))))

   ;; SRFI 6
   ("Basic String Ports"
    (open-input-string (lambda (str) input-port))
    (open-output-string (lambda () output-port))
    (get-output-string (lambda (output-port) str)))

   ;; SRFI 7
   ("Feature-based program configuration language"
    (program (syntax (clause ...)))
    (feature-cond (syntax (clause))))

   ;; SRFI 8
   ("receive: Binding to multiple values"
    (receive (syntax (identifiers producer body ...))))

   ;; SRFI 9
   ("Defining Record Types"
    (define-record-type (syntax (name constructor-name pred-name fields ...))))

   ;; SRFI 10
   ("Sharp-Comma External Form"
    (define-reader-ctor (syntax (name proc) undefined)))

   ;; SRFI 11
   ("Syntax for receiving multiple values"
    (let-values (syntax (bindings body ...)))
    (let-values* (syntax (bindings body ...))))

   ()

   ;; SRFI 13
   ("String Library"
    (string-map (lambda (proc str :optional start end) str))
    (string-map! (lambda (proc str :optional start end) undefined))
    (string-fold (lambda (kons knil str :optional start end) obj))
    (string-fold-right (lambda (kons knil str :optional start end) obj))
    (string-unfold (lambda (p f g seed :optional base make-final) str))
    (string-unfold-right (lambda (p f g seed :optional base make-final) str))
    (string-tabulate (lambda (proc len) str))
    (string-for-each (lambda (proc str :optional start end) undefined))
    (string-for-each-index (lambda (proc str :optional start end) undefined))
    (string-every (lambda (pred str :optional start end) obj))
    (string-any (lambda (pred str :optional start end) obj))
    (string-hash (lambda (str :optional bound start end) int))
    (string-hash-ci (lambda (str :optional bound start end) int))
    (string-compare (lambda (string1 string2 lt-proc eq-proc gt-proc :optional start end) obj))
    (string-compare-ci (lambda (string1 string2 lt-proc eq-proc gt-proc :optional start end) obj))
    (string= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string<> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string< (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string<= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string>= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci<> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci< (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci> (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci<= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-ci>= (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-titlecase (lambda (string :optional start end) str))
    (string-upcase (lambda (string :optional start end) str))
    (string-downcase (lambda (string :optional start end) str))
    (string-titlecase! (lambda (string :optional start end) undefined))
    (string-upcase! (lambda (string :optional start end) undefined))
    (string-downcase! (lambda (string :optional start end) undefined))
    (string-take (lambda (string nchars) str))
    (string-drop (lambda (string nchars) str))
    (string-take-right (lambda (string nchars) str))
    (string-drop-right (lambda (string nchars) str))
    (string-pad (lambda (string k :optional char start end) str))
    (string-pad-right (lambda (string k :optional char start end) str))
    (string-trim (lambda (string :optional char/char-set/pred start end) str))
    (string-trim-right (lambda (string :optional char/char-set/pred start end) str))
    (string-trim-both (lambda (string :optional char/char-set/pred start end) str))
    (string-filter (lambda (char/char-set/pred string :optional start end) str))
    (string-delete (lambda (char/char-set/pred string :optional start end) str))
    (string-index (lambda (string char/char-set/pred :optional start end) (or integer bool)))
    (string-index-right (lambda (string char/char-set/pred :optional end start) (or integer bool)))
    (string-skip (lambda (string char/char-set/pred :optional start end) (or integer bool)))
    (string-skip-right (lambda (string char/char-set/pred :optional end start) (or integer bool)))
    (string-count (lambda (string char/char-set/pred :optional start end) n))
    (string-prefix-length (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-suffix-length (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-prefix-length-ci (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-suffix-length-ci (lambda (string1 string2 :optional start1 end1 start2 end2) n))
    (string-prefix? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-suffix? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-prefix-ci? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-suffix-ci? (lambda (string1 string2 :optional start1 end1 start2 end2) bool))
    (string-contains (lambda (string pattern :optional s-start s-end p-start p-end) obj))
    (string-contains-ci (lambda (string pattern :optional s-start s-end p-start p-end) obj))
    (string-fill! (lambda (string char :optional start end) undefined))
    (string-copy! (lambda (to tstart from :optional fstart fend) undefined))
    (string-copy (lambda (str :optional start end) str))
    (substring/shared (lambda (str start :optional end) str))
    (string-reverse (lambda (str :optional start end) str))
    (string-reverse! (lambda (str :optional start end) undefined))
    (reverse-list->string (lambda (char-list) str))
    (string->list (lambda (str :optional start end) list))
    (string-concatenate (lambda (string-list) str))
    (string-concatenate/shared (lambda (string-list) str))
    (string-append/shared (lambda (str ...) str))
    (string-concatenate-reverse (lambda (string-list :optional final-string end) str))
    (string-concatenate-reverse/shared (lambda (string-list :optional final-string end) str))
    (xsubstring (lambda (str from :optional to start end) str))
    (string-xcopy! (lambda (target tstart str from :optional to start end) undefined))
    (string-null? (lambda (str) bool))
    (string-join (lambda (string-list :optional delim grammar) str))
    (string-tokenize (lambda (string :optional token-chars start end) str))
    (string-replace (lambda (str1 str2 start1 end1 :optional start2 end2) str))
    (string-kmp-partial-search (lambda (pat rv str i :optional c= p-start s-start s-end) n))
    (make-kmp-restart-vector (lambda (str :optional c= start end) vec))
    (kmp-step (lambda (pat rv c i c= p-start) n))
    )

   ;; SRFI 14
   ("Character-Set Library"
    (char-set? (lambda (cset) bool))
    (char-set= (lambda (cset ...) bool))
    (char-set<= (lambda (cset ...) bool))
    (char-set-hash (lambda (cset :optional int) int))
    (char-set-cursor (lambda (cset) cursor))
    (char-set-ref (lambda (cset cursor) ch))
    (char-set-cursor-next (lambda (cset cursor) int))
    (end-of-char-set? (lambda (cursor) bool))
    (char-set-fold (lambda (proc obj cset) obj))
    (char-set-unfold (lambda (proc proc proc obj :optional obj) cset))
    (char-set-unfold! (lambda (proc proc proc obj obj) cset))
    (char-set-for-each (lambda (proc cset) undefined))
    (char-set-map (lambda (proc cset) cset))
    (char-set-copy (lambda (cset) cset))
    (char-set (lambda (ch ...) cset))
    (list->char-set (lambda (list :optional obj) cset))
    (list->char-set! (lambda (list cset) cset))
    (string->char-set (lambda (str :optional cset) cset))
    (string->char-set! (lambda (str cset) cset))
    (ucs-range->char-set (lambda (int int :optional bool cset) cset))
    (ucs-range->char-set! (lambda (int int bool cset) cset))
    (char-set-filter (lambda (proc cset :optional base-cset) cset))
    (char-set-filter! (lambda (proc cset base-cset) cset))
    (->char-set (lambda (obj) cset))
    (char-set-size (lambda (cset) n))
    (char-set-count (lambda (proc cset) n))
    (char-set-contains? (lambda (cset ch) bool))
    (char-set-every (lambda (proc cset) obj))
    (char-set-any (lambda (proc cset) obj))
    (char-set-adjoin (lambda (cset ch ...) cset))
    (char-set-delete (lambda (cset ch ...) cset))
    (char-set-adjoin! (lambda (cset ch ...) cset))
    (char-set-delete! (lambda (cset ch ...) cset))
    (char-set->list (lambda (cset) list))
    (char-set->string (lambda (cset) str))
    (char-set-complement (lambda (cset) cset))
    (char-set-union (lambda (cset ...) cset))
    (char-set-intersection (lambda (cset ...) cset))
    (char-set-xor (lambda (cset ...) cset))
    (char-set-difference (lambda (cset ...) cset))
    (char-set-diff+intersection (lambda (cset ...) cset))
    (char-set-complement! (lambda (cset) cset))
    (char-set-union! (lambda (cset ...) cset))
    (char-set-intersection! (lambda (cset ...) cset))
    (char-set-xor! (lambda (cset ...) cset))
    (char-set-difference! (lambda (cset ...) cset))
    (char-set-diff+intersection! (lambda (cset ...) cset))
    (char-set:lower-case char-set)
    (char-set:upper-case char-set)
    (char-set:letter char-set)
    (char-set:digit char-set)
    (char-set:letter+digit char-set)
    (char-set:graphic char-set)
    (char-set:printing char-set)
    (char-set:whitespace char-set)
    (char-set:blank char-set)
    (char-set:iso-control char-set)
    (char-set:punctuation char-set)
    (char-set:symbol char-set)
    (char-set:hex-digit char-set)
    (char-set:ascii char-set)
    (char-set:empty char-set)
    (char-set:full char-set)
    )

   ()

   ;; SRFI 16
   ("Syntax for procedures of variable arity"
    (case-lambda (syntax (clauses ...) procedure)))

   ;; SRFI 17
   ("Generalized set!"
    (set! (syntax (what value) undefined)))

   ;; SRFI 18
   ("Multithreading support"
    (current-thread (lambda () thread))
    (thread? (lambda (obj) bool))
    (make-thread (lambda (thunk :optional name) thread))
    (thread-name (lambda (thread) name))
    (thread-specific (lambda (thread)))
    (thread-specific-set! (lambda (thread obj)))
    (thread-base-priority (lambda (thread)))
    (thread-base-priority-set! (lambda (thread number)))
    (thread-priority-boost (lambda (thread)))
    (thread-priority-boost-set! (lambda (thread number)))
    (thread-quantum (lambda (thread)))
    (thread-quantum-set! (lambda (thread number)))
    (thread-start! (lambda (thread)))
    (thread-yield! (lambda ()))
    (thread-sleep! (lambda (number)))
    (thread-terminate! (lambda (thread)))
    (thread-join! (lambda (thread :optional timeout timeout-val)))
    (mutex? (lambda (obj) bool))
    (make-mutex (lambda (:optional name) mutex))
    (mutex-name (lambda (mutex) name))
    (mutex-specific (lambda (mutex)))
    (mutex-specific-set! (lambda (mutex obj)))
    (mutex-state (lambda (mutex)))
    (mutex-lock! (lambda (mutex :optional timeout thread)))
    (mutex-unlock! (lambda (mutex :optional condition-variable timeout)))
    (condition-variable? (lambda (obj) bool))
    (make-condition-variable (lambda (:optional name) condition-variable))
    (condition-variable-name (lambda (condition-variable) name))
    (condition-variable-specific (lambda (condition-variable)))
    (condition-variable-specific-set! (lambda (condition-variable obj)))
    (condition-variable-signal! (lambda (condition-variable)))
    (condition-variable-broadcast! (lambda (condition-variable)))
    (current-time (lambda () time))
    (time? (lambda (obj) bool))
    (time->seconds (lambda (time) x1))
    (seconds->time (lambda (x1) time))
    (current-exception-handler (lambda () handler))
    (with-exception-handler (lambda (handler thunk)))
    (raise (lambda (obj)))
    (join-timeout-exception? (lambda (obj) bool))
    (abandoned-mutex-exception? (lambda (obj) bool))
    (terminated-thread-exception? (lambda (obj) bool))
    (uncaught-exception? (lambda (obj) bool))
    (uncaught-exception-reason (lambda (exc) obj))
    )

   ;; SRFI 19
   ("Time Data Types and Procedures"
    (current-date (lambda (:optional tz-offset)) date)
    (current-julian-day (lambda ()) jdn)
    (current-modified-julian-day (lambda ()) mjdn)
    (current-time (lambda (:optional time-type)) time)
    (time-resolution (lambda (:optional time-type)) nanoseconds)
    (make-time (lambda (type nanosecond second)))
    (time? (lambda (obj)))
    (time-type (lambda (time)))
    (time-nanosecond (lambda (time)))
    (time-second (lambda (time)))
    (set-time-type! (lambda (time)))
    (set-time-nanosecond! (lambda (time)))
    (set-time-second! (lambda (time)))
    (copy-time (lambda (time)))
    (time<=? (lambda (time1 time2)))
    (time<? (lambda (time1 time2)))
    (time=? (lambda (time1 time2)))
    (time>=? (lambda (time1 time2)))
    (time>? (lambda (time1 time2)))
    (time-difference (lambda (time1 time2)))
    (time-difference! (lambda (time1 time2)))
    (add-duration (lambda (time duration)))
    (add-duration! (lambda (time duration)))
    (subtract-duration (lambda (time duration)))
    (subtract-duration! (lambda (time duration)))
    (make-date (lambda (nanosecond second minute hour day month year zone-offset)))
    (date? (lambda (obj)))
    (date-nanosecond (lambda (date)))
    (date-second (lambda (date)))
    (date-minute (lambda (date)))
    (date-hour (lambda (date)))
    (date-day (lambda (date)))
    (date-month (lambda (date)))
    (date-year (lambda (date)))
    (date-zone-offset (lambda (date)))
    (date-year-day (lambda (date)))
    (date-week-day (lambda (date)))
    (date-week-number (lambda (date)))
    (date->julian-day (lambda (date)))
    (date->modified-julian-day (lambda (date)))
    (date->time-monotonic (lambda (date)))
    (date->time-tai (lambda (date)))
    (date->time-utc (lambda (date)))
    (julian-day->date (lambda (date)))
    (julian-day->time-monotonic (lambda (date)))
    (julian-day->time-tai (lambda (date)))
    (julian-day->time-utc (lambda (date)))
    (modified-julian-day->date (lambda (date)))
    (modified-julian-day->time-monotonic (lambda (date)))
    (modified-julian-day->time-tai (lambda (date)))
    (modified-julian-day->time-utc (lambda (date)))
    (time-monotonic->date (lambda (date)))
    (time-monotonic->julian-day (lambda (date)))
    (time-monotonic->modified-julian-day (lambda (date)))
    (time-monotonic->time-monotonic (lambda (date)))
    (time-monotonic->time-tai (lambda (date)))
    (time-monotonic->time-tai! (lambda (date)))
    (time-monotonic->time-utc (lambda (date)))
    (time-monotonic->time-utc! (lambda (date)))
    (time-tai->date (lambda (date)))
    (time-tai->julian-day (lambda (date)))
    (time-tai->modified-julian-day (lambda (date)))
    (time-tai->time-monotonic (lambda (date)))
    (time-tai->time-monotonic! (lambda (date)))
    (time-tai->time-utc (lambda (date)))
    (time-tai->time-utc! (lambda (date)))
    (time-utc->date (lambda (date)))
    (time-utc->julian-day (lambda (date)))
    (time-utc->modified-julian-day (lambda (date)))
    (time-utc->time-monotonic (lambda (date)))
    (time-utc->time-monotonic! (lambda (date)))
    (time-utc->time-tai (lambda (date)))
    (time-utc->time-tai! (lambda (date)))
    (date->string (lambda (date :optional format-string)))
    (string->date (lambda (input-string template-string)))
    )

   ()

   ;; SRFI 21
   ("Real-time multithreading support"
    srfi-18)                            ; same as srfi-18

   ;; SRFI 22
   ("Running Scheme Scripts on Unix"
    )

   ;; SRFI 23
   ("Error reporting mechanism"
    (error (lambda (reason-string arg ...))))

   ()

   ;; SRFI 25
   ("Multi-dimensional Array Primitives"
    (array? (lambda (obj)))
    (make-array (lambda (shape :optional init)))
    (shape (lambda (bound ...)))
    (array (lambda (shape obj ...)))
    (array-rank (lambda (array)))
    (array-start (lambda (array)))
    (array-end (lambda (array)))
    (array-shape (lambda (array)))
    (array-ref (lambda (array i ...)))
    (array-set! (lambda (array obj ...) undefined))
    (share-array (lambda (array shape proc)))
    )

   ;; SRFI 26
   ("Notation for Specializing Parameters without Currying"
    (cut (syntax (obj ...)))
    (cute (lambda (obj ...))))

   ;; SRFI 27
   ("Sources of Random Bits"
    (random-integer (lambda (n)))
    (random-real (lambda ()))
    (default-random-source (lambda ()))
    (make-random-source (lambda ()))
    (random-source? (lambda (obj)))
    (random-source-state-ref (lambda (random-source)))
    (random-source-state-set! (lambda (random-source state)))
    (random-source-randomize! (lambda (random-source)))
    (random-source-pseudo-randomize! (lambda (random-source i j)))
    (random-source-make-integers (lambda (random-source)))
    (random-source-make-reals (lambda (random-source)))
    )

   ;; SRFI 28
   ("Basic Format Strings"
    (format (lambda (port-or-boolean format-string arg ...))))

   ;; SRFI 29
   ("Localization"
    (current-language (lambda (:optional symbol)))
    (current-country (lambda (:optional symbol)))
    (current-locale-details (lambda (:optional list)))
    (declare-bundle! (lambda (bundle-name association-list)))
    (store-bundle (lambda (bundle-name)))
    (load-bundle! (lambda (bundle-name)))
    (localized-template (lambda (package-name message-template-name)))
    )

   ;; SRFI 30
   ("Nested Multi-line Comments"
    )

   ;; SRFI 31
   ("A special form for recursive evaluation"
    (rec (syntax (name body ...) procedure)))

   ()

   ()

   ;; SRFI 34
   ("Exception Handling for Programs"
    (guard (syntax (clauses ...)))
    (raise (lambda (obj)))
    )

   ;; SRFI 35
   ("Conditions"
    (make-condition-type (lambda (id parent field-name-list)))
    (condition-type? (lambda (obj)))
    (make-condition (lambda (condition-type)))
    (condition? (lambda (obj)))
    (condition-has-type? (lambda (condition condition-type)))
    (condition-ref (lambda (condition field-name)))
    (make-compound-condition (lambda (condition ...)))
    (extract-condition (lambda (condition condition-type)))
    (define-condition-type (syntax (name parent pred-name fields ...)))
    (condition (syntax (type-field-binding ...)))
    )

   ;; SRFI 36
   ("I/O Conditions"
    (&error condition)
    (&i/o-error condition)
    (&i/o-port-error condition)
    (&i/o-read-error condition)
    (&i/o-write-error condition)
    (&i/o-closed-error condition)
    (&i/o-filename-error condition)
    (&i/o-malformed-filename-error condition)
    (&i/o-file-protection-error condition)
    (&i/o-file-is-read-only-error condition)
    (&i/o-file-already-exists-error condition)
    (&i/o-no-such-file-error condition)
    )

   ;; SRFI 37
   ("args-fold: a program argument processor"
    (args-fold
     (arg-list option-list unrecognized-option-proc operand-proc seed ...))
    (option-processor (lambda (option name arg seeds ...)))
    (operand-processor (lambda (operand seeds ...)))
    (option (lambda (name-list required-arg? optional-arg? option-proc)))
    (option-names (lambda (option)))
    (option-required-arg? (lambda (option)))
    (option-optional-arg? (lambda (option)))
    (option-processor (lambda (option)))
    )

   ;; SRFI 38
   ("External Representation for Data With Shared Structure"
    (write-with-shared-structure (lambda (obj :optional port optarg)))
    (read-with-shared-structure (lambda (:optional port)))
    )

   ;; SRFI 39
   ("Parameter objects"
    (make-parameter (lambda (init-value :optional converter)))
    (parameterize (syntax (bindings body ...))))

   ;; SRFI 40
   ("A Library of Streams"
    (stream-null stream)
    (stream-cons (syntax (obj stream)))
    (stream? (lambda (obj)))
    (stream-null? (lambda (obj)))
    (stream-pair? (lambda (obj)))
    (stream-car (lambda (stream)))
    (stream-cdr (lambda (stream)))
    (stream-delay (syntax (expr)))
    (stream (lambda (obj ...)))
    (stream-unfoldn (lambda (generator-proc seed n)))
    (stream-map (lambda (proc stream ...)))
    (stream-for-each (lambda (proc stream ...) undefined))
    (stream-filter (lambda (pred stream)))
    )

   ()

   ;; SRFI 42
   ("Eager Comprehensions"
    (list-ec (syntax))
    (append-ec (syntax))
    (sum-ec (syntax))
    (min-ec (syntax))
    (max-ec (syntax))
    (any?-ec (syntax))
    (every?-ec (syntax))
    (first-ec (syntax))
    (do-ec (syntax))
    (fold-ec (syntax))
    (fold3-ec (syntax))
    (:list (syntax () undefined))
    (:string (syntax () undefined))
    (:vector (syntax () undefined))
    (:integers (syntax () undefined))
    (:range (syntax () undefined))
    (:real-range (syntax () undefined))
    (:char-range (syntax () undefined))
    (:port (syntax () undefined))
    (:do (syntax () undefined))
    (:let (syntax () undefined))
    (:parallel (syntax () undefined))
    (:while (syntax () undefined))
    (:until (syntax () undefined))
    )

   ;; SRFI 43
   ("Vector Library"
    (vector-unfold (f length initial-seed ...))
    (vector-unfold-right (lambda (f length initial-seed ...)))
    (vector-tabulate (lambda (f size)))
    (vector-copy (lambda (vec :optional start end fill)))
    (vector-reverse-copy (lambda (vec :optional start end)))
    (vector-append (lambda (vec ...)))
    (vector-concatenate (lambda (vector-list)))
    (vector-empty? (lambda (obj)))
    (vector= (lambda (eq-proc vec ...)))
    (vector-fold (lambda (kons knil vec ...)))
    (vector-fold-right (lambda (kons knil vec ...)))
    (vector-map (lambda (f vec ...)))
    (vector-map! (lambda (f vec ...)))
    (vector-for-each (lambda (f vec ...) undefined))
    (vector-count (lambda (pred vec ...)))
    (vector-index (lambda (pred vec ...)))
    (vector-index-right (lambda (pred vec ...)))
    (vector-skip (lambda (pred vec ...)))
    (vector-skip-right (lambda (pred vec ...)))
    (vector-binary-search (lambda (vec value cmp-proc)))
    (vector-any (lambda (pred vec ...)))
    (vector-every (lambda (pred vec ...)))
    (vector-swap! (lambda (vec i j) undefined))
    (vector-reverse! (lambda (vec :optional start end) undefined))
    (vector-copy! (lambda (target-vec t-start source-vec :optional start end) undefined))
    (vector-reverse-copy! (lambda (target-vec t-start source-vec :optional start end) undefined))
    (reverse-vector-to-list (lambda (vec :optional start end)))
    (reverse-list-to-vector (lambda (list)))
    )

   ;; SRFI 44
   ("Collections"
    )

   ;; SRFI 45
   ("Primitives for expressing iterative lazy algorithms"
    (delay (syntax (expr)))
    (lazy (syntax (expr)))
    (force (lambda (promise)))
    (eager (lambda (promise)))
    )

   ;; SRFI 46
   ("Basic Syntax-rules Extensions"
    (syntax-rules (syntax () undefined)))

   ;; SRFI 47
   ("Array"
    (make-array (lambda (prototype k ...)))
    (ac64 (lambda (:optional z)))
    (ac32 (lambda (:optional z)))
    (ar64 (lambda (:optional x1)))
    (ar32 (lambda (:optional x1)))
    (as64 (lambda (:optional n)))
    (as32 (lambda (:optional n)))
    (as16 (lambda (:optional n)))
    (as8 (lambda (:optional n)))
    (au64 (lambda (:optional n)))
    (au32 (lambda (:optional n)))
    (au16 (lambda (:optional n)))
    (au8 (lambda (:optional n)))
    (at1 (lambda (:optional bool)))
    (make-shared-array (lambda (array mapper k ...)))
    (array-rank (lambda (obj)))
    (array-dimensions (lambda (array)))
    (array-in-bounds? (lambda (array k ...)))
    (array-ref (lambda (array k ...)))
    (array-set! (lambda (array obj k ...)))
    )

   ;; SRFI 48
   ("Intermediate Format Strings"
    (format (lambda (port-or-boolean format-string arg ...))))

   ;; SRFI 49
   ("Indentation-sensitive syntax"
    )

   ()

   ;; SRFI 51
   ("Handling rest list"
    (rest-values (lambda (caller rest-list :optional args-number-limit default)))
    (arg-and (syntax))
    (arg-ands (syntax))
    (err-and (syntax))
    (err-ands (syntax))
    (arg-or (syntax))
    (arg-ors (syntax))
    (err-or (syntax))
    (err-ors (syntax))
    )

   ()

   ()

   ;; SRFI 54
   ("Formatting"
    (cat (lambda (obj ...))))

   ;; SRFI 55
   ("require-extension"
    (require-extension (syntax)))

   ()

   ;; SRFI 57
   ("Records"
    (define-record-type (syntax))
    (define-record-scheme (syntax))
    (record-update (syntax))
    (record-update! (syntax))
    (record-compose (syntax)))

   ;; SRFI 58
   ("Array Notation"
    )

   ;; SRFI 59
   ("Vicinity"
    (program-vicinity (lambda ()))
    (library-vicinity (lambda ()))
    (implementation-vicinity (lambda ()))
    (user-vicinity (lambda ()))
    (home-vicinity (lambda ()))
    (in-vicinity (lambda (vicinity filename)))
    (sub-vicinity (lambda (vicinity name)))
    (make-vicinity (lambda (dirname)))
    (path-vicinity (lambda (path)))
    (vicinity:suffix? (lambda (ch)))
    )

   ;; SRFI 60
   ("Integers as Bits"
    (bitwise-and (lambda (n ...) int))
    (bitwise-ior (lambda (n ...) int))
    (bitwise-xor (lambda (n ...) int))
    (bitwise-not (lambda (n) int))
    (bitwise-if (lambda (mask n m) int))
    (any-bits-set? (lambda (n m) bool))
    (bit-count (lambda (n) int))
    (integer-length (lambda (n) int))
    (first-bit-set (lambda (n) int))
    (bit-set? (lambda (i n) bool))
    (copy-bit (lambda (index n bool) int))
    (bit-field (lambda (n start end) int))
    (copy-bit-field (lambda (to-int from-int start end) int))
    (arithmetic-shift (lambda (n count) int))
    (rotate-bit-field (lambda (n count start end) int))
    (reverse-bit-field (lambda (n start end) int))
    (integer->list (lambda (k :optional len) list))
    (list->integer (lambda (list) int))
    )

   ;; SRFI 61
   ("A more general cond clause"
    (cond (syntax)))

   ;; SRFI 62
   ("S-expression comments"
    )

   ;; SRFI 63
   ("Homogeneous and Heterogeneous Arrays"
    )

   ;; SRFI 64
   ("A Scheme API for test suites"
    (test-assert (syntax))
    (test-eqv (syntax))
    (test-equal (syntax))
    (test-eq (syntax))
    (test-approximate (syntax))
    (test-error (syntax))
    (test-read-eval-string (lambda (string)))
    (test-begin (syntax (suite-name :optional count)))
    (test-end (syntax (suite-name)))
    (test-group (syntax (suite-name decl-or-expr ...)))
    (test-group-with-cleanup (syntax (suite-name decl-or-expr ...)))
    (test-match-name (lambda (name)))
    (test-match-nth (lambda (n :optional count)))
    (test-match-any (lambda (specifier ...)))
    (test-match-all (lambda (specifier ...)))
    (test-skip (syntax (specifier)))
    (test-expect-fail (syntax (specifier)))
    (test-runner? (lambda (obj)))
    (test-runner-current (lambda (:optional runner)))
    (test-runner-get (lambda ()))
    (test-runner-simple (lambda ()))
    (test-runner-null (lambda ()))
    (test-runner-create (lambda ()))
    (test-runner-factory (lambda (:optional factory)))
    (test-apply (syntax (runner specifier ...)))
    (test-with-runner (syntax (runner decl-or-expr ...)))
    (test-result-kind (lambda (:optional runner)))
    (test-passed? (lambda (:optional runner)))
    (test-result-ref (lambda (runner prop-name (:optional default))))
    (test-result-set! (lambda (runner prop-name value)))
    (test-result-remove (lambda (runner prop-name)))
    (test-result-clear (lambda (runner)))
    (test-result-alist (lambda (runner)))
    (test-runner-on-test-begin (lambda (runner :optional proc)))
    (test-runner-on-test-begin! (lambda (runner :optional proc)))
    (test-runner-on-test-end (lambda (runner :optional proc)))
    (test-runner-on-test-end! (lambda (runner :optional proc)))
    (test-runner-on-group-begin (lambda (runner :optional proc)))
    (test-runner-on-group-begin! (lambda (runner :optional proc)))
    (test-runner-on-group-end (lambda (runner :optional proc)))
    (test-runner-on-group-end! (lambda (runner :optional proc)))
    (test-runner-on-bad-count (lambda (runner :optional proc)))
    (test-runner-on-bad-count! (lambda (runner :optional proc)))
    (test-runner-on-bad-end-name (lambda (runner :optional proc)))
    (test-runner-on-bad-end-name! (lambda (runner :optional proc)))
    (test-runner-on-final (lambda (runner :optional proc)))
    (test-runner-on-final! (lambda (runner :optional proc)))
    (test-runner-pass-count (lambda (runner)))
    (test-runner-fail-count (lambda (runner)))
    (test-runner-xpass-count (lambda (runner)))
    (test-runner-skip-count (lambda (runner)))
    (test-runner-test-name (lambda (runner)))
    (test-runner-group-path (lambda (runner)))
    (test-runner-group-stack (lambda (runner)))
    (test-runner-aux-value (lambda (runner)))
    (test-runner-aux-value! (lambda (runner)))
    (test-runner-reset (lambda (runner)))
    )

   ()

   ;; SRFI 66
   ("Octet Vectors"
    (make-u8vector (lambda (len n)))
    (u8vector (lambda (n ...)))
    (u8vector->list (lambda (u8vector)))
    (list->u8vector (lambda (octet-list)))
    (u8vector-length u8vector)
    (u8vector-ref (lambda (u8vector k)))
    (u8vector-set! (lambda (u8vector k n)))
    (u8vector=? (lambda (u8vector-1 u8vector-2)))
    (u8vector-compare (lambda (u8vector-1 u8vector-2)))
    (u8vector-copy! (lambda (source source-start target target-start n)))
    (u8vector-copy (lambda (u8vector)))
    )

   ;; SRFI 67
   ("Compare Procedures"
    )

   ()

   ;; SRFI 69
   ("Basic hash tables"
    (alist->hash-table (lambda (alist) hash-table))
    (hash (lambda (obj :optional n) int))
    (hash-by-identity (lambda (obj :optional n) int))
    (hash-table->alist (lambda (hash-table) alist))
    (hash-table-copy (lambda (hash-table) hash-table))
    (hash-table-delete! (lambda (hash-table key) undefined))
    (hash-table-equivalence-function (lambda (hash-table) pred))
    (hash-table-exists? (lambda (hash-table key) bool))
    (hash-table-fold (lambda (hash-table f init-value)))
    (hash-table-hash-function (lambda (hash-table) f))
    (hash-table-keys (lambda (hash-table) list))
    (hash-table-merge! (lambda (hash-table1 hash-table2) undefined))
    (hash-table-ref (lambda (hash-table key :optional thunk)))
    (hash-table-ref/default (lambda (hash-table key default)))
    (hash-table-remove! (lambda (hash-table proc) undefined))
    (hash-table-set! (lambda (hash-table key value) undefined))
    (hash-table-size (lambda (hash-table) n))
    (hash-table-update! (lambda (hash-table key proc :optional thunk) undefined))
    (hash-table-update!/default (lambda (hash-table key proc default) undefined))
    (hash-table-values (lambda (hash-table) list))
    (hash-table-walk (lambda (hash-table proc) undefined))
    (hash-table? (lambda (obj) bool))
    (make-hash-table (lambda (:optional eq-fn hash-fn) hash-table))
    (string-ci-hash (lambda (str :optional n) n))
    (string-hash (lambda (str1 :optional n) n))
    )

   ;; SRFI 70
   ("Numbers"
    )

   ;; SRFI 71
   ("LET-syntax for multiple values"
    )

   ;; SRFI 72
   ("Simple hygienic macros"
    )

   ()

   ;; SRFI 74
   ("Octet-Addressed Binary Blocks"
    )

   ])

(defvar *gosh-undocumented-info*
  '((report-error (lambda (err)))))



(defconst gosh-info-appendixes-ja
  '(
    ("(gauche-refj.info)Index - 手続きと構文索引" nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - モジュール索引"   nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - クラス索引"      nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - 変数索引"        nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)))

(defconst gosh-info-appendixes-en
  '(("(gauche-refe.info)Function and Syntax Index" nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refe.info)Module Index"   nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refe.info)Class Index"      nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refe.info)Variable Index"        nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)))

(defvar gosh-info-appendixes
  (if (and current-language-environment
           (string= current-language-environment "Japanese"))
      gosh-info-appendixes-ja
    gosh-info-appendixes-en))


(provide 'gosh-const)

;;; gosh-const.el ends here
