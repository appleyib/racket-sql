#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
<Name>, <CDF>, <ID>
<Name>, <CDF>, <ID>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
;(define (And x y) (and x y)) 
;(define (Or x y) (or x y))
;(define (If x y z) (if x y z))
;
; Correction Oct 5 2016
(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c))))

(define-syntax Or
  (syntax-rules ()
  ((Or a b)
   (If a #t b))))

(define-syntax And
  (syntax-rules ()
  ((And a b)
  (If a b #f))))
; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.
  

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (first table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table))

  
#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  ((lambda(x)(- x 1)) (length table)))

(define t1 '(("a" "b" "c")(1 2 3)(4 5 6)(7 8 9)))

; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (h1 atts att tup)
  (if (equal? att (first atts))
      (first tup)
      (h1 (rest atts) att (rest tup))))

;(h1 '("a" "b" "c") "a" '(1 2 3))

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (satisfy? f table)(filter f (tuples table)))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x a) (lambda(tuple) (if (sameAttrInList? x a) (h1 a x tuple) x)))


; select and its helper functions start
(define (GetOneColumn att table atts)
  (if (empty? table)
      '()
      (cons (list (h1 atts att (attributes table)))  (GetOneColumn att (rest table) atts))))

;(GetOneColumn "b" t1 (attributes t1))

(define (CombineTwoList l1 l2)
  (if (empty? l2)
      l1
      (cons (append (first l1) (first l2)) (CombineTwoList (rest l1) (rest l2)))))

;(CombineTwoList '(("b") (2) (5) (8)) '(("c") (3) (6) (9)))

(define (CombineAllColumn atts table)
  (if (empty? atts)
      '()
      (CombineTwoList (GetOneColumn (first atts) table (attributes table)) (CombineAllColumn (rest atts) table))))

;(CombineAllColumn '("b" "c") t1)

(define (select attlst table)
  (if (equal? "*" attlst)
      (select (attributes table) table)
      (if (equal? attlst (attributes table))
          table
          (CombineAllColumn attlst table))))

;(select "*" t1)
;(select '("a") t1)




;-------------------------------------------
; FROM and its helper function starts here
(define (sameAttrInList? attr headTuple)
  (If (equal? (length headTuple) 1)
      (equal? (first headTuple) attr)
      (If (equal? (first headTuple) attr)
          #t
          (sameAttrInList? attr (rest headTuple))
          )
      )
  )

(define (sameAttrExist? attr tableHeadList)
  (If (equal? (length tableHeadList) 1)
      (sameAttrInList? attr (first tableHeadList))
      (If (sameAttrInList? attr (first tableHeadList))
          #t
          (sameAttrExist? attr (rest tableHeadList))
          )
      )
  )
 
(define (getAttrs tables) (foldl (lambda(x y) (append y (list(attributes x)))) (list) tables))

(define (getTuples tables) (foldl (lambda(x y) (append y (list(rest x)))) (list) tables))

(define (changeNameAttr tableName Attr tableHeads)
  (If (sameAttrExist? Attr tableHeads)
      (string-append tableName "." Attr)
      Attr
      )
  )
   
(define (changeAllAttrInList tableName AttrList tableHeads) (map (lambda(x) (changeNameAttr tableName x tableHeads)) AttrList))

(define (removeDuplicateHead tableName tableNameList tableHeads)
  (cond
    [(equal? 0 (length tableNameList)) (list)]
    [(equal? tableName (first tableNameList)) (rest tableHeads)]
    [else (append (list (first tableHeads)) (removeDuplicateHead tableName (rest tableNameList) (rest tableHeads)))]
    )
  )

(define (changeAllAttrs tableNameList tableHeads)
  (If (equal? (length tableHeads) 1)
      tableHeads
      (map (lambda(tableName tableHead) (changeAllAttrInList tableName tableHead (removeDuplicateHead  tableName tableNameList tableHeads))) tableNameList tableHeads)
  ))

(define (makeResultHead tableNameList tableHeads) (apply append (changeAllAttrs tableNameList tableHeads)))

(define (cartesian-product table1 table2) (foldr append (list) (map (lambda(x) (map (lambda(y) (append x y)) table2)) table1)))

(define (from tableNameList tables ) (append (list (makeResultHead tableNameList (getAttrs tables))) (foldr cartesian-product (list(list)) (getTuples tables))))

;(removeDuplicateHead  tableName tableNameList tableHeads)
;(removeDuplicateHead  "123" (list "323" "223" "123" "333") (list (list 1) (list 2) (list 3) (list 4)))
;(changeAllAttrs '("23" "34" "45") (list (list "2" "3") (list "test1" "23") (list "3" "test1")))
;(changeAllAttrs '("23") (list (list "2" "3")))
;(makeResultHead '("23" "34" "45") (list (list "2" "3") (list "test1" "23") (list "3" "test1")))
;(from '("23" "34" "45") (list(list (list "b" "3") (list 1 2) (list 3 4)) (list (list "a" "b") (list 5 6) (list 7 8)) (list (list "a" "d") (list 9 10) (list 11 12))))




;-------------------------------------------
; WHERE and its helper function starts here

(define (replaceAll expression atts)
  (if (list? expression)
      (lambda(tuple) ((first expression) ((replaceAll (second expression) atts) tuple) ((replaceAll (third expression) atts) tuple)))
      (replace-attr expression atts)
      )
  )

(define (where table expression) (satisfy? (replaceAll expression (attributes table)) table))
;(replaceAll (list equal? "a" "c") (list "a" "b" "c")) 
;(replaceAll (list equal? (list + "a" "b") "c") (list "a" "b" "c"))
;(where (list (list "a" "b" "c") (list 1 2 5) (list 1 2 4) (list 1 2 3) (list 2 2 4)) (list equal? (list + "a" "b") "c"))
; Starter for Part 3; feel free to ignore!


; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))
