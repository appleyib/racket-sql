#| Assignment 1 - Racket Query Language Tests (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student id for each of your group members below.***
<Name>, <CDF>, <ID>
<Name>, <CDF>, <ID>
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.
; DON'T export them from database.rkt!
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

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))


#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
   FROM '(("C" "A" "B" "D")
          (1 "Hi" 5 #t)
          (2 "Bye" 5 #f)
          (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; [self-test]Select one column from an empty table
(test
 (SELECT '("Name") FROM '(("A" "Name" "B" "C")))
 '(("Name")))

; [self-test]Select one column from an one tuple table
(test
 (SELECT '("Name") FROM '(("A" "Name" "B" "C") (1 "dav" 2 3)))
 '(("Name") ("dav")))

; [self-test]Select columns and reordering from an empty table
(test
 (SELECT '("Name" "A" "C") FROM '(("A" "Name" "B" "C")))
 '(("Name" "A" "C")))

; [self-test]Select columns and reordering from an one tuple table
(test
 (SELECT '("Name" "A" "C") FROM '(("A" "Name" "B" "C") (1 "dav" 2 3)))
 '(("Name" "A" "C") ("dav" 1 3)))
 
; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; [self-test]Select some from the product of two empty tables
(test (SELECT * FROM ['(("Name" "b")) "P"] ['(("Age" "c")) "T"])
      '(("Name" "b" "Age" "c")
        ))

; [self-test]Select some from the product of three empty tables
(test (SELECT * FROM ['(("Name" "b")) "P"] ['(("Age" "c")) "T"] ['(("Age" "d")) "R"])
      '(("Name" "b" "T.Age" "c" "R.Age" "d")
        ))

; [self-test]Select some from the product of an empty table and a non-empty table with one same arrtribute
(test (SELECT '("P.Name" "T.Name" "b") FROM [Person "P"] ['(("Name" "b")) "T"])
      '(("P.Name" "T.Name" "b")
        ))

; [self-test]Select some from the product of two one-tuple tables
(test (SELECT * FROM ['(("Name" "b") ("David" 20) "P")] ['(("Age" "c") (37 14) "T")])
      '(("Name" "b" "Age" "c") ("David" 20 37 14))
      )

; [self-test]Select some from the product of three tables.
(test (SELECT '("P.Name" "T.Name" "b") FROM [Person "P"] [Teaching "T"] ['("A" "B") (1 2) (3 4)] "R")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course" "A" "B")
        ("David" 20 #t "David" "CSC324" 1 2)
        ("David" 20 #t "Paul" "CSC108" 1 2)
        ("David" 20 #t "David" "CSC343" 1 2)
        ("Jen" 30 #t "David" "CSC324" 1 2)
        ("Jen" 30 #t "Paul" "CSC108" 1 2)
        ("Jen" 30 #T "David" "CSC343" 1 2)
        ("Paul" 100 #f "David" "CSC324" 1 2)
        ("Paul" 100 #f "Paul" "CSC108" 1 2)
        ("Paul" 100 #f "David" "CSC343" 1 2)
        ("David" 20 #t "David" "CSC324" 3 4)
        ("David" 20 #t "Paul" "CSC108" 3 4)
        ("David" 20 #t "David" "CSC343" 3 4)
        ("Jen" 30 #t "David" "CSC324" 3 4)
        ("Jen" 30 #t "Paul" "CSC108" 3 4)
        ("Jen" 30 #T "David" "CSC343" 3 4)
        ("Paul" 100 #f "David" "CSC324" 3 4)
        ("Paul" 100 #f "Paul" "CSC108" 3 4)
        ("Paul" 100 #f "David" "CSC343" 3 4)))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; [self-test]Take the product of three same tables
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E2"] [Teaching "E3"])
      '(("E1.Course" "E2.Course" "E3.Course")
        ("CSC324" "CSC324" "CSC324")
        ("CSC108" "CSC324" "CSC324")
        ("CSC343" "CSC324" "CSC324")
        ("CSC324" "CSC108" "CSC324")
        ("CSC108" "CSC108" "CSC324")
        ("CSC343" "CSC108" "CSC324")
        ("CSC324" "CSC343" "CSC324")
        ("CSC108" "CSC343" "CSC324")
        ("CSC343" "CSC343" "CSC324")
        ("CSC324" "CSC324" "CSC108")
        ("CSC108" "CSC324" "CSC108")
        ("CSC343" "CSC324" "CSC108")
        ("CSC324" "CSC108" "CSC108")
        ("CSC108" "CSC108" "CSC108")
        ("CSC343" "CSC108" "CSC108")
        ("CSC324" "CSC343" "CSC108")
        ("CSC108" "CSC343" "CSC108")
        ("CSC343" "CSC343" "CSC108")
        ("CSC324" "CSC324" "CSC343")
        ("CSC108" "CSC324" "CSC343")
        ("CSC343" "CSC324" "CSC343")
        ("CSC324" "CSC108" "CSC343")
        ("CSC108" "CSC108" "CSC343")
        ("CSC343" "CSC108" "CSC343")
        ("CSC324" "CSC343" "CSC343")
        ("CSC108" "CSC343" "CSC343")
        ("CSC343" "CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
   FROM ['(("Age" "A" "Name" "D")
           (1 "Hi" 5 #t)
           (2 "Bye" 5 #f)
           (3 "Hi" 10 #t))
         "T1"]
        [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))


; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
        FROM Person
        WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
        FROM Person
        WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
        FROM Person
        WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
        FROM Teaching
        WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
        FROM Person
        WHERE #t)
      Person)

; [self-test]Test on an empty table
(test (SELECT *
        FROM '(("A" "B" "C"))
        WHERE (equal? "B" "C"))
      '(("A" "B" "C"))
      )

; Constant false compound condition
(test (SELECT *
        FROM Person
        WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; [self-test]Simple condition on joined empty table
(test (SELECT *
        FROM ['(("A" "B" "C")) "T"] ['(("D")) "P"] ['(("E")) "R"]
        WHERE (equal? "B" "C"))
      '(("A" "B" "C" "D" "E"))
      )

; [self-test]Simple condition on joined one tuple table
(test (SELECT *
        FROM ['(("A" "B" "C") (1 2 3) "T")] ['(("D") (4) "P")] ['(("E") (5)) "R"]
        WHERE (equal? "B" "C"))
      '(("A" "B" "C" "D" "E") (1 2 3 4 5))
      )

; [self-test]Compound condition on joined empty table with a same attribute
(test (SELECT *
        FROM ['(("A" "B" "C")) "T"] ['(("D")) "P"] ['(("B")) "R"]
        WHERE (And (equal? "T.B" "C") (equal? "D" "R.B")))
      '(("A" "T.B" "C" "D" "R.B")
      ))

; [self-test]Compound constant true condition on joined one tuple table
(test (SELECT *
        FROM ['(("A" "B" "C") (1 2 3)) "T"] ['(("D") (4)) "P"] ['(("E") (5)) "R"]
        WHERE (equal? (+ 1 2) (+ 0 3)))
      '(("A" "B" "C" "D" "E") (1 2 3 4 5))
      )

; Simple condition on joined tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        FROM [Person "P"] [Teaching "T"] [Person "P1"]
        WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))


; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
        FROM Person
        ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
        FROM Person
        ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; [self-test]Order by attribute, selected
(test (SELECT '("Name")
        FROM Person
        ORDER BY "Name")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by a function of an attribute
(test (SELECT *
        FROM Person
        ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

;[self-test]Order on an empty table
(test (SELECT '("Name")
        FROM '(("Name"))
        ORDER BY "Name")
      '(("Name")
        ))

;[self-test]Order on a one tuple table
(test (SELECT '("Name")
        FROM '(("Name") ("A"))
        ORDER BY "Name")
      '(("Name") ("A"))
        )

; Order with duplicate
(test (SELECT *
        FROM Teaching
        ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))



; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

; [self-test]Two joined tables and coubound conditions, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] 
   WHERE (And (equal? "P.Name" "T.Name") (equal? "Course" "CSC324"))
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("David" "CSC324" #t)
   ))

; [self-test]Two joined tables and coubound always false conditions, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] 
   WHERE (And (equal? "P.Name" "T.Name") (equal? "Course" "1234"))
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ))

 
; ---- Nested queries ----
(test
 (SELECT * 
   FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
   FROM [(SELECT '("Name") FROM Person) "Person"]
        [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                           (equal? "Course" "CSC108")))
         "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
   FROM [(SELECT '("A") 
           FROM '(("A" "B") 
                  (1)
                  (10)))
         "Table1"]
        [(SELECT *
           FROM '(("C" "A")
                  ("Hi" "Bye")
                  ("Dog" "Cat")
                  ("Red" "Blue")))
         "Table2"]
   WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))