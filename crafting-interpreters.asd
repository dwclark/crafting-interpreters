(asdf:defsystem #:crafting-interpreters
  :description "Crafting Interpreters Lox Implementatation"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("parse-float" "alexandria")
  :serial t
  :components ((:file "src/tokens")
	       (:file "src/scanner")
	       (:file "src/parser")
	       (:file "src/ast")
	       (:file "src/pretty-print")
	       (:file "src/lox")))

(asdf:defsystem #:crafting-interpreters-tests
  :description "Crafting Interpreters Tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t

  :depends-on ("crafting-interpreters" "fiveam")

  :components ((:file "test/tests")))

