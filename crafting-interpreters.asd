(asdf:defsystem #:crafting-interpreters
  :description "Crafting Interpreters Lox Implementatation"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("parse-float" "alexandria")
  :serial t
  :components ((:file "src/tokens")
	       (:file "src/scanner")
	       (:file "src/lox")))
