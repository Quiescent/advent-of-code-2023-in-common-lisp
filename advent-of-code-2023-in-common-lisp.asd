(defsystem "advent-of-code-2023-in-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :trivia :trivia.ppcre :arrow-macros :metabang-bind :fset)
  :components ((:module "src"
                        :components ((:file "day-3")
(:file "day-2")
(:file "day-1")
))))
