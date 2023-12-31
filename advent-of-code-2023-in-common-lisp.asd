(defsystem "advent-of-code-2023-in-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :trivia :trivia.ppcre :arrow-macros :metabang-bind :fset :datastructures)
  :components ((:module "src"
                        :components ((:file "day-25")
(:file "day-24")
(:file "day-23")
(:file "day-22")
(:file "day-21")
(:file "day-20")
(:file "day-19")
(:file "day-18")
(:file "day-17")
(:file "day-16")
                             (:file "day-15")
                             (:file "day-14")
                             (:file "day-13")
                             (:file "day-12")
                             (:file "day-11")
                             (:file "day-10")
                             (:file "day-9")
                             (:file "day-8")
                             (:file "day-7")
                             (:file "day-6")
                             (:file "day-5")
                             (:file "day-4")
                             (:file "day-3")
                             (:file "day-2")
                             (:file "day-1")))
               (:module "main"
                :depends-on ("src")
                :components ((:file "all")))))
