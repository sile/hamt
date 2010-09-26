(in-package :asdf)

(defsystem hamt
  :name    "hamt"
  :version "0.0.1"
  :author  "Takeru Ohta"
  :description "A implementation of Hash Array Mapped Trie"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "arc-stream")
               (:file "amt")
               (:file "hamt")))