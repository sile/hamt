(in-package :asdf)

(defsystem hamt
  :name    "hamt"
  :version "0.1.0"
  :author  "Takeru Ohta"
  :description "A implementation of Hash Array Mapped Trie"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "entries-allocator")
               (:file "arc-stream")
               (:file "amt")
               (:file "hamt")))