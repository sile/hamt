(in-package :asdf)

(defsystem hamt
  :name    "hamt"
  :version "0.2.0"
  :author  "Takeru Ohta"
  :description "An implementation of Hash Array Mapped Trie"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "entries-allocator")
               (:file "arc-stream")
               (:file "amt")
               (:file "hamt")))