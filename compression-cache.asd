(defsystem "compression-cache"
  :version "0.1.0"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :depends-on ("salza2" "clache")
  :components ((:file "compression-cache"))
  :description "To provide a simple protocol for caching compressed files.")
