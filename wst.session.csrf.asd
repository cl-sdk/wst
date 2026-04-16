(asdf:defsystem #:wst.session.csrf
  :description "Generic interface for managing per-session CSRF tokens (add/remove/verify)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :pathname "session"
  :serial t
  :components ((:file "csrf")))
