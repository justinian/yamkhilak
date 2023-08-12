(declare-project
  :name "yamkhilak"
  :description ```A conlang word generator```
  :version "0.0.1"
  :dependencies ["https://github.com/janet-lang/spork.git"])

(declare-executable
  :name "ygen"
  :entry "yamkhilak/gen.janet")
