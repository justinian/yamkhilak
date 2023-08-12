(use spork)
(import ./definition)

(defn hello
  `Evaluates to "Hello!"`
  []
  "Hello!")

(def program-desc "Yamakhilak, a conglang word generator")
(def options-list
  '(:default  {:kind :option :required true :help "Language definition file"}
    "count"   {:kind :option :default "50" :short "c" :help "Number of words to generate"}
    "type"    {:kind :option :default "name" :short "t" :help "Type of word to generate"}))

(defn- print-line [words]
  (loop [word :in words] (prinf "%-15s" (string/trim word)))
  (print))

(defn- print-table [width words]
  (var i 0)
  (while (< i (length words))
    (print-line (take width (slice words i)))
    (+= i width)))

(defn main [& args]
  (try
    (let [opts (argparse/argparse program-desc ;options-list)
          def-file (opts :default)
          definition (definition/get-definition def-file)
          wordcount (scan-number (opts "count"))
          words (seq [i :in (range wordcount)] (:generate definition (opts "type")))]
        (print-table 5 words))
    ([err] (print "Error: " err))))