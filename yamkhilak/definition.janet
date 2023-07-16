# Parsing and handling language definition files

(import ./util)

(def definition @{
    :expand (fn [self C &opt iteration]
        (default iteration 0)
        (if (> iteration 10) (error (string/format "Error: could not expand %s after %d tries" C (- iteration 1))))
        (let [options (get (get self :classes) C)
              output (:class-replace self (util/choice-weighted options))]
            (if (has-value? (get self :reject) output)
                (:expand self C (+ iteration 1))
                output)))

    :class-replace (fn [self str]
        (string (peg/replace-all '(range "AZ") (fn [C] (:expand self C)) str)))

    :filter (fn [self str]
        (var str str)
        (loop [[before after] :pairs (get self :filters)]
            (set str (string/replace-all before after str)))
        str)

    :generate (fn [self pos]
        (let [options (get (get self :words) pos)]
            (if options
                (:filter self (:class-replace self (util/choice-weighted options)))
                (error (string/format "Error: Unknown word type: %s" pos)))))
    })

(defn new-definition []
    (table/setproto @{:classes @{} :filters @{} :reject @[] :words @{}} definition))

(defn- subsection-list [kind & items] 
    ~(,kind ,items))
(defn- subsection-pairs [kind & items]
    ~(,kind ,(table ;items)))
(defn- subsection-named-list [kind name & items]
    ~(,kind ,name ,items))

(def- definition-grammar
    ~{
        :main (* :s* (any :statement) :s* -1)
        #:statement (+ :classdef :filter :other-config)
        :statement (* (+ :classdef :filter :reject :words) :eol)

        :punct (set ";.<>:\\//=+@#$%^&*()[]{}|-_")
        :char (if-not (+ :punct :wschar :eol) 1)
        :wschar (set " \t\r\f\v")
        #:eol (some (+ "\n" -1)) # strangely this does not work
        :eol (+ (some "\n") (* (any "\n") -1))
        :ws (any :wschar)

        :token (<- (some :char))
        :expression (<- (some (+ :char :punct)))

        :classdef (cmt (* (constant :classdef) (<- (range "AZ")) :ws ":=" :ws (some (* :token :ws))) ,subsection-named-list)

        :filter-term (* :token :ws ">" :ws :token)
        :filter-list (* :filter-term :ws (any (* ";" :ws :filter-term :ws)))
        :filter (cmt (* (constant :filter) "!filter:" :ws :filter-list) ,subsection-pairs)

        :reject (cmt (* (constant :reject) "!reject:" :ws (some (* :expression :ws))) ,subsection-list)

        :words (cmt (* (constant :words) (<- (some (range "az"))) ":" :ws (some (* :expression :ws))) ,subsection-named-list)
        })

(defn- add-weights [tab]
    (loop [[name list] :pairs tab]
        (update tab name util/make-weighted)))

(defn parse-definition [text]
    `Parse a language definition from a string`
    (let [result (new-definition)]
        (loop [item :in (peg/match definition-grammar text)]
            (match item
                [:classdef name entries] (put (get result :classes) name entries)
                [:filter entries] (merge-into (get result :filters) entries)
                [:reject entries] (array/concat (get result :reject) entries)
                [:words pos entries] (let
                    [poslist (get result :words)]
                    (if (has-key? poslist pos)
                        (array/concat (get poslist pos) entries)
                        (put (get result :words) pos entries)))))
        (add-weights (get result :classes))
        (add-weights (get result :words))
        result))

(defn get-definition [filename]
    `Get a language definition from <filename>`
    (parse-definition (util/slurp-file filename)))
