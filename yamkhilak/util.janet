(var *module-rng* (math/rng))

(defn choice [xs]
    (in xs (math/rng-int *module-rng* (length xs))))

(defn- log+1 [n] (math/log (+ n 1)))

(defn make-weighted [xs]
    (let
        [total-log (log+1 (length xs))]
        (tuple
            ;(seq [i :range [0 (length xs)]]
                (tuple (xs i) (- total-log (log+1 i)))))))

(defn choice-weighted [xs]
    (def total (sum (map last xs)))
    (var roll (* total (math/rng-uniform *module-rng*)))
    ((find
        (fn [x]
            (if (< roll (x 1))
                true
                (do (-= roll (x 1)) false)))
        xs '(nil)) 0))

(defn slurp-file [filename]
    `Return the contents of a file`
    (let [infile (file/open filename)]
        (defer (file/close infile)
            (if (not (= infile nil)) (file/read infile :all)))))
