(use spork)
(var *module-rng* (math/rng (os/time)))

(defn power-law [falloff (x & xs)]
    (if (or (= (length xs) 0)
            (> falloff (math/rng-uniform *module-rng*)))
        x
        (power-law falloff xs)))

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
    (if (sh/exists? filename)
        (let [infile (file/open filename)]
            (defer (file/close infile)
                (if (not (= infile nil)) (file/read infile :all))))
        (error (string/format "No such file %s" filename))))
