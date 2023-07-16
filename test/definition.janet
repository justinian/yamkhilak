(use ../yamkhilak/util)
(use ../yamkhilak/definition)

(comment
(def test-file "definitions/test.def")
(assert (not (= config-file nil)))

(def result (parse-definition config-file))

(assert (not (= result nil)))
(pp result)

(def parsed (process result))
(pp parsed)
)

(def source `
W := a o u
V := a i o u e
C := n t k s ɾ m d h g z b y w p
K := ɾy ky tʃ jy gy ny my hy py by
N := n
F := CV CV CVN V VN
S := CV CV CVN V VN KW

!reject: di tun yi ye wi we wo
!filter: ti > tʃi; si > ʃi; tu > tsu; hu > fu; zi > ʤi; za > tsa
!filter: np > mp; nb > mb
!filter: ts > x; ɾ > r; tʃ > ch; ʃ > sh; ʤ > j

name: FS FSS
noun: FS FSS FSSS

    `)
(def result (parse-definition source))

(assert (get result :classes))
(assert (= (get (get result :classes) "W") (make-weighted ["a" "o" "u"])))
(assert (= (get (get result :filters) "tu") "tsu"))

(loop [i :in (range 20)]
    (pp (:generate result "name")))
