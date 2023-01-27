(ns lab1_1
  (:require [clojure.string :as s]))

(defn add-char-to-string [chars string]
  (cond
    (= (count chars) 0) '()
    (> (count chars) 0) (if (s/ends-with? string (first chars))
                            (add-char-to-string (rest chars) string)
                            (cons
                             (str string (first chars))
                             (add-char-to-string (rest chars) string)))))

(defn add-char  [chars strings]
  (cond
    (= (count strings) 0) '()
    (> (count strings) 0) (concat
                         (add-char-to-string chars (first strings))
                         (add-char chars (rest strings)))))

(defn make-combinations [chars n]
  (cond
    (<= n 0) '()
    ;; (= n 1) chars
    (= (count chars) 0) '()
    (and (= (count chars) 1) (> n 1)) '()
    :else (add-char chars (make-combinations chars (dec n)))))

(defn -main []
  (println (make-combinations '() 3))
  (println (make-combinations '("a") 1))
  (println (make-combinations '("a") 3))
  (println (make-combinations '("a" "b" "c") -3))
  (println (make-combinations '("a" "b" "c") 2))
  (println (make-combinations '("a" "b" "c") 3)))

(-main)
(make-combinations '("a" "b" "c") 3)




(list (str "a" "abs"))

(Integer/parseInt "123")

(dec 0.5)

(reduce concat "" (list "ghbdtn" "hello"))
(s/join #" " '("ghbdtn" "hello" "holla"))

(map #(Math/abs %) '(-1 -2 3))

(Math/abs (- 5 7))

(reduce max 0 (map #(Math/abs %) (map - '(1 2 3) '(4 7 10))))

(first '(1 2 3))

(def res (filter (fn [x] (= "Bob" (first x))) (list (list "Tom" 31)
                                                    (list "Alice" 22)
                                                    (list "Bob" 42))))
(println res)

(rest '())

(map inc (seq "abc"))

(map int (seq "abc"))

(concat '("ab" "bc") '("ac", "ad"))