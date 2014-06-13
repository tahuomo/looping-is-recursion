(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [sum base n]
                 (if (zero? n)
                   sum
                   (recur (* sum base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last a-seq]
                 (if (empty? a-seq)
                   last
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [p pred
         s a-seq
         n 0]
    (cond
     (empty? s) nil
     (pred (first s)) n
     :else (recur p (rest s) (inc n)))))


(defn avg [a-seq]
  (loop [sum 0
         n 0
         a a-seq]
    (cond
     (empty? a) (if (zero? n) n (/ sum n))
     :else (recur (+ sum (first a)) (inc n) (rest a)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
     (disj a-set elem)
     (conj a-set elem)))

(defn parity [a-seq]
  (loop [par #{}
         a a-seq]
    (if (empty? a)
         par
         (recur (toggle par (first a)) (rest a)))))

(defn fast-fibo [n]
  (if (zero? n)
      0
      (loop [now 1
             prev 0
             times (dec n)]
        (if (zero? times)
            now
            (recur (+ now prev) now (dec times))))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         vect []
         a a-seq]
    (if
     (or (empty? a) (contains? seen (first a)))
       vect
       (recur (conj seen (first a)) (conj vect (first a)) (rest a)))))



