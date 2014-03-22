(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (*  acc n) n (dec k))))]
    (helper 1 base exp)))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn last-element [a-seq]
  (let [helper (fn [seq]
                 (cond
                  (empty? seq) nil
                  (singleton? (rest seq)) (first (rest seq))
                  :else (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
     (empty? seq) nil
     (pred (first seq)) index
     :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [count 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum count)
      (recur (inc count) (+ sum (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{}
         seq a-seq]
    (if (empty? seq)
      result
      (recur (toggle result (first seq)) (rest seq)))))


(defn fast-fibo [n]
  (loop [n n
         fn-minus-1 1
         fn-minus-2 1]
    (cond
     (zero? n) 0
     (<= n 2) fn-minus-1
     :else (recur (dec n) (+ fn-minus-1 fn-minus-2) fn-minus-1 ))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         seen #{}
         prologue []]
    (if (or (empty? seq)
            (contains? seen (first seq)))
      prologue
      (recur (rest seq) (conj seen (first seq)) (conj prologue (first seq))))))

