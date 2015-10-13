(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (nil? (first coll))
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq)))
      false
    (not (= (first a-seq) (first b-seq)))
      false
    (and (empty? a-seq) (empty? b-seq))
      true
    :else
      (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (not (or (empty? seq-1) (empty? seq-2)))
  (cons
    (f (first seq-1) (first seq-2))
    (my-map f (rest seq-1) (rest seq-2)))
  '()))

(defn power [n k]
  (cond
    (zero? k)
      1
    (< k 1)
      n
    :else
      (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0)
      ()
    :else
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= up-to 0)
      ()
    :else
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (= (count a-seq) 0)
      (cons a-seq '())
    :else
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (= (count a-seq) 0)
      (cons a-seq '())
    :else
      (cons a-seq (inits (drop-last a-seq)))))

(defn rotations-helper [i a-seq]
  (cond
    (= (count a-seq) i)
      '()
  :else
    (cons a-seq (rotations-helper (inc i) (cons (last a-seq) (drop-last a-seq))))))

(defn rotations [a-seq]
  (cond
    (= (count a-seq) 0)
      '(())
  :else
    (rotations-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (cond (empty? a-seq)
    freqs
  :else
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (merge freqs {(first a-seq) (inc (get freqs (first a-seq)))}) (rest a-seq))
      (my-frequencies-helper (merge freqs {(first a-seq) 1}) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [unfreqs a-map]
  (let [first-amount (second (first a-map))
        first-value (first (first a-map))]
  (cond (empty? a-map)
    unfreqs
  :else
    (un-frequencies-helper (concat unfreqs (repeat first-amount first-value)) (rest a-map)))))

(defn un-frequencies [a-map]
    (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

