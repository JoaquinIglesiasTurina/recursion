(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (= elem (first a-seq)) true
   (empty? a-seq) false
   :else
   (sequence-contains? elem (rest a-seq))))

;(println (sequence-contains? 3 [1 2 3]))       ;=> true
;(println (sequence-contains? 3 [4 7 9])) ;=> false
;(println (sequence-contains? :pony []))  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? b-seq) (empty? a-seq)) true
        (and (= (first b-seq) (first a-seq))
             (and (not (empty? a-seq)) (not (empty? b-seq))))
        (seq= (rest a-seq) (rest b-seq))
          :else false))

(defn my-map [f seq-1 seq-2]
  (cond (empty? seq-1) seq-1
        (empty? seq-2) seq-2
        :else (cons (f (first seq-1) (first seq-2))
                    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [update-key (first a-seq)
          update-val (inc (or (get freqs update-key) 0))
          new-freqs (assoc freqs update-key update-val)]
      (frequencies-helper new-freqs (rest a-seq)))))
(defn my-frequencies [a-seq]
  (frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [f (first a-map)
          new (repeat (last f) (first f))]
      (concat new (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [c (count a-seq)
        f (int (/ c 2))]
    (vector (my-take f a-seq) (my-drop f a-seq))))

(defn seq-merge [a-seq b-seq]
  (defn helper [end-seq a-seq b-seq]
    (cond (empty? a-seq) (concat end-seq b-seq)
          (empty? b-seq) (concat end-seq a-seq)
          :else
          (let [fa (first a-seq)
                fb (first b-seq)]
            (if (> fa fb)
              (helper (conj end-seq fb) a-seq (rest b-seq))
              (helper (conj end-seq fa) (rest a-seq) b-seq)))))
  (helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [h (halve a-seq)
          fh (first h)
          sh (second h)]
      (seq-merge (merge-sort fh) (merge-sort sh)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

