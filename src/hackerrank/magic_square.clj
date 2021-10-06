(ns hackerrank.magic-square)


(def input
  [[5, 3, 4],
   [1, 5, 8],
   [6, 4, 2]])

(def input2
  [[4 9 2]
   [3 5 7]
   [8 1 5]])

(def input3
  [[4 8 2]
   [4 5 7]
   [6 1 6]])



(defn rows-options [input]
  (loop [rows   input
         result {1 [] 2 [] 3 []}]
    (let [row        (first rows)
          [n1 n2 n3] row
          row-number (- 4 (count rows))]
      (if (seq row)
        (recur (rest rows)
               (->> (reduce
                     (fn [acc x]
                       (cond-> acc
                               (= 15 (+ x n2 n3)) (update row-number conj [x n2 n3])
                               (= 15 (+ n1 x n3)) (update row-number conj [n1 x n3])
                               (= 15 (+ n1 n2 x)) (update row-number conj [n1 n2 x])))
                     result
                     (range 1 10))))
        result))))



(defn cost [initial result]
  (apply + (map
            (fn [i r]
              (Math/abs (- i r)))
            (flatten initial)
            (flatten result))))



(defn solution [input]
  (let [options (rows-options input)]
    (let [row1   (get options 1)
          row2   (get options 2)
          row3   (get options 3)
          result (for [r1 row1
                       r2 row2
                       r3 row3
                       :let [column1-sum (+ (first r1) (first r2) (first r3))
                             column2-sum (+ (second r1) (second r2) (second r3))
                             column3-sum (+ (last r1) (last r2) (last r3))
                             diagonal    (+ (first r1) (second r2) (last r3))]
                       :when (and (= 15 column1-sum column2-sum column3-sum diagonal)
                                  (= 9 (count (distinct (flatten [r1 r2 r3])))))]
                   (do
                     (println [r1 r2 r3])
                     (cost input [r1 r2 r3])))]
      (first result))))


(solution input)
(solution input2)
(solution input3)

