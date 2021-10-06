(ns hackerrank.magic-square)


(defn gen-options [[n1 n2 n3]]
  (->> (for [x (range 1 10)
             y (range 1 10)
             z (range 1 10)]
         (cond-> []
                 (= 15 (+ n1 n2 n3)) (conj [n1 n2 n3])

                 (= 15 (+ x n2 n3)) (conj [x n2 n3])
                 (= 15 (+ n1 x n3)) (conj [n1 x n3])
                 (= 15 (+ n1 n2 x)) (conj [n1 n2 x])

                 (= 15 (+ x x n3)) (conj [x x n3])
                 (= 15 (+ x n2 x)) (conj [x n2 x])
                 (= 15 (+ n1 x x)) (conj [n1 x x])

                 (= 15 (+ y y n3)) (conj [y y n3])
                 (= 15 (+ y n2 y)) (conj [y n2 y])
                 (= 15 (+ n1 y y)) (conj [n1 y y])

                 (= 15 (+ z z n3)) (conj [z z n3])
                 (= 15 (+ z n2 z)) (conj [z n2 z])
                 (= 15 (+ n1 z z)) (conj [n1 z z])

                 (= 15 (+ x y n3)) (conj [x y n3])
                 (= 15 (+ x n2 y)) (conj [x n2 y])
                 (= 15 (+ n1 x y)) (conj [n1 x y])

                 (= 15 (+ x z n3)) (conj [x z n3])
                 (= 15 (+ x n2 z)) (conj [x n2 z])
                 (= 15 (+ n1 x z)) (conj [n1 x z])

                 (= 15 (+ y z n3)) (conj [y z n3])
                 (= 15 (+ y n2 z)) (conj [y n2 z])
                 (= 15 (+ n1 y z)) (conj [n1 y z])

                 (= 15 (+ x x x)) (conj [x x x])
                 (= 15 (+ x y z)) (conj [x y z])))
       (apply concat)
       (distinct)))



(comment
 (count (gen-options [4 5 8])))



(defn rows-options [input]
  (loop [rows   input
         result {1 [] 2 [] 3 []}]
    (let [row        (first rows)
          row-number (- 4 (count rows))]
      (if (seq row)
        (recur (rest rows)
               (assoc result row-number (distinct (gen-options row))))
        result))))



(defn cost [initial result]
  (apply + (map
            (fn [i r]
              (Math/abs (- i r)))
            (flatten initial)
            (flatten result))))



(defn cubes [input]
  (let [options (rows-options input)]
    (let [row1 (get options 1)
          row2 (get options 2)
          row3 (get options 3)]
      (for [r1 row1
            r2 row2
            r3 row3
            :let [column1-sum (+ (first r1) (first r2) (first r3))
                  column2-sum (+ (second r1) (second r2) (second r3))
                  column3-sum (+ (last r1) (last r2) (last r3))
                  diagonal1   (+ (first r1) (second r2) (last r3))
                  diagonal2   (+ (last r1) (second r2) (first r3))]
            :when (and (= 15 column1-sum column2-sum column3-sum diagonal1 diagonal2)
                       (= 9 (count (distinct (flatten [r1 r2 r3])))))]
        [r1 r2 r3]))))



(defn solution [input]
  (let [cubes-set (cubes input)]
    (->> cubes-set
         (map #(cost input %))
         (apply min))))



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

(def input4
  [[4 5 8]
   [2 4 1]
   [1 9 7]])

(def input5
  [[2 9 8]
   [4 2 7]
   [5 6 7]])


(defn debug [input]
  (->> (cubes input)
       (mapv (fn [cube]
               [(cost input cube) "-" cube]))
       (sort-by first)))


(solution input)    ;; 7
(solution input2)   ;; 1
(solution input3)   ;; 4
(solution input4)   ;; 14
(solution input5)   ;; 21


(debug input5)
