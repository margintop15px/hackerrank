(ns hackerrank.plus-minus)


(defn solution [arr]
  (let [increment (/ 1 (count arr))
        ratios    (reduce
                   (fn [acc number]
                     (cond
                       (> number 0) (update acc :pos + increment)
                       (< number 0) (update acc :neg + increment)
                       :otherwise (update acc :nulls + increment)))
                   {:pos 0 :neg 0 :nulls 0}
                   arr)
        {:keys [pos neg nulls]} ratios]
    (->> [pos neg nulls]
         (map double)
         (apply format "%.6f\n%.6f\n%.6f")
         print)))
