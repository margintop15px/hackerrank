(ns hackerrank.tree-view)


;;       1
;;   2      3
;; 4   5   6   7
;;      10      8  9



(deftype Node [value left right])


(def tree
  (Node. 1
         (Node. 2 (Node. 4 nil nil)
                (Node. 5 nil
                       (Node. 10 nil nil)))
         (Node. 3 (Node. 6 nil nil)
                (Node. 7 (Node. 8 (Node. 11 nil nil) nil)
                       (Node. 9 nil nil)))))



(defn get-level-values [tree level]
  (loop [nodes         [tree]
         current-level 1]
    (println nodes)
    (let [values (->> nodes
                      (mapv (fn [node]
                              (when node
                                (.value node))))
                      (filter some?))]
      (if (= level current-level)
        values

        (recur
         (flatten (mapv #(vector (.left %) (.right %))
                        (filter some? nodes)))
         (inc current-level))))))


(defn tree-view [tree side depth]
  (let [total-levels (range 1 depth)
        all-values   (mapv (partial get-level-values tree) total-levels)]
    (if (= side :left)
      (mapv first all-values)
      (mapv last all-values))))


(comment
 (get-level-values tree 5)
 (tree-view tree :left 6)
 (tree-view tree :right 6))
