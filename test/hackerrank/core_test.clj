(ns hackerrank.core-test
  (:require [clojure.test :refer :all]
            [hackerrank.plus-minus :refer [solution]]))


(def test1
  [-4, 3, -9, 0, 4, 1])


(def result1
  "0,500000
0,333333
0,166667")


(deftest solution-test
  (testing "testing"
    (is (= (with-out-str (solution test1))
           result1))))
