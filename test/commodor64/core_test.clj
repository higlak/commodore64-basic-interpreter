(ns commodor64.core-test
  (:require [clojure.test :refer :all]
            [commodor64.core :refer :all]))

(deftest test-palabra-reservada
  (testing "FIXME, I fail."
    (is (palabra-reservada? 'REM))))

(deftest test-palabra-no-reservada
  (testing "FIXME, I fail."
    (is (not (palabra-reservada? 'HOLA)))))