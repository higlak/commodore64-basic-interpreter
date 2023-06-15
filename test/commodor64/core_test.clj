(ns commodor64.core-test
  (:require [clojure.test :refer :all]
            [commodor64.core :refer :all]))

(deftest test-palabra-reservada
  (testing "Palabra reservada, true."
    (is (palabra-reservada? 'REM))))

(deftest test-palabra-no-reservada
  (testing "Palabra reservada, false."
    (is (not (palabra-reservada? 'HOLA)))))

;======================================================

(deftest test-operador-simbolo
  (testing "Operador?, simbolo."
    (is (operador? (symbol "<>")))))


(deftest test-operador-no-simbolo
  (testing "Operador?, no simbolo."
    (is (not (operador? '|)))))

;======================================================

(deftest test-simbolo-valido-palabra-reservada(
  testing "Simbolo valido, palabra reservada."
    (is 
      (simbolo-valido? 'RUN)
    )
  )  
)

(deftest test-simbolo-invalido-no-alfanumerico(
  testing "Simbolo no alfanumerico."
    (is 
      (not(simbolo-valido? '$*))
    )
  )  
)

(deftest test-simbolo-invalido-empieza-con-palabra-privilegiada(
  testing "Simbolo empieza con RU"
    (is 
      (not(simbolo-valido? 'RUCULA))
    )
  )  
)

(deftest test-simbolo-invalido-contiene-palabra-privilegiada(
  testing "Simbolo contiene RUN"
    (is 
      (not(simbolo-valido? 'ESRUN))
    )
  )  
)

(deftest test-simbolo-valido-palabra-reservada(
  testing "Simbolo valido, cumple con reglas de nombre."
    (is 
      (simbolo-valido? 'VAR)
    )
  )  
)

(deftest test-anular-invalidos(
  testing "Anular simbolos invalidos"
    (is 
      (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)) (concat '(IF X) '(nil) '(* Y < 12 THEN LET) '(nil) '(X = 0)))
    )
  )  
)