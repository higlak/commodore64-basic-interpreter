(ns commodor64.core-test
  (:require [clojure.test :refer :all]
            [commodor64.core :refer :all]))

;======================================================

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

;======================================================

(deftest test-cargar-linea(
  testing "Se puede cargar una linea al entorno ."
    (is 
      ( =
        (first(cargar-linea '(10 (PRINT X)) '[() [:ejecucion-inmediata 0] [] [] [] 0 {}]))
        '((10 (PRINT X)))
        )
    )
  )  
)

(deftest test-se-pueden-cargar-multiples-lineas(
  testing "Se pueden cargar multiples lineas al entorno ."
    (is 
      ( =
        (first(cargar-linea '(10 (PRINT Y)) '[((20 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
        '( (10 (PRINT Y))  (20 (PRINT X)))
        )
    )
  )  
)

(deftest test-las-mayores-se-cargan-despues-de-las-menores(
  testing "Las lineas mayores se cargan despues de las menores ."
    (is 
      ( =
        (first(cargar-linea '(20 (PRINT Y)) '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
        '( (10 (PRINT X))  (20 (PRINT Y)))
        )
    )
  )  
)

(deftest test-se-puede-cargar-en-el-medio(
  testing "Se pueden cargar lineas en el medio."
    (is 
      ( =
        (first(cargar-linea '(15 (Z = 150)) '[((10 (X = 100)) (20 (Y = 200)) ) (5,4)]))
        '( (10 (X = 100)) (15 (Z = 150)) (20 (Y = 200)))
        )
    )
  )  
)

(deftest test-si-se-carga-la-misma-linea-se-pisa(
  testing "Si se carga la misma linea se pisa."
    (is 
      ( =
        (first(cargar-linea '(10 (Z = 150)) '[((10 (X = 100)) (20 (Y = 200)) ) (5,4)]))
        '( (10 (Z = 150)) (20 (Y = 200)))
        )
    )
  )  
)

;======================================================

(deftest test-expandir-next-simple(
  testing "Se puede subdividir dos next simples"
    (is 
      ( =
        (expandir-next (list 'NEXT 'A (symbol ",") 'B))
        '((NEXT A) (NEXT B))
        )
    )
  )  
)

(deftest test-expandir-next-complejos(
  testing "Se puede expandir los next y el resto queda igual"
    (is 
      ( =
        (expandir-nexts(list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))
        '((PRINT 1) (NEXT A) (NEXT B))
        )
    )
  )  
)

(deftest test-expandir-next-mas-sentencias(
  testing "Se puede expandir los next y los parentesis quedan bien"
    (is 
      ( =
        (expandir-nexts(list '(PRINT 1) '(PRINT 2) (list 'NEXT 'A (symbol ",") 'B)))
        '((PRINT 1) (PRINT 2) (NEXT A) (NEXT B))
        )
    )
  )  
)

;======================================================

(deftest test-tipo-de-variable-float(
  testing "La variable es de tipo float"
    (is (
        and (variable-float? 'X) (not(variable-integer? 'X)) (not(variable-string? 'X))
      )
    )
  )  
)

(deftest test-tipo-de-variable-integer(
  testing "La variable es de tipo integer"
    (is (
        and (not(variable-float? 'X%)) (variable-integer? 'X%) (not(variable-string? 'X%))
      )
    )
  )  
)

(deftest test-tipo-de-variable-string(
  testing "La variable es de tipo string"
    (is (
        and (not(variable-float? 'X$)) (not(variable-integer? 'X$)) (variable-string? 'X$)
      )
    )
  )  
)

;======================================================

(deftest test-contar-sentencias-simple(
  testing "Se cuenta bien la cantidad de sentencias"
    (is (
        = 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
      )
    )
  )  
)

(deftest test-contar-sentencias-multiples-sentencias(
  testing "Se cuenta bien la cantidad de sentencias"
    (is (
        = 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
      )
    )
  )  
)

(deftest test-contar-sentencias-con-expansion-de-next(
  testing "Se cuenta bien la cantidad de sentencias con expansion de next"
    (is (
        = 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
      )
    )
  )  
)

