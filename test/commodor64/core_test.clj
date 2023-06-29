(ns commodor64.core-test
  (:require [clojure.test :refer :all]
            [commodor64.core :refer :all]))

;======================================================

(deftest test-palabra-reservada
  (testing "Palabra reservada, true."
    (is (palabra-reservada? 'REM))
  )
  (testing "Palabra reservada, false."
    (is (not (palabra-reservada? 'HOLA)))
  )
)

;======================================================

(deftest test-operador-simbolo
  (testing "Operador?, simbolo."
    (is (operador? (symbol "<>")))
    )
  (testing "Operador?, no simbolo."
    (is (not (operador? '|)))
  )
)

;======================================================

(deftest test-simbolo-valido?
  (testing "Simbolo valido, palabra reservada."
    (is (simbolo-valido? 'RUN))
  )  
  (testing "Simbolo no alfanumerico."
    (is (not(simbolo-valido? '@*)))
  )
  (testing "Simbolo contiene RUN"
    (is (not(simbolo-valido? 'ESRUN)))
  )
  (testing "Simbolo valido, cumple con reglas de nombre."
    (is (simbolo-valido? 'VAR))
  )
  (testing "Simbolo valido, cumple con reglas de nombre, e indica tipo."
    (is (simbolo-valido? 'VAR%))
  )
)

;======================================================

(deftest test-anular-invalidos
  (testing "Anular simbolos invalidos"
    (is 
      (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)) (concat '(IF X) '(nil) '(* Y < 12 THEN LET) '(nil) '(X = 0)))
    )
  )  
)

;======================================================

(deftest test-cargar-linea
  (testing "Se puede cargar una linea al entorno ."
    (is ( =
      (first(cargar-linea '(10 (PRINT X)) '[() [:ejecucion-inmediata 0] [] [] [] 0 {}]))
      '((10 (PRINT X)))
    ))
  )  
  (testing "Se pueden cargar multiples lineas al entorno ."
    (is ( =
      (first(cargar-linea '(10 (PRINT Y)) '[((20 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
      '( (10 (PRINT Y))  (20 (PRINT X)))
    ))
  )
  (testing "Las lineas mayores se cargan despues de las menores ."
    (is ( =
      (first(cargar-linea '(20 (PRINT Y)) '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
      '( (10 (PRINT X))  (20 (PRINT Y)))
    ))
  )
  (testing "Se pueden cargar lineas en el medio."
    (is ( =
      (first(cargar-linea '(15 (Z = 150)) '[((10 (X = 100)) (20 (Y = 200)) ) (5,4)]))
      '( (10 (X = 100)) (15 (Z = 150)) (20 (Y = 200)))
    ))
  )
  (testing "Si se carga la misma linea se pisa."
    (is ( =
      (first(cargar-linea '(10 (Z = 150)) '[((10 (X = 100)) (20 (Y = 200)) ) (5,4)]))
      '( (10 (Z = 150)) (20 (Y = 200)))
    ))
  )  
)

;======================================================

(deftest test-expandir-next-simple
  (testing "Se puede subdividir dos next simples"
    (is ( =
      (expandir-next (list 'NEXT 'A (symbol ",") 'B))
      '((NEXT A) (NEXT B))
    ))
  )  
  (testing "Se puede expandir los next y el resto queda igual"
    (is ( =
      (expandir-nexts(list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))
      '((PRINT 1) (NEXT A) (NEXT B))
    ))
  )
  (testing "Se puede expandir los next y los parentesis quedan bien"
    (is( =
      (expandir-nexts(list '(PRINT 1) '(PRINT 2) (list 'NEXT 'A (symbol ",") 'B)))
      '((PRINT 1) (PRINT 2) (NEXT A) (NEXT B))
    ))
  )
)

;======================================================

(deftest test-tipo-de-variable
  (testing "La variable es de tipo float"
    (is (
      and (variable-float? 'X) (not(variable-integer? 'X)) (not(variable-string? 'X))
    ))
  )
  (testing "La variable es de tipo integer"
    (is (
      and (not(variable-float? 'X%)) (variable-integer? 'X%) (not(variable-string? 'X%))
    ))
  )  
  (testing "La variable es de tipo string"
    (is (
      and (not(variable-float? 'X$)) (not(variable-integer? 'X$)) (variable-string? 'X$)
    ))
  )
)

;======================================================

(deftest test-contar-sentencias
  (testing "Se cuenta bien la cantidad de sentencias simple"
    (is (
      = 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
    ))
  )
  (testing "Se cuenta bien la cantidad de sentencias"
    (is (
      = 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
    ))
  )
  (testing "Se cuenta bien la cantidad de sentencias con expansion de next"
    (is (
      = 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
    ))
  )
)

;======================================================

(deftest test-buscar-lineas-restantes
  (testing "Se obtienen bien las lineas restantes cuando faltan todas"
    (is ( =
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
      (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
    ))
  )
  (testing "Se obtienen bien las lineas restantes cuando se realizaron algunas sentencias "
    (is ( =
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
      (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
    ))
  )
  (testing "Se obtienen bien las lineas restantes cuando la linea actual tiene nexts"
    (is (= 
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
      '((20 (NEXT J)))
    ))
  )
  (testing "Se obtienen bien las lineas restantes cuando hay una cantidad negativa de sentencias restantes"
    (is (= 
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
      '((20))
    ))
  )
  (testing "Se devuelve nil cuando la linea no existe"
    (is (nil? 
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [17 2] [] [] [] 0 {}])
    ))
  )
  (testing "Se devuelve nil cuando la linea se va de rango"
    (is (nil? 
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 2] [] [] [] 0 {}])
    ))
  )
  (testing "Se devuelve nil cuando no hay lineas restantes"
    (is (nil? 
      (buscar-lineas-restantes [() [20 1] [] [] [] 0 {}])
    ))
  )
  (testing "Se devuelve nil cuando es ejecucion inmediata"
    (is (nil?
      (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])
    ))
  )
)

;======================================================

(deftest test-continuar-linea(
  testing "Falla y devuelve nill con un entorno si el stack esta vacio"
    (is (= 
      (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
      [nil, [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]
    ))
  )  
  (testing "Devuelve una dupla, con :omitir-restante y el entorno, con "
    (is ( =
      (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
      [:omitir-restante, [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]
    ))
  )  
)

;======================================================

(deftest test-extraer-data
  (testing "No se extrae data cuando no hay data para extraer"
    (is (= (extraer-data '(())) '()))
  )  
  (testing "Se extrae la data de una sentencia simple"
    (is (= (extraer-data (list '(10 (DATA 5))) ) '(5)))
  )  
  (testing "Se extrae la data de una sentencia con multiples datos"
    (is (= (extraer-data (list '(10 (DATA 5 , 10))) ) '(5 10)))
  )
  (testing "Se extrae la data de multiples lineas"
    (is (= (extraer-data (list '(5 (DATA 1)) '(10 (DATA 5 , 10))) ) '(1 5 10)))
  )  
  (testing "Se extrae la data y es de tipo string"
    (is (string? (first(extraer-data (list '(10 (DATA HOLA)))))))
  )  
  (testing "Se extrae la data ignorando los comentarios"
    (is (= 
      (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
      '("HOLA" "MUNDO" 10 20)
    ))
  )  
)

;======================================================

(deftest test-preprocesar-expresion
  (testing "Se reemplazan las variables por sus valores"
    (is (= 
      (preprocesar-expresion '(X$ + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" Z$ "MUNDO"}])
      '("HOLA" + "MUNDO")
    ))
  )  
  (testing "Se reemplazan los . por 0"
    (is (= 
      (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" Z$ "MUNDO"}])
      '("HOLA" + "MUNDO" + 0)
    ))
  )  
  (testing "Se reemplazan las variablkes de tipo string no definida ppr """
    (is (
      = (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" }])
      '("HOLA" + "" + 0)
    ))
  )
  (testing "Se reemplazan las variables no definidas int y float por 0"
    (is (= 
      (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" }])
      '("HOLA" + "" + 0)
    ))
  )  
  (testing "Se reemplazan las variables no string"
    (is (
      = (preprocesar-expresion '(Z + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Z 5}])
      '(5 + 0)
    ))
  )  
  (testing "No se reemplazan los literales"
    (is (= 
      (preprocesar-expresion '(Z + 2 + "HOLA") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Z 5}])
      '(5 + 2 + "HOLA")
    ))
  )  
)

;======================================================

(deftest test-desambiguar(
  testing "Se desambiguan los mas menos y MID$"
    (is (
        = (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
          (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")"))
      )
    )
  )  
)

;======================================================

(deftest test-yunting-yard-precedencia(
  testing "Se obtiene bien la RPN en un ejemplo simple"
    (is (
        = (shunting-yard '(1 + 2))
        '(1 2 +)
      )
    )
  )  
  (testing "Se obtiene bien la RPN en un ejemplo con multiples niveles de precedencia"
    (is (= 
      (shunting-yard '(1 + 2 / 2 EXP 3))
      '(1 2 2 3 EXP / +)
    ))
  )  
  (testing "Se obtiene bien la RPN en un ejemplo con multiples niveles de precedencia"
    (is (= 
      (shunting-yard (list '1 '+ (symbol "(") '2 '/ '2 (symbol ")") 'EXP '3))
      '(1 2 2 / 3 EXP +)
    ))
  )  
)

;======================================================

(deftest test-elimnar-cero-decimal
  (testing "Se elimina el cero sin eliminar los demas decimales"
    (is (= (eliminar-cero-decimal 1.50) 1.5))
  )  
  (testing "Se elimina el cero y el punto cuando no quedan mas decimales"
    (is ( = (eliminar-cero-decimal 1.0) 1))
  )  
  (testing "No se elimina el numero 0"
    (is (= (eliminar-cero-decimal 0) 0))
  )  
)

;======================================================

(deftest test-elimnar-cero-entero-numero-natural
  (testing "Devuelve el mismo numero natural con un espacio"
    (is (= (eliminar-cero-entero 1) " 1"))
  )  
  (testing "Devuelve el mismo numero entero "
    (is (= (eliminar-cero-entero -1) "-1"))
  )  
  (testing "Devuelve el decimal sin el 0 a izquierda"
    (is (= (eliminar-cero-entero 0.5) " .5"))
  )  
  (testing "Devuelve el decimal sin el 0 a izquierda"
    (is (= (eliminar-cero-entero -0.5) "-.5"))
  )  
  (testing "Funciona correctamente para 0"
    (is (= (eliminar-cero-entero 0) " 0"))
  )  
  (testing "Funciona correctamente para 0"
    (is (= (eliminar-cero-entero 'HOLA) "HOLA"))
  )  
)

;======================================================

(deftest test-ejecutar-asignacion
  (testing "Se asigna una varibale al ambiente"
    (is (= 
      (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]
    ))
  )  
  (testing "Se ya estaba la variable que se quiere asignar pisa el valor anterior"
    (is (
      = (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]
    ))
  )  
  (testing "Solo se guardan los primeros dos caracteres"
    (is (
      = (ejecutar-asignacion '(HOLA = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{HO 5}]
    ))
  )  
  (testing "Solo se guardan los primeros dos caracteres y los tipos explicitos"
    (is (= 
      (ejecutar-asignacion '(HOLA% = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{HO% 5}]
    ))
  )  
  (testing "Se asigna el resultado de calcular la expresion de la derecha"
    (is (= 
      (ejecutar-asignacion '(X = 5 + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 6}]
    ))
  )  
  (testing "Se asigna el resultado de calcular la expresion de la derecha, incluso si reutiliza variables"
    (is (= 
      (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}])
      ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 6}]
    ))
  )  
  (testing "Se asigna el resultado de calcular la expresion de la derecha"
    (is (= 
      (ejecutar-asignacion '(X$ = "HOLA" + " CHAU") ['((10 (PRINT X$))) [10 1] [] [] [] 0 '{}])
      ['((10 (PRINT X$))) [10 1] [] [] [] 0 '{X$ "HOLA CHAU"}]
    ))
  )
  (testing "Se asigna el resultado de calcular la expresion de la derecha, incluso si reutiliza variables"
    (is (= 
      (ejecutar-asignacion '(X$ = X$ + " CHAU") ['((10 (PRINT X$))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
      ['((10 (PRINT X$))) [10 1] [] [] [] 0 '{X$ "HOLA CHAU"}]
    ))
  )  
) 

;======================================================

(deftest test-evaluar 
  (testing "let asigna las variables"
    (is ( =
      (evaluar '(LET X = 2) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
      [:sin-errores ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]]
    ))
  )
  (testing "end corta la ejecucion y mantiene el entorno"
    (is ( =
      (evaluar '(END) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
      [nil ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]]
    ))
  )

  (testing "Restore reinicia el puntero a la data"
    (is ( =
      (evaluar '(RESTORE) ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 3 '{X 2}])
      [:sin-errores ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 0 '{X 2}]]
    ))
  )

  (testing "Read devuelve el siguiente elemento dentro de data"
    (is ( =
      (evaluar '(READ X) ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 2 '{X 2}])
      [:sin-errores ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 3 '{X 3}]]
    ))
  )

  (testing "Clear limpia las variables"
    (is ( =
      (evaluar '(CLEAR) ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 2 '{X 2}])
      [:sin-errores ['((10 (PRINT X))) [10 1] [] [] [5 4 3 2 1] 2 '{}]]
    ))
  )
)

;======================================================
(deftest test-aplicar-desigualdad
  (testing "> numerico"
    (is (= (aplicar '> 4 2 [15 2]) -1))
  )

  (testing "< numerico"
    (is (= (aplicar '< 4 2 [15 2]) 0))
  )

  (testing "<= numerico igual"
    (is (= (aplicar '<= 2 2 [15 2]) -1))
  )

  (testing "<= numerico menor"
    (is (= (aplicar (symbol "<=") 2 4 [15 2]) -1))
  )

  (testing ">= numerico igual"
    (is (= (aplicar '>= 2 2 [15 2]) -1))
  )

  (testing ">= numerico menor"
    (is (= (aplicar '>= 2 4 [15 2]) 0))
  )

  (testing "<> numerico igual"
    (is (= (aplicar '<> 2 2 [15 2]) 0))
  )

  (testing "<> numerico distinto"
    (is (= (aplicar '<> 2 4 [15 2]) -1))
  )
)

(deftest aplicar-STR
  (testing "Si se aplica sobre un numero devuelve su string"
    (is (= (aplicar 'STR$ 50 [10 1] ) " 50"))
  )
   
  (testing "Si se aplica sobre un string deuelve nil"
    (is (= (aplicar 'STR$ "hola" [10 1] ) nil))
  )  
)

(deftest aplicar-ASC
  (testing "Si se aplica sobre un numero devuelve devuelve nil"
    (is ( = (aplicar 'ASC 50 [10 1] ) nil))
  )
   
  (testing "Si se aplica sobre un string deuelve el codigo ascii del primero"
    (is (= 
      (aplicar 'ASC "hola" [10 1] )
      (int (first "hola"))
    ))
  )  
)