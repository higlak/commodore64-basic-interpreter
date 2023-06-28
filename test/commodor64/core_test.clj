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
      (not(simbolo-valido? '@*))
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

(deftest test-simbolo-valido-palabra-cumple-reglas-nombre(
  testing "Simbolo valido, cumple con reglas de nombre."
    (is 
      (simbolo-valido? 'VAR)
    )
  )  
)

(deftest test-simbolo-valido-con-tipo(
  testing "Simbolo valido, cumple con reglas de nombre, e indica tipo."
    (is 
      (simbolo-valido? 'VAR%)
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

;======================================================

(deftest test-buscar-lineas-restantes-faltan-todas(
  testing "Se obtienen bien las lineas restantes cuando faltan todas"
    (is (
        = (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
          (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
      )
    )
  )  
)


(deftest test-buscar-lineas-restantes-algunas-sentencias-realizadas(
  testing "Se obtienen bien las lineas restantes cuando se realizaron algunas sentencias "
    (is (
        = (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
          (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-expande-nexts-de-la-linea-actual(
  testing "Se obtienen bien las lineas restantes cuando la linea actual tiene nexts"
    (is (
        = (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
           '((20 (NEXT J)))
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-cantidad-negativa-de-sentencias-restantes(
  testing "Se obtienen bien las lineas restantes cuando hay una cantidad negativa de sentencias restantes"
    (is (
        = (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
           '((20))
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-si-la-linea-no-existe-devuelve-nil(
  testing "Se devuelve nil cuando la linea no existe"
    (is (
        nil? (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [17 2] [] [] [] 0 {}])
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-si-la-linea-se-va-de-rango-devuelve-nil(
  testing "Se devuelve nil cuando la linea se va de rango"
    (is (
        nil? (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 2] [] [] [] 0 {}])
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-si-no-hay-lineas-restantes-devuelve-nil(
  testing "Se devuelve nil cuando no hay lineas restantes"
    (is (
        nil? (buscar-lineas-restantes [() [20 1] [] [] [] 0 {}])
      )
    )
  )  
)

(deftest test-buscar-lineas-restantes-devuelve-nil-si-es-ejecucion-inmediata(
  testing "Se devuelve nil cuando es ejecucion inmediata"
    (is (
        nil? (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])
      )
    )
  )  
)

;======================================================

(deftest test-continuar-linea-stack-vacio(
  testing "Falla y devuelve nill con un entorno si el stack esta vacio"
    (is (
        = (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
          [nil, [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]
      )
    )
  )  
)

(deftest test-continuar-linea(
  testing "Devuelve una dupla, con :omitir-restante y el entorno, con "
    (is (
        = (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
          [:omitir-restante, [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]
      )
    )
  )  
)

;======================================================

(deftest test-extraer-no-hay-data-para-extraer(
  testing "No se extrae data cuando no hay data para extraer"
    (is (
        = (extraer-data '(())) '()
      )
    )
  )  
)

(deftest test-extraer-sentencia-data-simple(
  testing "Se extrae la data de una sentencia simple"
    (is (
        = (extraer-data (list '(10 (DATA 5))) ) '(5)
      )
    )
  )  
)

(deftest test-extraer-data-multiples-datos-en-sentencia(
  testing "Se extrae la data de una sentencia con multiples datos"
    (is (
        = (extraer-data (list '(10 (DATA 5 , 10))) ) '(5 10)
      )
    )
  )  
)

(deftest test-extraer-data-multiples-lineas(
  testing "Se extrae la data de multiples lineas"
    (is (
        = (extraer-data (list '(5 (DATA 1)) '(10 (DATA 5 , 10))) ) '(1 5 10)
      )
    )
  )  
)

(deftest test-extraer-sentencia-data-tipo-string(
  testing "Se extrae la data y es de tipo string"
    (is (
        string? (first(extraer-data (list '(10 (DATA HOLA)))))
      )
    )
  )  
)

(deftest test-extraer-data-se-ignora-rem(
  testing "Se extrae la data ignorando los comentarios"
    (is (
        = (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
          '("HOLA" "MUNDO" 10 20)
      )
    )
  )  
)

;======================================================

(deftest test-preprocesar-expresion-reemplazo-valores(
  testing "Se reemplazan las variables por sus valores"
    (is (
        = (preprocesar-expresion '(X$ + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" Z$ "MUNDO"}])
          '("HOLA" + "MUNDO")
      )
    )
  )  
)

(deftest test-preprocesar-expresion-se-reemplazan-los-puntos(
  testing "Se reemplazan los . por 0"
    (is (
        = (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" Z$ "MUNDO"}])
          '("HOLA" + "MUNDO" + 0)
      )
    )
  )  
)

(deftest test-preprocesar-expresion-variable-string-no-definida-se-reemplaza(
  testing "Se reemplazan las variablkes de tipo string no definida ppr """
    (is (
        = (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" }])
          '("HOLA" + "" + 0)
      )
    )
  )  
)

(deftest test-preprocesar-expresion-variable-no-string-no-definida-se-reemplaza(
  testing "Se reemplazan las variables no definidas int y float por 0"
    (is (
        = (preprocesar-expresion '(X$ + Z$ + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA" }])
          '("HOLA" + "" + 0)
      )
    )
  )  
)

(deftest test-preprocesar-expresion-variable-no-string-se-reemplaza(
  testing "Se reemplazan las variables no string"
    (is (
        = (preprocesar-expresion '(Z + .) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Z 5}])
          '(5 + 0)
      )
    )
  )  
)

(deftest test-preprocesar-expresion-variable-no-se-reemplazan-los-literales(
  testing "No se reemplazan los literales"
    (is (
        = (preprocesar-expresion '(Z + 2 + "HOLA") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Z 5}])
          '(5 + 2 + "HOLA")
      )
    )
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

(deftest test-yunting-yard-precedencia-simple(
  testing "Se obtiene bien la RPN en un ejemplo simple"
    (is (
        = (shunting-yard '(1 + 2))
        '(1 2 +)
      )
    )
  )  
)

(deftest test-yunting-yard-precedencia-multiples-niveles-de-precedencia(
  testing "Se obtiene bien la RPN en un ejemplo con multiples niveles de precedencia"
    (is (
        = (shunting-yard '(1 + 2 / 2 EXP 3))
        '(1 2 2 3 EXP / +)
      )
    )
  )  
)

(deftest test-yunting-yard-precedencia-con-parentesis(
  testing "Se obtiene bien la RPN en un ejemplo con multiples niveles de precedencia"
    (is (
        = (shunting-yard (list '1 '+ (symbol "(") '2 '/ '2 (symbol ")") 'EXP '3))
        '(1 2 2 / 3 EXP +)
      )
    )
  )  
)

;======================================================

(deftest test-elimnar-cero-decimal-elimina-los-ceros-finales(
  testing "Se elimina el cero sin eliminar los demas decimales"
    (is (
        = (eliminar-cero-decimal 1.50)
        1.5
      )
    )
  )  
)

(deftest test-elimnar-cero-decimal-no-quedan-decimales(
  testing "Se elimina el cero y el punto cuando no quedan mas decimales"
    (is (
        = (eliminar-cero-decimal 1.0)
        1
      )
    )
  )  
)

(deftest test-elimnar-cero-decimal-no-se-elimina-el-0(
  testing "No se elimina el numero 0"
    (is (
        = (eliminar-cero-decimal 0)
        0
      )
    )
  )  
)

;======================================================

(deftest test-elimnar-cero-entero-numero-natural(
  testing "Devuelve el mismo numero natural con un espacio"
    (is (
        = (eliminar-cero-entero 1)
        " 1"
      )
    )
  )  
)

(deftest test-elimnar-cero-entero-numero-entero(
  testing "Devuelve el mismo numero entero "
    (is (
        = (eliminar-cero-entero -1)
        "-1"
      )
    )
  )  
)

(deftest test-elimnar-cero-entero-numero-decimal(
  testing "Devuelve el decimal sin el 0 a izquierda"
    (is (
        = (eliminar-cero-entero 0.5)
        " .5"
      )
    )
  )  
)

(deftest test-elimnar-cero-entero-numero-decimal-negativo(
  testing "Devuelve el decimal sin el 0 a izquierda"
    (is (
        = (eliminar-cero-entero -0.5)
        "-.5"
      )
    )
  )  
)

(deftest test-elimnar-cero-entero-0(
  testing "Funciona correctamente para 0"
    (is (
        = (eliminar-cero-entero 0)
        " 0"
      )
    )
  )  
)

(deftest test-elimnar-cero-entero-no-numerico(
  testing "Funciona correctamente para 0"
    (is (
        = (eliminar-cero-entero 'HOLA)
        "HOLA"
      )
    )
  )  
)

;======================================================

(deftest test-ejecutar-asignacion-simple(
  testing "Se asigna una varibale al ambiente"
    (is (
        = (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-pisando(
  testing "Se ya estaba la variable que se quiere asignar pisa el valor anterior"
    (is (
        = (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-solo-guarda-los-primeros-dos-caracteres-delnombre(
  testing "Solo se guardan los primeros dos caracteres"
    (is (
        = (ejecutar-asignacion '(HOLA = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{HO 5}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-ademas-guarda-tipos-explicitos(
  testing "Solo se guardan los primeros dos caracteres y los tipos explicitos"
    (is (
        = (ejecutar-asignacion '(HOLA% = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{HO% 5}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-se-realizan-calculos-numericos(
  testing "Se asigna el resultado de calcular la expresion de la derecha"
    (is (
        = (ejecutar-asignacion '(X = 5 + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 6}]
      )
    )
  )  
)


(deftest test-ejecutar-asignacion-se-realizan-calculos-numericos-con-redefinicion-de-variables(
  testing "Se asigna el resultado de calcular la expresion de la derecha, incluso si reutiliza variables"
    (is (
        = (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 6}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-se-realizan-calculos-de-strings(
  testing "Se asigna el resultado de calcular la expresion de la derecha"
    (is (
        = (ejecutar-asignacion '(X$ = "HOLA" + " CHAU") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA CHAU"}]
      )
    )
  )  
)

(deftest test-ejecutar-asignacion-se-realizan-calculos-de-strings-con-redefinicion-de-variables(
  testing "Se asigna el resultado de calcular la expresion de la derecha, incluso si reutiliza variables"
    (is (
        = (ejecutar-asignacion '(X = X + " CHAU") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X "HOLA"}])
        ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X "HOLA CHAU"}]
      )
    )
  )  
)

;======================================================

(deftest test-evaluar 
  (testing "let asigna las variables"
    (is (
        = (evaluar '(LET X = 2) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])
        [:sin-errores ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]]
      )
    )
  )
  (testing "end corta la ejecucion y mantiene el entorno"
    (is (
        = (evaluar '(end) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
        [nil ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]]
      )
    )
  )
)

(deftest test-aplicar-desigualdad
  (testing "> numerico"
    (is (
        = (aplicar '> 4 2 [15 2]) -1
      )
    )
  )

  (testing "< numerico"
    (is (
        = (aplicar '< 4 2 [15 2]) 0
      )
    )
  )

  (testing "<= numerico igual"
    (is (
        = (aplicar '<= 2 2 [15 2]) -1
      )
    )
  )

  (testing "<= numerico menor"
    (is (
        = (aplicar (symbol "<=") 2 4 [15 2]) -1
      )
    )
  )

   (testing ">= numerico igual"
    (is (
        = (aplicar '>= 2 2 [15 2]) -1
      )
    )
  )

  (testing ">= numerico menor"
    (is (
        = (aplicar '>= 2 4 [15 2]) 0
      )
    )
  )

  (testing "<> numerico igual"
    (is (
        = (aplicar '<> 2 2 [15 2]) 0
      )
    )
  )

  (testing "<> numerico distinto"
    (is (
        = (aplicar '<> 2 4 [15 2]) -1
      )
    )
  )
)