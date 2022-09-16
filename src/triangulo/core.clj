(ns triangulo.core
  (:require [clojure.math :as math])) 

(defn calc-perimetro
  "Calcula o perimetro do triangulo, dado A B e C"
  [a b c]
  (+ a b c))

(defn calc-radianos
  "Calcular radianos dado lados a b e c de um triangulo"
  [a b c]
  (math/acos (/ (-
                 (+
                  (* b b)
                  (* c c))
                 (* a a))
                (* 2 b c))))
  
(defn calc-angulo
  "Calcula o ângulo ∠A, dado A B C."
  [a b c]
  (math/to-degrees (calc-radianos a b c)))

(defn calc-area
  "Calcula a área de um triângulo usando a formula de Heron."
  [a b c]
  (let [semiperimetro (/ 
                       (calc-perimetro a b c) 
                       2)]
    (math/sqrt (*
                semiperimetro
                (- semiperimetro a)
                (- semiperimetro b)
                (- semiperimetro c)))))
  

(defn calc-altura
  "Calcula altura de A, dado a AREA."
  [a area]
  (/ (* 2 area) a))
  
(defn equilateral?
  "Verifica se o triangulo é equilateral"
  [a b c]
  (== a b c))
  
(defn isosceles?
  "Verifica se pelo menos dois lados sao iguais."
  [a b c]
  (or (== a b) (== b c) (== c a)))
  
(defn escaleno?
  "Verifica se os lados dos triangulos sao diferentes entre si."
  [a b c]
  (not (isosceles? a b c)))
  
(defn retangulo?
  "Verifica se é um triangulo retangulo, cujos angulos são iguais a 90o.
  O resultado não é exato, dado que cada angulo é arredondado utilizando clojure.math/round."
  ;; Qualquer triângulo retângulo, o quadrado da hipotenusa
  ;; equivale à soma dos quadrados dos catetos.
  [a b c]
  ;; (let 
  ;;  [calcular-pitagoras
  ;;   #(== (+ (* %1 %1) (* %2 %2)) (* %3 %3))]
  ;;   (or 
  ;;    (calcular-pitagoras a b c) 
  ;;    (calcular-pitagoras c b a) 
  ;;    (calcular-pitagoras a c b))
  ;;   )
  (let [angulos [(calc-angulo a b c)
                 (calc-angulo b c a)
                 (calc-angulo c a b)]]
    (if (some #(= 90 %) (map math/round angulos))
      true
      false)))
  
(defn obtuso?
  "Verifica se o triangulo é obtuso, tendo algum angulo >90o."
  [a b c]
  ;; a² + b² > c²
  ;; (>
  ;;  (+ (* a b) (* b b))
  ;;  (* c c))
  (let [angulos [(calc-angulo a b c)
                 (calc-angulo b c a)
                 (calc-angulo c a b)]]
    (if (some #(< 90 %) (map math/round angulos))
      true
      false)))
  
(defn agudo?
  "Verifica se o triangulo é obtuso, tendo algum angulo >90o."
  [a b c]
  ;; a² + b² < c²
  ;; (<
  ;;  (+ (* a b) (* b b))
  ;;  (* c c))
  (let [angulos [(calc-angulo a b c)
                 (calc-angulo b c a)
                 (calc-angulo c a b)]]
    (if (every? #(> 90 %) (map math/round angulos))
      true
      false)))
  
(defn gerar-dados-completos
  [a b c]
  (let [area (calc-area a b c)]
       {:lados [a b c]
        :retagulo (retangulo? a b c)
        :obtuso (obtuso? a b c)
        :agudo (agudo? a b c)
        :escaleno (escaleno? a b c)
        :isosceles (isosceles? a b c)
        :equilateral (equilateral? a b c)
        :area area
        :altura [(calc-altura a area)
                 (calc-altura b area)
                 (calc-altura c area)]
        :angulos [(calc-angulo a b c)
                  (calc-angulo b c a)
                  (calc-angulo c a b)]}))

(comment
  (calc-radianos 8 6 7)
  (calc-angulo 8 6 7)
  (calc-area 8 6 7)
  (calc-altura 8 (calc-area 8 6 7))
  (isosceles? 3 4 4)
  (equilateral? 4 4 4)
  (escaleno? 1 4 3) 
  (== 90.0 90)
  (retangulo? 8 6 7) ;; false
  (retangulo? 5 13 12) ;; true
  (retangulo? 8 10 6) ;; true

  (calc-angulo 8 10 6) ;; A 
  (calc-angulo 6 10 8) ;; B
  (calc-angulo 8 6 10) ;; C

  (math/round 90.00000807310232)
  (map math/round [35.322649843393165 22.67189532884259 122.00545482776425])
  (some #(= 90 %) [12 33 11])

  ; 90 > %
  (every? #(> 90 %) [90.00000807310232 59.999991926898005 29.999999999999673])
  (every? #(> 90 %) [35.322649843393165 22.67189532884259 122.00545482776425])
  (every? #(> 90 %) [35.322649843393165 22.67189532884259 22.00545482776425])

  (require 'clojure.pprint)
  (escaleno? 60 51.96152 30)
  (retangulo? 60 51.96152 30)
  (clojure.pprint/pprint (gerar-dados-completos 30 20 44))
  (clojure.pprint/pprint (gerar-dados-completos 60 51.96152 30))
  (clojure.pprint/pprint (gerar-dados-completos 15.14741 28.08887 30)))
  
