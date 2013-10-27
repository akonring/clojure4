(ns clojure4.core)

;; This is Clojure practice from www.4clojure.com

;; Step 22
(def count4 
  (fn [xs]
    (loop [list xs n 0]
      (if (= list '())
        n
        (recur (rest list) (inc n))))))

;; Step 23
(def reverse-seq 
  (fn [xs]
    (reduce conj '() xs)))

;; Step 24
(def adding
  (fn [xs]
    (reduce + xs)))

;; Step 25
(def odd
  (fn [xs]
    (filter #(= (mod % 2) 0) xs)))

;; Step 26
(def fib
  (fn [n]
    (reverse (loop [list '(1 1) n (- n 2)]
               (if (= n 0)
                 list
                 (recur (cons (+ (first list) (second list)) list) (dec n)))))))

;; Step 27
(def pal
  (fn [xs]
    (let [list xs revlist (reverse xs)]
      (= list 
         (if (string? list)
           (apply str revlist)
           revlist))))) 

;; Step 28
(def flat
  (fn [xs]
    (if (sequential? xs)
      (mapcat flat xs)
      (list xs))))

;; Step 29
(def filter-caps 
  (fn [xs]
    (apply str (re-seq #"[A-Z]" xs))))

;; Step 30
(def remove-dup 
  (fn [xs]
    (reverse (loop [xs xs list '()]
               (cond 
                (= (first xs) nil) list
                (= (first xs) (first list)) (recur (rest xs) list)
                :else (recur (rest xs) (conj list (first xs))))))))

;; Step 31
(def pack
  (fn [xs]
    (partition-by identity xs)))

;; Step 32
(def dub-seq
  (fn [xs]
    (mapcat #(list % %) xs)))

;; Step 33
(def mult-seq
  (fn [xs n]
    (apply concat (mapcat #(list (repeat n %)) xs))))

;; Step 34
(def range
  (fn [first last]
    (take (- last first) (iterate inc first))))

;; Step 35
7

;; Step 36
;; x+y=10
;; y+z=4
;; z=1
'(x 7 y 3 z 1)

;; Step 37
"ABC"

;; Step 38
(def maximum
  (fn [& xs]
    (reduce 
     (fn [x y]
       (if (> x y)
         x
         y))
     0
     xs)))

;; Step 39
(def interleave
  (fn [xs ys]
    (mapcat list xs ys)))

;; Step 40
(def interpose
  (fn [del xs]
    (let 
        [list (mapcat #(list % del) xs) 
         length (* (count xs) 2)]
      (take (- length 1) list))))

;; Step 41
(def dropnth
  (fn [xs n]
    (keep-indexed
     #(if (= 0 (mod (inc %1) n))
        nil
        %2)
     xs)))

;; Step 42
(def fac
  (fn [n]
    (apply * (range 1 (inc n)))))

;; Step 43
(def reverse-interleave
  (fn [xs n]
    (let [filter-by-index
          (fn [xs n k]
            (map second (filter #(= n (mod (first %) k)) (map-indexed list xs))))]
      (for [x (range n)
            :let [y (filter-by-index xs x n)]]
        y))))

;; Step 44
;; Properbly the ugliest code ever written.
;; I am tired and got an early running session
;; If not excellence now, then when? -> tomorrow :)

(def rotate
  (fn [n xs]
    (let [v (vec xs)
          abs (fn [x]
                (if (< x 0)
                  (- x)
                  x))
          rotate #(vec (cons (peek %) (pop %)))]
      
      (if (< n 0)
        (last (take (inc (abs n)) (iterate rotate v)))
        (reverse (last (take (inc (abs n)) (iterate rotate (vec (reverse v))))))))))

;; Step 45
'(1 4 7 10 13)

;; Step 46
(fn [f]
  (fn [x y]
    (f y x)))

;; Step 47
4

;; Step 48
6

;; Step 49
(def split-seq
  (fn [n xs]
    (list (take n xs) (take-last (- (count xs) n) xs))))

;; Step 50
(def split-by-type
  #(vals (group-by type %)))

;; Step 51
'(1 2 3 4 5)

;; Step 52
'(c e)

;; Step 53
(def inc-sub-seq
  (fn [xs]
    (reverse (loop [xs xs t (list (first xs))  winner '()]
               (cond
                (empty? xs) winner
                (>= (first t) (first xs))
                (recur (rest xs) (list (first xs)) winner)
                :else 
                (recur (rest xs) (cons (first xs) t)
                       (if (>= (count winner) (count (cons (first xs) t)))
                         winner
                         (cons (first xs) t))))))))

;; Step 54
(def split-in-group
  (fn [n xs]
    (reverse (loop [xs xs c 1 list '() result '()]
      (if (empty? xs)
        result
        (if (= c n)
          (recur (rest xs) 1 '() (cons (reverse (cons (first xs) list)) result))
          (recur (rest xs) (inc c) (cons (first xs) list) result)))))))

;; Step 55
(def freq
  (fn [xs]
    (into {} (for [[k v] (group-by identity xs)] [k (count v)]))))




;; Step 64
+


  
  





  

  

  

