;; NAME: Rida Zabad
;; ID:   1278354


(ns assignment1.core
  (:gen-class))
;;Ask for the certain classes that we would like to use
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])


;;Create the multiset which holds a hashmap of keys and values of the characters
(defn  create-multiset [listvals]
  (reduce (fn [x y]
            (assoc x y (inc (x y 0)))) {} listvals
  )
)

;;Returns the size of a multiset with the total number of elements that exists
(defn size-multiset [multiset]
  (reduce + (vals multiset))
)

;;Return the intersection of two multisets and returns a multiset of the min values and the intersect
(use 'clojure.set)
(defn intersect-multisets [multiset1 multiset2]

    (select-keys

      (merge-with min multiset1 multiset2)
      (intersection (set(keys multiset1)) (set(keys multiset2)))

    )
)

;;Part 1 calling the past functions and test their fuinctionality
(defn  part1  []
2
(let [a (create-multiset '("ab" "cd" "ef" "ab" "fg" "ef" "ab" "dd"))
      b (create-multiset '("zz" "ef" "fg" "ef" "ab" "ab" "ef" "ef"))
      c (intersect-multisets a b)]
(println "a = " a ", size = " (size-multiset a))
(println "b = " b ", size = " (size-multiset b))
(println "c = " c ", size = " (size-multiset c))
)
)

;;Read a file con verts it to string and then makes a multiset
(defn create-multiset-from-text-file [filename]
  (create-multiset (str/split (slurp filename) #"[\s]"))
)

;;Doing full analysis of multisets and file comparing the pos to positive words and negative
;;with negative words
(defn analyse-sentiment [posmultiset negmultiset filename]

  (def filetocomp (create-multiset-from-text-file filename ))

  (let [posscore (size-multiset (intersect-multisets posmultiset filetocomp))
        negscore (size-multiset (intersect-multisets negmultiset filetocomp))
        finalscore (- posscore negscore)]

    (println "file  :"filename)
    (println "  Postive Score :" posscore)
    (println "  Negative Score :" negscore)
    (println "  Final Score :" finalscore)
  )
)

;;Main part2
(defn  part2 []


(let [pos (create-multiset-from-text-file "positive-words.txt")
      neg (create-multiset-from-text-file "negative-words.txt")]

    (println "size of pos  dictionary = " (size-multiset  pos))
    (println "size of neg  dictionary = " (size-multiset  neg))
    (analyse-sentiment  pos neg "pos1.txt")
    (analyse-sentiment  pos neg "pos2.txt")
    (analyse-sentiment  pos neg "pos3.txt")
    (analyse-sentiment  pos neg "neg1.txt")
    (analyse-sentiment  pos neg "neg2.txt")
    (analyse-sentiment  pos neg "neg3.txt")
  )
)

;;Call part1 and part2
(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (part1)
  (println "      ")
  (part2)
)

;;All code was understood and taken from the clojure API given in the main clojure.org
