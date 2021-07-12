(ns lab2.core
  (:gen-class))

(defn range1 [] (set (filter #(= (mod % 7) 0) (range 100 201 2)))

)

(defn values [] (apply hash-map(interleave (map #(keyword( str( char %))) (range 97 123)) (range 97 123)))

)

(defn -main
  "Trying to print"
  [& args]
  (println (values))

)






