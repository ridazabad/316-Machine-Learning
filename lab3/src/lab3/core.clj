(ns lab3.core
  (:gen-class))

;Create
(defn create-matrix "takes the values for rows and cols and creates a 2D matrix" [x y]
  (if (> y 0)
     (do
       (vec (replicate x (vec (replicate y 0))))
     )
  )
)

;prints
(defn print-matrix "takes a matrix and prints all the values within it" [matrix]

    (println matrix)
)

;dimensions
(defn get-dims-matrix "returns the dimensions of the matrix" [matrix]

  (apply hash-map [:row (count matrix) :column (count(first matrix))])

)

;get vals
(defn get-matrix "gets the value in certain row col" [matrix x y]

  (nth (nth matrix x) y)

)

;change the values
(defn set-matrix "change the values of the point" [matrix x y z]

  (assoc matrix x (assoc (nth matrix x) y z))

)

;get the row
(defn get-matrix-row "Reurns the row we try to find" [matrix x]

  (nth matrix x)

)

;get the column
(defn get-matrix-column "Reurns the column we try to find" [matrix y]

  (vec (nth (apply mapv vector matrix) y))

)

;get the value index
(defn query-matrix "Find the index of the value we need" [matrix]

    (loop [i (count matrix)]

      (if (> i 0)

        (println i)(recur (- i 1))

      )

    )

)

(defn -main
  "matrix functionality"
  [& args]
  (let [initial-matrix (create-matrix 5 5)
        modified-matrix (-> (set-matrix initial-matrix 2 2 5)
                            (set-matrix 3 3 70)
                            (set-matrix 1 1 6))]
                                            (println "--Main Initial Matrix--")
                                            (print-matrix initial-matrix)
                                            (println "--Dimensions--")
                                            (println (get-dims-matrix initial-matrix))
                                            (println "--Mod Matrix--")
                                            (print-matrix modified-matrix)
                                            (println "--Get the value--")
                                            (println (get-matrix modified-matrix 2 2))
                                            (println "--Display the Row--")
                                            (println (get-matrix-row modified-matrix 2))
                                            (println "--Display the Col--")
                                            (println (get-matrix-column modified-matrix 3))
  )

)
