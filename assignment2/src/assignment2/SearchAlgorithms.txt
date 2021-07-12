(ns assignment2.core
  (:gen-class))

(require '[clojure.string :as str])

;Read a map from a chosen file saving it into a hashmap of :map and :path using slurp
(defn read-map-from-file [filename] {:map (str/split (slurp filename) #"[\n]") :path []})

;Located where the Start "S" is in the map taking the hashmap of the map
(defn start [hashmap]

  ;Begin a with-local-vars and define a variable y with the number of rows in the map
  ;and a posstrt that return the hash of x and y
  (with-local-vars [y (count(get hashmap :map)) posstart {}]

    ;loop while i is less than or equal to the number of rows
    (loop [i 0] (when (<= i (- @y 1))

                  ;Check if there is an occurance of "S" in the row
                  (if (str/includes? (nth (get hashmap :map) i) "S")

                    ;if "S" in row def a variable to store the x and y values
                    (var-set posstart {:x (str/index-of (nth (get hashmap :map) i) "S") :y i})
                  )

                  ;recur with incremented value of i
                  (recur (+ i 1))
                )
    )
    ;Return the hashmap of the x and y values
    @posstart
  )
)

;Located where the Start "G" is in the map taking the hashmap of the map
(defn goal [hashmap]

  ;Begin a with-local-vars and define a variable y with the number of rows in the map
  ;and a possgoal that returns the hash of x and y
  (with-local-vars [y (count(get hashmap :map)) possgoal {}]

    ;loop while i is less than or equal to the number of rows
    (loop [i 0] (when (<= i (- @y 1))

                  ;Check if there is an occurance of "G" in the row
                  (if (str/includes? (nth (get hashmap :map) i) "G")

                    ;if "G" in row def a variable to store the x and y values
                    (var-set possgoal {:x (str/index-of (nth (get hashmap :map) i) "G") :y i})
                  )

                  ;recur with incremented value of i
                  (recur (+ i 1))
                )
    )

    ;Return the hashmap of x and y values
    @possgoal
  )
)

;Finds the number of steps taken
(defn cost [hashmap]

  ;Get the number of values in vector :path of hashmap
  (count(get hashmap :path))

)

;Takes the hashmap of the map to calculate current position of object
(defn position [hashmap]
  (
    ;Begin a with-local-vars creating variables a for the vector of path coordinates for the start x and y
    ;and size for the size of the path vector
    ;xpos for the x position
    ;ypos for the y position
    ;a hashmap posxy that saves the x and y position
    with-local-vars [a (get hashmap :path)
                     size (count (get hashmap :path))
                     xpos (get (start hashmap) :x)
                     ypos (get (start hashmap) :y)
                     posxy {}]

    ;loop while not the end of the path
    (loop [i 0]
      (when (<= i (- @size 1))

        ;If value is north subtract by 1 from ypos
        (if (= (nth @a i) :north) (var-set ypos (- @ypos 1)))
        ;if value is south add by 1 to ypos
        (if (= (nth @a i) :south) (var-set ypos (+ @ypos 1)))
        ;if value is east add by 1 to xpos
        (if (= (nth @a i) :east)  (var-set xpos (+ @xpos 1)))
        ;if value is west subtract by 1 from xpos
        (if (= (nth @a i) :west)  (var-set xpos (- @xpos 1)))
        ;recur with i + 1
        (recur (+ i 1))
      )
    )

    ;Create a hashmap of both values for x and y after moving
    (var-set posxy {:x @xpos :y @ypos})

    ;Return hashmap of current x and y
    @posxy
  )
)

;Print the map in easily readable format with the dots
(defn print-state [hashmap]
  ;with-local-vars that has stored variables to be used in function
  ;themap storing the main map
  ;size1 storing the lenght of the vector of map
  ;ptf is path through file
  ;size2 us the total size of the path vector
  ;bigvec is tge main vector to print
  ;pathvec is the step to take
  ;minhash is the hashmap to pass to position to locate the x and y
  ;dotx is the x value
  ;doty is the y value
  ;rowp is the row to be changed and re added
  (with-local-vars [themap (get hashmap :map)
                    size1 (count (get hashmap :map))
                    ptf (get hashmap :path)
                    size2 (count (get hashmap :path))
                    bigvec []
                    pathvec []
                    minhash {}
                    dotx 0
                    doty 0
                    rowp 0]

    ;loop to save the map into the big vector in order to change
    ;to add the dots into the map
    (loop [i 0]
      (when (<= i (- @size1 1))

        ;gets the value at the nth position and saves it in bigvec
        (var-set bigvec (conj @bigvec (vec (nth @themap i))))
        (recur (+ i 1))
      )
    )

    (loop [i 0]
      (when (<= i (- @size2 1))


        ;Add the value into a vector to call position to locate position
        (var-set pathvec (conj @pathvec (nth @ptf i)))

        ;Save the previous path vector into a hashmap to call into position
        (var-set minhash {:map (get hashmap :map) :path @pathvec})


        ;Save the x and y value
        (var-set dotx (get (position @minhash) :x))
        (var-set doty (get (position @minhash) :y))

        ;Go to the row by using the y value
        (var-set rowp (nth @bigvec @doty))

        ;Chance the value at posiiton to a "." and add it back to the main
        ;map in order to print
        (var-set bigvec (assoc @bigvec @doty (assoc @rowp @dotx ".")))
        (recur (+ i 1))
      )
    )

    ;Print the whole map inside of the vector in an easy to comprehend way
    (loop [i 0]
      (when (<= i (- @size1 1))
        (println (nth @bigvec i))
        (recur (+ i 1))
      )
    )
  )
)

;Calculate the heuristic using goal and current posiitons
(defn heuristic [hashmap]

  ;get the hashmaps coordinates for goal and current
  (with-local-vars [px (get (position hashmap) :x)
                    py (get (position hashmap) :y)
                    gx (get (goal hashmap) :x)
                    gy (get (goal hashmap) :y)]

    ;Heuristic formula
    (Math/sqrt (+ (* (- @px @gx) (- @px @gx)) (* (- @py @gy) (- @py @gy))))
  )
)

;Expand to see where are the possible traversable movements
(defn expand [hashmap]

  ;with-local-vars that storeds variables
  ;pathm is the path to follow
  ;lastxy is the last step that was taken in hashmap format
  ;pathnorth, pathsouth, patheast, pathwest gets the respective position in the map
  ;north south east west are the string that is held in that position
  ;expvec are all pathes that can be traversed
  (with-local-vars [pathm (get hashmap :path)
                    lastxy {}
                    lasttrav (last (get hashmap :path))
                    tempath []
                    arrayofpos []
                    pathnorth {:x (get (position hashmap) :x) :y (- (get (position hashmap) :y) 1)}
                    pathsouth {:x (get (position hashmap) :x) :y (+ (get (position hashmap) :y) 1)}
                    patheast {:x (+ (get (position hashmap) :x) 1) :y (get (position hashmap) :y)}
                    pathwest {:x (- (get (position hashmap) :x) 1) :y (get (position hashmap) :y)}
                    north ""
                    south ""
                    east ""
                    west ""
                    expvec []]


    ;Stores what is in that position in the map to check if traversable by getting the row
    ;and then getting the value from its x position
    (var-set north (nth (vec (nth (get hashmap :map) (get @pathnorth :y))) (get @pathnorth :x)))
    (var-set south (nth (vec (nth (get hashmap :map) (get @pathsouth :y))) (get @pathsouth :x)))
    (var-set east (nth (vec (nth (get hashmap :map) (get @patheast :y))) (get @patheast :x)))
    (var-set west (nth (vec (nth (get hashmap :map) (get @pathwest :y))) (get @pathwest :x)))

    ;;Computed vector of all points on path
    (loop [i 0]
      (when(<= i (- (count @pathm) 1))

        (var-set tempath (conj @tempath (get @pathm i)))
        (var-set arrayofpos (conj @arrayofpos (position {:map (get hashmap :map) :path @tempath})))
        (recur (+ i 1))
      )
    )


    ;First checks every direction if there is an empty space or is the goal state
    ;If true second checks if it is not a path that has ben previously traversed
    ;All paths that are possible to traverse will be saved into a vector
    (if (or (= @north (first (vec " "))) (= @north (first (vec "G")))) (if (= (some #(= @pathnorth %) @arrayofpos) nil) (var-set expvec (conj @expvec :north))))
    (if (or (= @south (first (vec " "))) (= @south (first (vec "G")))) (if (= (some #(= @pathsouth %) @arrayofpos) nil) (var-set expvec (conj @expvec :south))))
    (if (or (= @east (first (vec " "))) (= @east (first (vec "G")))) (if (= (some #(= @patheast %) @arrayofpos) nil) (var-set expvec (conj @expvec :east))))
    (if (or (= @west (first (vec " "))) (= @west (first (vec "G")))) (if (= (some #(= @pathwest %) @arrayofpos) nil) (var-set expvec (conj @expvec :west))))

    ;Return the vector
    @expvec

  )
)


;-----------------------------------------------------------------------------------------------------------------------------;
;Verbose flag
(def verbose false)
;Part2 for heuristic

(defn findbest [vectorh hashmap]
  ;Define the index of smallest
  ;Define the current smallest as the first value of the vector
  ;temp to compare to the smallest
  ;size of the vector of all values
  (with-local-vars [size (count vectorh)
                    index 0
                    smallest (heuristic {:map (get hashmap :map) :path (first vectorh)})
                    temp {}]


    ;Check if the vector has more than one value
    (if (> @size 1)
      (loop [i 1]

        ;Begin at the second value of vector
        ;Loop to find smallest
        (when (<= i (- @size 1))


          ;Call heuristic ;Define a hashmap to compute the heuristic
          (var-set temp (heuristic {:map (get hashmap :map) :path (nth vectorh i)}))

          ;Check if value is smaller
          (if (<= @temp @smallest)
            (do
              ;if it is smaller then save it in the place of smallest
              (var-set smallest @temp)
              ;save the index of the value
              (var-set index i)
            )
          )

          (recur (+ i 1))
          )
      )
    )

    ;return the value of the best pathwest
    (nth vectorh @index)
  )
)

(defn best-first [hashmap]

  ;beg as the beggining of the hashmap
  ;vectorh the main vector of values
  ;expvec are the expected next steps
  ;bestpath is the best path to follow heuristically
  ;uphash is an updated hashmap with the new added path
  ;poscurr is the current position
  ;curr is the value of current posiiton to compare to the above vector
  ;run is the number of runs to display
  (with-local-vars [beg (start hashmap)
                    vectorh []
                    expvec []
                    bestpath []
                    updhash {}
                    poscurr []
                    curr {}
                    run 0]

    ;First step if the hashmap's path is empty
    (if (= (get hashmap :path) [])

      ;if there are more than one value in the map
      (if (> (count (expand hashmap)) 1)
        (loop [i 0]

          ;loop for all posible next steps
          (when (<= i (- (count (expand hashmap)) 1))

            ;add to vectorh the new added path
            (var-set vectorh (conj @vectorh [(nth (expand hashmap) i)]))
            (recur (+ i 1))
          )
        )
      )
    )




    ;While we have not hit the goal
    (while (not= (position @updhash) (goal hashmap))


      (do
          ;Find best path
          ;Update the new hashmap
          ;get expected next steps
          ;remove the last expanded value
          (var-set bestpath (findbest @vectorh hashmap))
          (var-set updhash {:map (get hashmap :map) :path @bestpath})
          (var-set expvec (expand @updhash))
          (var-set vectorh (remove (fn [x] (= x @bestpath)) @vectorh))

          ;Loop to add all traversed positions in the poscurr vector
          (loop [i 0]
            (when (< i (- (count @vectorh) 1))

              (var-set poscurr (conj @poscurr (position {:map (get hashmap :map) :path (nth @vectorh i)})))
            )

          )


          ;For all the next new paths to add
          (if (= (count @expvec) 0)
            (do
              ;Remove path if there are no expansions possible
              (var-set vectorh (remove (fn [x] (= x @bestpath)) @vectorh))
            )
            (loop [i 0]
              (when (<= i (- (count @expvec) 1))

                ;save the current position by fololowing best path
                (var-set curr (position {:map (get hashmap :map) :path (conj @bestpath (nth @expvec i))}))

                ;check the current position has already been traversed by other paths prior
                ;if no other paths traversed into that posiiton add it to the main vector
                (if (= (some #(= @curr %) @poscurr) nil) (var-set vectorh (conj @vectorh (conj @bestpath (nth @expvec i)))))
                (recur (+ i 1))
              )
            )
          )

        (var-set run (+ @run 1))

        (if (= verbose true)
          (do
            ;Print the state of the map everyloop to keep up with steps
            ;print the runs
            (print-state @updhash)
            (println "Run Number: "@run)
          )

        )

      )
    )

    (if (= verbose false)
      (do

        ;Print the state of the map everyloop to keep up with steps
        ;print the runs
        (print-state @updhash)
        (println "Run Number: "@run)

      )
    )
  )
)

;-----------------------------------------------------------------------------------------------------------------------------
;Part2 for A*

(defn findbestcost [vectorh hashmap]
  ;Define the index of smallest
  ;Define the current smallest as the first value of the vector
  ;temp to compare to the smallest
  ;size of the vector of all values
  ;costvartemp is temporary value of cost
  ;heurvartemp is the temporary heuristic
  (with-local-vars [size (count vectorh)
                    index 0
                    costvar  (cost {:map (get hashmap :map) :path [first vectorh]})
                    costvartemp 0
                    heurvartemp 0
                    smallest (heuristic {:map (get hashmap :map) :path (first vectorh)})
                    temp {}]


    ;Change the smalles tto include cost and update the smallest to hold heuristic +cost
    (var-set costvar (cost {:map [] :path (first vectorh)}))
    (var-set smallest (+ @costvar @smallest))


    ;Check if the vector has more than one value
    (if (> @size 1)
      (loop [i 1]

        ;Begin at the second value of vector
        ;Loop to find smallest
        (when (<= i (- @size 1))


          ;calculate the heuristic and the cost and add them together into temp variable
          (var-set heurvartemp (heuristic {:map (get hashmap :map) :path (nth vectorh i)}))
          (var-set costvartemp (cost {:map (get hashmap :map) :path (nth vectorh i)}))
          (var-set temp (+ @heurvartemp @costvartemp))


          ;Check if value is smaller
          (if (< @temp @smallest)
            (do

              ;if smaller save temp into the smallest and check next iteration in the vector
              (var-set smallest @temp)
              (var-set index i)
            )
          )

          (recur (+ i 1))
          )
      )
    )

    ;Return the path that is the best using index in the main vector
    (nth vectorh @index)
  )
)


(defn a-star [hashmap]
  ;beg as the beggining of the hashmap
  ;vectorh the main vector of values
  ;expvec are the expected next steps
  ;bestpath is the best path to follow heuristically
  ;uphash is an updated hashmap with the new added path
  ;poscurr is the current position
  ;curr is the value of current posiiton to compare to the above vector
  ;run is the number of runs to display
  (with-local-vars [beg (start hashmap)
                    vectorh []
                    expvec []
                    bestpath []
                    updhash {}
                    poscurr []
                    curr {}
                    run 0]

    ;FCheck if the path is empty
    (if (= (get hashmap :path) [])

      ;check how many different steps can be taken
      (if (> (count (expand hashmap)) 1)
        (loop [i 0]

          ;loop till the end of the expanded vector
          (when (<= i (- (count (expand hashmap)) 1))

            ;add the first values into the main vector vectorh
            (var-set vectorh (conj @vectorh [(nth (expand hashmap) i)]))
            (recur (+ i 1))
          )
        )
      )
    )


    ;Define the first values to save into best path using findbestcost
    ;save it into the updated hash
    (var-set bestpath (findbestcost @vectorh hashmap))
    (var-set updhash {:map (get hashmap :map) :path @bestpath})



    ;While not hit the goal
    (while (not= (position @updhash) (goal hashmap))
      (do
          ;get the expected next steps
          (var-set expvec (expand @updhash))

          ;remove the expanded vector in order to add the new steps in
          (var-set vectorh (remove (fn [x] (= x @bestpath)) @vectorh))

          (loop [i 0]
            (when (< i (- (count @vectorh) 1))

              ;Loop to add all possible traversed paths as positions
              (var-set poscurr (conj @poscurr (position {:map (get hashmap :map) :path (nth @vectorh i)})))
            )

          )


          ;For all possible next steps
          (if (= (count @expvec) 0)
            (do

              ;if no where to go remove it from the main vector
              (var-set vectorh (remove (fn [x] (= x @bestpath)) @vectorh))
            )
            (loop [i 0]

              ;loop till the end of the expected vector
              (when (<= i (- (count @expvec) 1))

                ;get current position of the best path+next step
                (var-set curr (position {:map (get hashmap :map) :path (conj @bestpath (nth @expvec i))}))

                ;Check if step position is an already traversed pathwest if not add to the vector
                ;if it is in the path do not add it
                (if (= (some #(= @curr %) @poscurr) nil) (var-set vectorh (conj @vectorh (conj @bestpath (nth @expvec i)))))
                (recur (+ i 1))
              )
            )
          )

        ;redefine the next bestpath
        ;update the hash and loop back to the top
        (var-set bestpath (findbestcost @vectorh @updhash))
        (var-set updhash {:map (get hashmap :map) :path @bestpath})

        (var-set run (+ @run 1))

        (if (= verbose true)
          (do
            ;Print the state of the map everyloop to keep up with steps
            ;print the runs
            (print-state @updhash)
            (println "Run Number: "@run)
          )

        )

      )
    )

    (if (= verbose false)
      (do

        ;Print the state of the map everyloop to keep up with steps
        ;print the runs
        (print-state @updhash)
        (println "Run Number: "@run)

      )
    )
  )
)




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [a (read-map-from-file "map1.txt")
        b (read-map-from-file "map2.txt")
        c (read-map-from-file "map3.txt")
        d (read-map-from-file "map4.txt")]

    ;(println (position {:map (get a :map) :path [:west :west]}))
    ;(println (heuristic a))
    ;(println (expand {:map (get a :map) :path [:west :west :west :west :west :west]}))
    ;(print-state {:map (get a :map) :path [:north :east :north :north :west :west :north :north :north]})
    ;(println (expand {:map (get a :map) :path [:north :north :east :south]}))
    ;(println (expand {:map (get a :map) :path [:north :north :east :south]}))
    ;(println (findbest [[:north :north]] a))
    ;(println px)
    ;(print-state {:map (get a :map) :path [:north]})
    ;(println (findbest [[:west :west] [:north :north] [:north :east] [:east :east :east]] a))
    ;(allpossiblecost c)
    (best-first d)
    (println "--------------------------------------------------------------------------------")
    (a-star d)
    ;(println (cost {:map [] :path [:west :south :north :north]}))
    ;(findbestcost [[:west :west] [:north :north] [:north :east] [:east :east :east]] a)
  )
)
