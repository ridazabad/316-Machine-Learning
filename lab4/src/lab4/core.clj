(ns lab4.core
  (:gen-class))

(require '[clojure.string :as str])



(defn get-initial-state [] {:board "eeeeeeeee", :player :red})

(defn makemap [string color] {:board string :player color})

(defn is-goal [hashmap]

  (if
    (str/includes?  (get hashmap :board) "RRRR")

    ;Output if true
    "RRRR"

    ;Outpout if false
    (if (str/includes? (get hashmap :board) "BBBB") "BBBB")
  )
)

(defn action [hashmap position]

  (let [a (str/split (get hashmap :board) #"")
        b (str/join "" (if (= (get hashmap :player) :red) (assoc a position "R") (assoc a position "B")))]

   (if (= (nth a position) "e")
       (if (= (get hashmap :player) :red) (makemap b :blue) (makemap b :red))
     )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Test of  get-initial-state:")
  (println (get-initial-state))
  (println "Test of  is-goal:")
  (println (is-goal  {:board "eRRBBBBe" :player :red}))
  (println (is-goal  {:board "eeReBeeB" :player :red}))
  (println "Tests  of  action:")
  (println (action   {:board "eeeeeeee" :player   :red} 0))
  (println (action   {:board "BeeReeee" :player   :red} 5))
  (println (action   {:board "eeRRReee" :player   :blue} 7))
  (println (action   {:board "eeRRReee" :player   :blue} 2))
)
