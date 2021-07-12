(ns lab6.core
  (:gen-class)
  (:import [java.awt.image  BufferedImage])
  (:import [javax.imageio  ImageIO])
  (:import [java.io File])
)

(defn  new-image [width  height]
  (BufferedImage. width  height  BufferedImage/TYPE_INT_RGB)
)

(defn read-image [filename]
  (let [file (File. filename)]
    (ImageIO/read file)
  )
)

(defn  save-image [image  extension  filename]
  (let [file (File. filename )]
    (ImageIO/write  image  extension  file)
  )
)

(defn  get-width [image]
  (.getWidth  image)
)

(defn  get-height [image]
  (.getHeight  image)
)

(defn  get-rgb [image x y]
(let [rgb (.getRGB  image x y)
      red    (bit-shift-right (bit-and  rgb 0xFF0000) 16)
      blue   (bit-shift-right (bit-and  rgb 0xFF00) 8)
      green (bit-and  rgb 0xFF)]
      (vec (list  red  green  blue))

    )
)

(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
     (let [rgb (+ (bit-shift-left red 16)
                  (bit-shift-left blue 8)
                  (bit-shift-left green 0) ) ]
       (.setRGB image x y rgb)
   )
)

(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
   (set-rgb image x y [grey grey grey])
)

(defn make-histogram [image]
  (with-local-vars [rgb []
                    value 0
                    rgbvec [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]
    (dotimes [x (get-width image)]
      (dotimes [y (get-height image)]
        (var-set rgb (get-rgb image x y))
        (var-set value (int(/ (first @rgb) 16)))
        ;(println @value)
        (var-set rgbvec (update @rgbvec @value inc))
        ;(println @rgbvec)
      )
    )
    (println @rgbvec)
    @rgbvec
  )
)

(defn threshold [image threshold]
  (with-local-vars [rgb []
                    value 0
                    change (new-image (get-width image) (get-height image))
                    width (get-width image)
                    height (get-height image)]
    (dotimes [x @width]
      (dotimes [y @height]
        (var-set rgb (get-rgb image x y))
        (var-set value (int(/ (first @rgb) 16)))

        (if (< @value threshold) (set-rgb @change x y [0 0 0]) (set-rgb @change x y [255 255 255]))
      )
    )
    (save-image @change "png" "test-image-mod")
  )
)

(defn get-pix[image x y vect]
    (with-local-vars [centerval 0]
      (var-set centerval
                    (+ (* (first (get-rgb image (- x 1)(- y 1))) (nth vect 0))
                      (* (first (get-rgb image x (- y 1))) (nth vect 1))
                      (* (first (get-rgb image (+ x 1)(- y 1))) (nth vect 2))
                      (* (first (get-rgb image (- x 1) y)) (nth vect 3))
                      (* (first (get-rgb image x y)) (nth vect 4))
                      (* (first (get-rgb image (+ x 1) y)) (nth vect 5))
                      (* (first (get-rgb image (- x 1)(+ y 1))) (nth vect 6))
                      (* (first (get-rgb image x (+ y 1))) (nth vect 7))
                      (* (first (get-rgb image (+ x 1)(+ y 1))) (nth vect 8))
                    )
      )

      (var-set centerval (int @centerval))
      @centerval
    )
)

(defn matrix [image]
  (with-local-vars [rgb []
                    value 0
                    main-image (new-image (get-width image) (get-height image))]
    (loop [x 1]
      (when (< x (- (get-width image) 1))
        (loop [y 1]
          (when (< y (- (get-height image) 1))

            (var-set value (get-pix image x y [0.25 0.5 0.25 0.5 -3 0.5 0.25 0.5 0.25]))
            (set-grey @main-image x y @value)
            (recur (+ y 1))
          )
        )
        (recur (+ x 1))
      )
    )
    (save-image @main-image "png" "test-image-mod")
  )
)


(defn -main
  [& args]
  (let [test-image (read-image "picture.png")
        image-width (get-width test-image)
        image-height (get-height test-image)
        main-image (new-image image-width image-height)]

    ;test
    ;(make-histogram test-image)

    ;grey scale
    (dotimes [x image-width]
      (dotimes [y image-height]
        (let [rgb  (get-rgb test-image x y)
             grey (int (/ (reduce + rgb) 3.0))
              r (int (* (nth rgb 0) 0.2125))
              g (int (* (nth rgb 1) 0.7154))
              b (int (* (nth rgb 2) 0.072))]
          (set-grey main-image x y grey)
          ;(set-rgb test-image x y [r g b])
        )
      )
    )

    ;test
    ;(make-histogram test-image)
    ;(threshold test-image 11)
    (matrix main-image)

    ;save
    (save-image main-image "png" "mai-grey")
  )
)
