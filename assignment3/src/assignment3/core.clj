(ns assignment3.core
  (:gen-class)
  (:import [java.awt.image  BufferedImage])
  (:import [javax.imageio  ImageIO])
  (:import [java.io File])
)

;create a new image with a specified width and height
(defn  new-image [width  height]
  (BufferedImage. width  height  BufferedImage/TYPE_INT_RGB)
)

;method that takes an image and returns width
(defn  get-width [image]
  (.getWidth  image)
)

;method that takes the image and returns height
(defn  get-height [image]
  (.getHeight  image)
)

;saves an image to witha specified filename and extension
(defn  save-image [image  extension  filename]
  (let [file (File. filename )]
    (ImageIO/write  image  extension  file)
  )
)

;defines the rgb value
(defn  get-rgb [image x y]
(let [rgb (.getRGB  image x y)
      red    (bit-shift-right (bit-and  rgb 0xFF0000) 16)
      blue   (bit-shift-right (bit-and  rgb 0xFF00) 8)
      green (bit-and  rgb 0xFF)]
      (vec (list  red  green  blue))

    )
)

;sets the rgb value at a current x and y
(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
     (let [rgb (+ (bit-shift-left red 16)
                  (bit-shift-left blue 8)
                  (bit-shift-left green 0) ) ]
       (.setRGB image x y rgb)
   )
)

;sets rgb toa grey value
(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
   (set-rgb image x y [grey grey grey])
)

;read image that returns an image that is grey scaled
(defn read-image [filename]
  (let [file (File. filename)
        main-image (ImageIO/read file)
        image-width (get-width main-image)
        image-height (get-height main-image)
        test-image (new-image image-width image-height)]

    ;loop through all x and y of image and set the grey scale of the image to a new image
    (dotimes [x image-width]
      (dotimes [y image-height]
        (let [rgb  (get-rgb main-image x y)
             grey (int (/ (reduce + rgb) 3.0))]
          (set-grey test-image x y grey)
        )
      )
    )
    test-image
  )
)


;-----------------------------------------
;----------------PART1--------------------
;-----------------------------------------


;Method that reutrns the value to change the center of the matrix
;valuie that changes the given xy pixel
(defn get-pix[image x y index]

    ;Local variables for the centervalue, vect to store the chosen Filter, and vec of vec of all possible filters
    (with-local-vars [centerval 0
                      vect []]
      (let [mvec [[-1 0 1 -2 0 2 -1 0 1]
                 [-2 -1 0 -1 0 1 0 1 2]
                 [-1 -2 -1 0 0 0 1 2 1]
                 [0 -1 -2 1 0 -1 2 1 0]
                 [1 0 -1 2 0 -2 1 0 -1]
                 [2 1 0 1 0 -1 0 -1 -2]
                 [1 2 1 0 0 0 -1 -2 -1]
                 [0 1 2 -1 0 1 -2 -1 0]]]

        ;Chooses a filter from the vector of vectors
        (var-set vect (nth mvec index))

      )

      ;get all the values of the pixels around the matrix
      (var-set centerval
                    (+ (* (first (get-rgb image (- x 1)(- y 1))) (nth @vect 0))
                      (* (first (get-rgb image x (- y 1))) (nth @vect 1))
                      (* (first (get-rgb image (+ x 1)(- y 1))) (nth @vect 2))
                      (* (first (get-rgb image (- x 1) y)) (nth @vect 3))
                      (* (first (get-rgb image x y)) (nth @vect 4))
                      (* (first (get-rgb image (+ x 1) y)) (nth @vect 5))
                      (* (first (get-rgb image (- x 1)(+ y 1))) (nth @vect 6))
                      (* (first (get-rgb image x (+ y 1))) (nth @vect 7))
                      (* (first (get-rgb image (+ x 1)(+ y 1))) (nth @vect 8))
                    )
      )

      ;add 127 to the value to ensure it is within 0-255
      (var-set centerval (+ (int @centerval) 127))

      ;check if it is over or under 0-255
      (if (> @centerval 255) (var-set centerval 255)
        (if (< @centerval 0) (var-set centerval 0))
      )
      ;return
      @centerval
    )
)

;takes a file and an index that is to choose the given filter

;variable for rgb vector, main value, a grey image that is read, and a new image
(defn kirsh [file index]
  (with-local-vars [rgb []
                    value 0]
    (let [grey-image (read-image file)
          main-image (new-image (get-width grey-image) (get-height grey-image))
          output "test"]

      ;loop starting at 1 to before the last value on both x and y
      (loop [x 1]
        (when (< x (- (get-width grey-image) 1))
          (loop [y 1]
            (when (< y (- (get-height grey-image) 1))

              ;set new image rgb to that of the filtered value
              (var-set value (get-pix grey-image x y index))
              (set-grey main-image x y @value)
              (recur (+ y 1))
            )
          )
          (recur (+ x 1))
        )
      )

      ;save the image
      (save-image main-image "png" "test-image-mod5")

      )
  )
)

;-----------------------------------------
;----------------PART2--------------------
;-----------------------------------------

;a function that takes a vector and normalizes it
(defn normalize [vect]

  ;the final normalized vec
  (with-local-vars [normvec []]
    (let [maxv (reduce + vect)]

      ;loop a vec for 8 values
      (dotimes [x 8]

        (let [value (double (/ (nth vect x) maxv))]
          (var-set normvec (conj @normvec value))
        )
      )
    )
    @normvec
  )
)

;a method that returns the max value from applying all the filters
(defn get-max [image x y]
  (let [mvec [[-1 0 1 -2 0 2 -1 0 1]
              [-2 -1 0 -1 0 1 0 1 2]
              [-1 -2 -1 0 0 0 1 2 1]
              [0 -1 -2 1 0 -1 2 1 0]
              [1 0 -1 2 0 -2 1 0 -1]
              [2 1 0 1 0 -1 0 -1 -2]
              [1 2 1 0 0 0 -1 -2 -1]
              [0 1 2 -1 0 1 -2 -1 0]]]

    ;loop trhough all the filters and save the values returned into array
    ;to check what max is

    (with-local-vars [maxvec []
                      maxvalue 0]
      (dotimes [i 8]
        (let [histvec (nth mvec i)
              x1 (* (first (get-rgb image (- x 1) (- y 1))) (nth histvec 0))
              x2 (* (first (get-rgb image x (- y 1))) (nth histvec 1))
              x3 (* (first (get-rgb image (+ x 1) (- y 1))) (nth histvec 2))
              x4 (* (first (get-rgb image (- x 1) y)) (nth histvec 3))
              x5 (* (first (get-rgb image x y)) (nth histvec 4))
              x6 (* (first (get-rgb image (+ x 1) y)) (nth histvec 5))
              x7 (* (first (get-rgb image (- x 1) (+ y 1))) (nth histvec 6))
              x8 (* (first (get-rgb image x (+ y 1))) (nth histvec 7))
              x9 (* (first (get-rgb image (+ x 1) (+ y 1))) (nth histvec 8))
              ind (+ x1 x2 x3 x4 x5 x6 x7 x8 x9)]
          (var-set maxvec (conj @maxvec ind))
        )
        ;(var-set maxvec (conj @maxvec @value))
      )

      ;get the max value from the vector with all filter returned value
      (var-set maxvalue (apply max @maxvec))
      (if (> @maxvalue 255) (var-set maxvalue 255))

      @maxvalue
      ;(println (apply max @maxvec))
    )
  )
)

;Loop through creating ahistogram of all max values within 0-255
(defn edge-magnitude-hist [file]
  (with-local-vars [histvec [0 0 0 0 0 0 0 0]]
    (let [img (read-image file)
          image-width (get-width img)
          image-height (get-height img)]

      ;loop starting from (1,1) to before the last values
      (loop [x 1]
          (when (< x (- image-width 1))
            (loop [y 1]
              (when (< y (- image-height 1))

                ;divide by 32 to ensure it is in 8 bin ahistogram
                ;call previous method to get the max
                (let [histval (int (/ (get-max img x y) 32))]
                  (var-set histvec (update @histvec histval inc))
                )
                (recur (+ y 1))
              )
            )
            (recur (+ x 1))
          )
        )
    )

    ;normnalize the histvec and reutrns
    (var-set histvec (normalize @histvec))
    @histvec
  )
)

;applies all the filters and checks which filter returns the max value
(defn get-max-index [image x y]
  (let [mvec [[-1 0 1 -2 0 2 -1 0 1]
              [-2 -1 0 -1 0 1 0 1 2]
              [-1 -2 -1 0 0 0 1 2 1]
              [0 -1 -2 1 0 -1 2 1 0]
              [1 0 -1 2 0 -2 1 0 -1]
              [2 1 0 1 0 -1 0 -1 -2]
              [1 2 1 0 0 0 -1 -2 -1]
              [0 1 2 -1 0 1 -2 -1 0]]]
    (with-local-vars [maxvec []
                      direction 0]

      ;loop trhough every filter and get the value
      (dotimes [i 8]
        (let [histvec (nth mvec i)
              x1 (* (first (get-rgb image (- x 1) (- y 1))) (nth histvec 0))
              x2 (* (first (get-rgb image x (- y 1))) (nth histvec 1))
              x3 (* (first (get-rgb image (+ x 1) (- y 1))) (nth histvec 2))
              x4 (* (first (get-rgb image (- x 1) y)) (nth histvec 3))
              x5 (* (first (get-rgb image x y)) (nth histvec 4))
              x6 (* (first (get-rgb image (+ x 1) y)) (nth histvec 5))
              x7 (* (first (get-rgb image (- x 1) (+ y 1))) (nth histvec 6))
              x8 (* (first (get-rgb image x (+ y 1))) (nth histvec 7))
              x9 (* (first (get-rgb image (+ x 1) (+ y 1))) (nth histvec 8))
              ind (+ x1 x2 x3 x4 x5 x6 x7 x8 x9)]
          (var-set maxvec (conj @maxvec ind))
        )
        ;(var-set maxvec (conj @maxvec @value))
      )

      ;find the index of the max value which corresponds to the correct Histogram for the filter
      (var-set direction (.indexOf @maxvec (apply max @maxvec)))

      @direction
    )
  )
)

;created the direction Histogram
(defn edge-direction-hist [file]
  (with-local-vars [histvec [0 0 0 0 0 0 0 0]]
    (let [img (read-image file)
          image-width (get-width img)
          image-height (get-height img)]

      ;loop from (1,1) to before hte last value
      (loop [x 1]
          (when (< x (- image-width 1))
            (loop [y 1]
              (when (< y (- image-height 1))

                ;add values to the histogram at the returned direction value
                (var-set histvec (update @histvec (get-max-index img x y) inc))
                (recur (+ y 1))
              )
            )
            (recur (+ x 1))
          )
        )
    )

    ;normalize hte histogram
    (var-set histvec (normalize @histvec))
    @histvec
  )
)

;-----------------------------------------
;----------------PART3--------------------
;-----------------------------------------

;Make an intesity histogram
(defn make-histogram [file]
  (with-local-vars [rgbvec [0 0 0 0 0 0 0 0]]

    ;loop til width and height for the whole image
    (let [img (read-image file)]
      (dotimes [x (get-width img)]
        (dotimes [y (get-height img)]
          (let [rgb (get-rgb img x y)
                value (int(/ (first rgb) 32))]

            (var-set rgbvec (update @rgbvec value inc))
          )
        )
      )
    )

    @rgbvec
  )
)

;gets the 3 histograms and created a big 24 bin histogram and normalizes it
(defn image-descriptor [file]
  (with-local-vars [vect []]
    ;edgemagn histogram
    ;edgedirection histogram
    ;normalized intesity histogram
    (let [magvec (edge-magnitude-hist file)
          dirvec (edge-direction-hist file)
          histvec (normalize (make-histogram file))]
      (let [magvec (into magvec dirvec)
            magvec (into magvec histvec)]
        (dotimes [x 24]

          ;normalize by dividing all values by 3
          (let [value (double (/ (nth magvec x) 3))]

            ;add all values to main returned vec
            (var-set vect (conj @vect value))
          )
        )
      )
    )
    ;return
    @vect
  )
)

;-----------------------------------------
;----------------PART4--------------------
;-----------------------------------------

;compare 2 image descriptor values to make a 24 vector and getting the total to see how similar two images are
(defn image-similarity [file1 file2]
  (with-local-vars [vect []
                    return 0]

    ;image descriptor for 2 images
    (let [img1 (image-descriptor file1)
          img2 (image-descriptor file2)]
      (dotimes [i 24]

        ;compare all values taking the minimum and adding it to a new vector
        (let [value1 (nth img1 i)
              value2 (nth img2 i)]
          (if (= (compare value1 value2) 1) (var-set vect (conj @vect value2)) (var-set vect (conj @vect value1)))
        )
      )
    )

    ;get the total
    (var-set return (reduce + @vect))
    @return
  )
)

(defn -main
  [& args]
  (let [test-image (read-image "poro.png")
        image-width (get-width test-image)
        image-height (get-height test-image)]


  )
)
