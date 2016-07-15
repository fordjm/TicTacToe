(ns game.board
  (:require [clojure.set :as set]))

(def line-size 3)

(defn compute-size [line-sz]
  (* line-sz line-sz))

(def size (compute-size line-size))

(defn compute-corners [line-sz]
  (let [sz (compute-size line-sz)]
    (sorted-set 0
                (dec line-sz)
                (- sz line-sz)
                (dec sz))))

(def corners (compute-corners line-size))

(defn compute-opposites [cnrs]
  {(first cnrs) (last cnrs),
  (second cnrs) (second (rest cnrs)),
  (second (rest cnrs)) (second cnrs),
  (last cnrs) (first cnrs)})

(def opposites (compute-opposites corners))

(defn compute-odd-center [sz]
  (sorted-set (int (Math/floor (/ sz 2)))))

(defn compute-even-center [gap i j k l]
  (if (= k (+ j gap))
    (sorted-set i j k l)
    (compute-even-center gap (+ i (+ 2 gap)) (+ j gap) (- k gap) (- l (+ 2 gap)))))

(defn compute-center [line-sz]
  (let [sz (compute-size line-sz)]
    (if (odd? sz)
      (compute-odd-center sz)
      (apply compute-even-center (cons (dec line-sz) (compute-corners line-sz))))))

(def center (compute-center line-size))

(defn compute-sides [line-sz]
  (let [sz (compute-size line-sz)]
    (apply sorted-set (set/difference (set (range sz))
                                      (compute-center line-sz)
                                      (compute-corners line-sz)))))

(def sides (compute-sides line-size))

(def empty-board (vec (range size)))

;===ABOVE LINE DEPENDS ON CREATION TIME VALUES, BELOW LINE DEPENDS ON CURRENT VALUES===

(defn rows [board]
  (partition line-size board))

(defn cols [board]
  (apply map vector (rows board)))

(defn diags [board]
  (for [outer [(rows board) (reverse (cols board))]]
    (for [inner (range (count outer))]
      (nth (nth outer inner) inner))))

(defn sections [board]
  (concat (rows board) (cols board) (diags board)))

(defn available [board]
  (filter integer? board))

(defn win? [board]
  (some (fn [section] (apply = section))
    (sections board)))

(defn tie? [board]
  (and (empty? (available board))
       (not (win? board))))

(defn game-over? [board]
  (or (win? board) (tie? board)))

(defn winner [board]
  (some (fn [section] (if (= 1 (count (distinct section))) (first section)))
    (sections board)))

(defn selectable? [board space]
  (integer? (nth board space)))
