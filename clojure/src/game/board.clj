(ns game.board
	(:require [clojure.set :as set]))

(def size 9)
(def center (sorted-set 4))

(def line-size (int (Math/sqrt size)))
(def corners (sorted-set 0
												 (dec line-size)
												 (- size line-size)
												 (dec size)))
(def opposites {(first corners) (last corners),
								(second corners) (second (rest corners)),
								(second (rest corners)) (second corners),
								(last corners) (first corners)})
(def sides (apply sorted-set (set/difference (set (range size)) center corners)))
(def empty-board (vec (range size)))

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
