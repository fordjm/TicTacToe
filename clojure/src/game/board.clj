(ns game.board)

(def size 9)
(def center #{4})
(def corners #{0 2 6 8})
(def opposites {0 8, 2 6, 6 2, 8 0})
(def sides #{1 3 5 7})

(def line-size (int (Math/sqrt size)))
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
