(ns game.core)

(def size 9)
(def moves (atom []))
(def new-game {:board (vec (range size)) :p1 'X :p2 'O :ongoing true :winner nil})

(defn make-game []
	"TODO:  Complete validator - may be different for each type"
	(let [game (atom new-game)]
		(set-validator! game (fn [newval]
													 (every? (fn [key] (contains? newval key))
																	 [:board :p1 :p2])))
		game))

(defn rows [board]
	(partition 3 board))

(defn cols [board]
	(apply map vector (rows board)))

(defn diags [board]
	"Doesn't read well"
	(for [outer [(rows board) (reverse (cols board))]]
		(for [inner (range (count outer))]
			(nth (nth outer inner) inner))))

(defn sections [board]
	(concat (rows board) (cols board) (diags board)))

(defn win? [board]
	(some (fn [coll] (apply = coll))
				(sections board)))

(defn tie? [board]
	(and (empty? (filter integer? board))
			 (not (win? board))))

(defn game-over? [board]
	(or (win? board) (tie? board)))

(defn winner [board]
	(if (win? board)
		(apply first (filter (fn [coll] (= 1 (count (distinct coll))))
												 (sections board)))))

(defn move [game space]
	"Are the two swaps a design smell?  Temporal dependency? (no proof yet...)"
	(if (contains? (set (:board @game)) space)
		(let [p1 (:p1 @game)]
			(swap! game assoc
						 :board (assoc (:board @game) space p1)
						 :p1 (:p2 @game)
						 :p2 p1
						 :space space)
			(swap! game assoc
						 :ongoing (not (game-over? (:board @game)))
						 :winner (winner (:board @game))))
		{}))

(defn reset [game]
	(swap! game (fn [oldval] new-game)))

(defn make-move [game space]
	(fn [] (move game space)))

(defn add-move-to-history [move newval]
	(if (not (empty? newval))
		(swap! moves conj move)))

(defn execute-move [move]
	(let [newval (move)]
		(add-move-to-history move newval)
		newval))
