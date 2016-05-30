(ns game.core)

(def size 9)
(def moves (atom []))
(def new-game {:board (vec (range size)) :ongoing true :winner nil})
(def new-players {:p1 {:token 'X} :p2 {:token 'O}})

(defn make-game []
	"TODO:  Complete validator - should be different for each type (TM)"
	(let [game (atom (merge new-game new-players))]
		(set-validator! game (fn [newval]
													 (every? (fn [key] (contains? newval key))
																	 [:board :p1 :p2])))
		game))

(defn setup-game []
	@(make-game))

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
	(if (contains? (set (:board @game)) space)
		(let [p1 (:p1 @game)
					board (assoc (:board @game) space (:token p1))]
			(swap! game assoc
						 :board board
						 :p1 (:p2 @game)
						 :p2 p1
						 :space space
						 :ongoing (not (game-over? board))
						 :winner (winner board)))
		{}))

(defn reset [game]
	(swap! game (fn [oldval] (setup-game))))

(defn make-move [game space]
	(fn [] (move game space)))

(defn add-move-to-history [move newval]
	(if (not (empty? newval))
		(swap! moves conj move)))

(defn execute-move [move]
	(let [newval (move)]
		(add-move-to-history move newval)
		newval))
