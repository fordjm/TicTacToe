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

(def game (make-game))

(defn rows []
	(partition 3 (:board @game)))

(defn cols []
	(apply map vector (rows)))

(defn diags []
	"Doesn't read well"
	(for [outer [(rows) (reverse (cols))]]
		(for [inner (range (count outer))]
			(nth (nth outer inner) inner))))

(defn sections []
	(concat (rows) (cols) (diags)))

(defn win? []
	(some (fn [coll] (apply = coll))
				(sections)))

(defn tie? []
	(and (empty? (filter integer? (:board @game)))
			 (not (win?))))

(defn game-over? []
	(or (win?) (tie?)))

(defn winner []
	(if (win?)
		(apply first (filter (fn [coll] (= 1 (count (distinct coll))))
												 (sections)))))

(defn move [space]
	"Are the two swaps a design smell?  Temporal dependency? (no proof yet...)"
	(if (contains? (set (:board @game)) space)
		(let [p1 (:p1 @game)]
			(swap! game assoc
						 :board (assoc (:board @game) space p1)
						 :p1 (:p2 @game)
						 :p2 p1
						 :space space)
			(swap! game assoc
						 :ongoing (not (game-over?))
						 :winner (winner)))
		{}))

(defn reset []
	(swap! game (fn [oldval] new-game)))

(defn make-move [space]
	(fn [] (move space)))

(defn add-move-to-history [move oldval]
	(if (not= oldval @game)
		(swap! moves conj move)))

(defn execute-move [move]
	(let [oldval @game
				result (move)]
		(add-move-to-history move oldval)
		result))
