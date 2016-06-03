(ns game.coach
	(:require [game.board :refer :all]))

(defn mini-move [game idx]
	(assoc (:board game) idx (:p1 game)))

(defn mini-game [board p1 p2]
	{:board board :p1 p1 :p2 p2})

(defn winning-moves [game]
	(filter (fn [idx] (win? (mini-move game idx))) (available (:board game))))

(defn blocking-moves [game]
	(winning-moves (mini-game (:board game) (:p2 game) (:p1 game))))

(defn threat? [candidate token]
	(letfn [(matches [coll char]
						(filter (fn [x] (= char x)) coll))]
		(and
			(= 1 (count (available candidate)))
			(= 2 (count (matches candidate token))))))

(defn threats [board token]
	(filter (fn [section] (threat? section token)) (sections board)))

(defn forking-moves [game]
	(letfn [(fork? [game]
						(true? (< 1 (count (threats (:board game) (:p2 game))))))]
		(filter (fn [idx] (fork? (mini-game (mini-move game idx) (:p2 game) (:p1 game))))
						(available (:board game)))))

(defn defensive-fork-blocks [game]
	(forking-moves (mini-game (:board game) (:p2 game) (:p1 game))))

(defn available-threats [board token]
	(filter (fn [idx] (true? (< 0 (count (threats (assoc board idx token) token))))) (available board)))

(defn offensive-fork-blocks [game]
	(remove (fn [idx] (corners idx)) (available-threats (:board game) (:p1 game))))

(defn fork-blocks [game]
	(concat (offensive-fork-blocks game) (defensive-fork-blocks game)))

(defn opposite-corners [game]
	(let [opposites {0 8, 2 6, 6 2, 8 0}
				opponent (:p2 game)
				to-oppose (filter (fn [idx] (= opponent (nth (:board game) idx))) corners)]
		(for [corner to-oppose] (get opposites corner))))

(defn selectable? [spaces idx]
	(contains? (set (available spaces)) idx))

(defn best-by-position [game]
	(filter (fn [idx] (selectable? (:board game) idx))
					(flatten [center
										(opposite-corners game)
										(seq corners)
										(seq sides)])))

(defn prioritized-moves [game]
	(flatten [(winning-moves game)
						(blocking-moves game)
						(forking-moves game)
						(fork-blocks game)
						(best-by-position game)]))

(defn advise [game]
	(if (nil? game)
		nil
		(some identity (prioritized-moves game))))
