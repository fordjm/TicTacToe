(ns game.core
	(:require [game.board :refer :all]
						[game.game-maker :as maker]
						[game.coach :as coach]))

(def moves (atom []))

(defn minify-game [game]
	(let [gameval @game
				p1val (:p1 gameval)
				p2val (:p2 gameval)]
		{:board (:board gameval) :p1 (:token p1val) :p2 (:token p2val)}))

(defn move
	([game] (move game (coach/choose-move (minify-game game))))
	([game space] (if (contains? (set (:board @game)) space)
									(let [p1 (:p1 @game)
												board (assoc (:board @game) space (:token p1))]
										(swap! game assoc
													 :board board
													 :p1 (:p2 @game)
													 :p2 p1
													 :space space
													 :ongoing (not (game-over? board))
													 :winner (winner board)))
									{})))

(defn reset [game]
	"TODO:  Reset game without game-maker dependency (keep initial state or params)"
	(let [request (maker/extract-request @game)]
		(swap! game (fn [oldval] (maker/setup-game request)))))

(defn make-move
	([game] (fn [] (move game)))
	([game space] (fn [] (move game space))))

(defn add-move-to-history [move newval]
	(if (not (empty? newval))
		(swap! moves conj move)))

(defn execute-move [move]
	(let [newval (move)]
		(add-move-to-history move newval)
		newval))
