(ns game.board-spec
	(:require [speclj.core :refer :all]
						[game.board :refer :all]
						[game.coach :as coach]))

(defn game-should-not-be-over [state]
	(should-not (or (game-over? state) (win? state) (tie? state))))

(defn game-should-have-winner [token state]
	(should= token (winner state)))

(defn assoc-all [v ks value]
	"Credit:  http://stackoverflow.com/questions/22730726/idiomatic-way-to-assoc-multiple-elements-in-vector"
	(reduce #(assoc %1 %2 value) v ks))

(defn game-should-be-over-with-ending [ending state]
	(should (and (game-over? state) (ending state))))

(defn game-should-not-have-ending [ending state]
	(should-not (ending state)))

(defn recognizes-winner-by-section [sections token]
	(for [section sections]
		(let [state (assoc-all empty-board section token)]
			(game-should-be-over-with-ending win? state)
			(game-should-not-have-ending tie? state)
			(game-should-have-winner token state))))

(defn make-cats-game [game]
	(if (empty? (available (:board game)))
		(:board game)
		(make-cats-game {:board (assoc (:board game) (coach/choose-move game) (:t1 game))
										 :t1 (:t2 game)
										 :t2 (:t1 game)})))

(describe "board"
	(it "recognizes an unfinished game"
			(let [state empty-board]
				(game-should-not-be-over state)
				(game-should-have-winner nil state)))
	(it "recognizes row winner"
			(recognizes-winner-by-section (rows empty-board) 'X))
	(it "recognizes a column winner"
			(recognizes-winner-by-section (cols empty-board) 'O))
	(it "recognizes a diagonal winner"
			(recognizes-winner-by-section (diags empty-board) 'X))
	(it "recognizes a tie - TODO:  Remove coach dependency"
			(let [state (make-cats-game {:board empty-board :t1 'X :t2 'O})]
				(game-should-be-over-with-ending tie? state)
				(game-should-not-have-ending win? state)
				(game-should-have-winner nil state))))
