(ns game.board-spec
	(:require [speclj.core :refer :all]
						[game.board :refer :all]
						[game.test-util :as util]))

(defn game-should-not-be-over [state]
	(should-not (or (game-over? state) (win? state) (tie? state))))

(defn game-should-have-winner [token state]
	(should= token (winner state)))

(defn game-should-be-over-with-ending [ending state]
	(should (and (game-over? state) (ending state))))

(defn game-should-not-have-ending [ending state]
	(should-not (ending state)))

(defn recognizes-winner-by-section [sections token]
	(doall
		(for [section sections]
			(let [state (util/assoc-all empty-board section token)]
				(game-should-be-over-with-ending win? state)
				(game-should-not-have-ending tie? state)
				(game-should-have-winner token state)))))

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
	(it "recognizes a tie"
			(let [state (map str empty-board)]
				(game-should-be-over-with-ending tie? state)
				(game-should-not-have-ending win? state)
				(game-should-have-winner nil state))))
