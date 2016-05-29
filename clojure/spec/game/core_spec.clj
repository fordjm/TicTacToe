(ns game.core-spec
	(:require [speclj.core :refer :all]
						[game.core :refer :all]))

(def empty-board (vec (range 9)))
(def x-takes-4 {:board (assoc empty-board 4 'X) :space 4 :p1 'O :p2 'X :ongoing true :winner nil})

(defn make-board-state [p1-moves p2-moves]
	"How to refactor place-tokens? (Doesn't preserve move order anyway.)"
	(let [place-tokens (fn [board spaces token]
											(vec (reduce (fn [board idx] (assoc board idx token))
																	 board spaces)))]
		(place-tokens
			(place-tokens empty-board p1-moves 'X) p2-moves 'O)))

(def game (atom {}))

(defn moving-to-space-should-yield-response [space response]
	(should= response (execute-move (make-move game space))))

(defn interleave-moves [p1-moves p2-moves]
	(if (= (count p1-moves) (count p2-moves))
		(interleave p1-moves p2-moves)
		(cons (first p1-moves) (interleave p2-moves (rest p1-moves)))))

(defn run-moves [p1-moves p2-moves]
	"How can I refactor this?"
	(map execute-move
			 (map (fn [space] (make-move game space))
						(interleave-moves p1-moves p2-moves))))

(defn running-moves-should-yield-response [p1-moves p2-moves state]
	(let [executed (run-moves p1-moves p2-moves)]
		(should= state (last executed))))

(defn make-move-response [p1-moves p2-moves space p1 p2 ongoing winner]
	"TODO:  Refactor"
	{:board (make-board-state p1-moves p2-moves) :space space :p1 p1 :p2 p2 :ongoing ongoing :winner winner})

(describe "game.core"
	(before
		(reset game)
		(swap! moves (fn [oldval] [])))

	(it "creates a new game"
			(should= {:board empty-board :p1 'X :p2 'O :ongoing true :winner nil} @(make-game)))

	(it "does not move out-of-bounds"
			(moving-to-space-should-yield-response -1 {})
			(moving-to-space-should-yield-response 9 {}))

	(it "executes a legal move"
			(moving-to-space-should-yield-response 4 x-takes-4))

	(it "keeps a history"
			(let [executed (run-moves [4] [0])]
				(should= (make-move-response [4] [0] 0 'X 'O true nil)
								 (second executed))

				(reset game)
				(should= executed
								 (for [move @moves] (move)))))

	(it "does not move to occupied space"
			(running-moves-should-yield-response
				[4] [4]
				{}))

	(it "finds a top-row winner"
			(running-moves-should-yield-response
				[0 1 2] [3 4]
				(make-move-response [0 1 2] [3 4] 2 'O 'X false 'X)))

	(it "finds a second-row winner"
			(running-moves-should-yield-response
				[0 2 6] [3 4 5]
				(make-move-response [0 2 6] [3 4 5] 5 'X 'O false 'O)))

	(it "finds a third-row winner"
			(running-moves-should-yield-response
				[6 7 8] [3 4]
				(make-move-response [6 7 8] [3 4] 8 'O 'X false 'X)))

	(it "finds a first-column winner"
			(running-moves-should-yield-response
				[2 4 8] [0 3 6]
				(make-move-response [2 4 8] [0 3 6] 6 'X 'O false 'O)))

	(it "finds a second-column winner"
			(running-moves-should-yield-response
				[1 4 7] [2 5]
				(make-move-response [1 4 7] [2 5] 7 'O 'X false 'X)))

	(it "finds a third-column winner"
			(running-moves-should-yield-response
				[0 4 6] [2 5 8]
				(make-move-response [0 4 6] [2 5 8] 8 'X 'O false 'O)))

	(it "finds a first-diagonal winner"
			(running-moves-should-yield-response
				[0 4 8] [2 6]
				(make-move-response [0 4 8] [2 6] 8 'O 'X false 'X)))

	(it "finds a second-diagonal winner"
			(running-moves-should-yield-response
				[0 1 8] [2 4 6]
				(make-move-response [0 1 8] [2 4 6] 6 'X 'O false 'O)))

	(it "finds a cat's game"
			(running-moves-should-yield-response
				[0 8 3 2 7] [4 5 6 1]
				(make-move-response [0 8 3 2 7] [4 5 6 1] 7 'O 'X false nil)))

	(it "distinguishes a tie from a win"
			(running-moves-should-yield-response
				[0 1 5 6 2] [3 4 7 8]
				(make-move-response [0 1 5 6 2] [3 4 7 8] 2 'O 'X false 'X))
			(should-not (tie? (:board @game))))
	)
