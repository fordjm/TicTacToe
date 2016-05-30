(ns game.core-spec
	(:require [speclj.core :refer :all]
						[game.core :refer :all]))

(def empty-board (vec (range 9)))
(def p1 (:p1 new-players))
(def p2 (:p2 new-players))

(defn p1-takes-4 []
	{:board (assoc empty-board 4 p1) :space 4 :p1 p2 :p2 p1 :ongoing true :winner nil})

(defn make-board-state [p1-moves p2-moves]
	"How to refactor place-tokens? (Doesn't preserve move order anyway.)"
	(let [place-tokens (fn [board spaces token]
											(vec (reduce (fn [board idx] (assoc board idx token))
																	 board spaces)))]
		(place-tokens
			(place-tokens empty-board p1-moves p1) p2-moves p2)))

(def game (atom {}))

(defn interleave-moves [p1-moves p2-moves]
	(if (= (count p1-moves) (count p2-moves))
		(interleave p1-moves p2-moves)
		(cons (first p1-moves) (interleave p2-moves (rest p1-moves)))))

(defn make-moves [p1-moves p2-moves]
	(map (fn [space] (make-move game space))
			 (interleave-moves p1-moves p2-moves)))

(defn execute-moves [moves]
	(map execute-move moves))

(defn executing-moves-should-yield-response [p1-moves p2-moves state]
	(let [executed (execute-moves (make-moves p1-moves p2-moves))]
		(should= state (last executed))))

(defn make-move-response [p1-moves p2-moves space p1 p2 ongoing winner]
	"TODO:  Refactor"
	{:board (make-board-state p1-moves p2-moves) :space space :p1 p1 :p2 p2 :ongoing ongoing :winner winner})

(describe "game.core"
	(before
		(reset game)
		(swap! moves (fn [oldval] [])))

	(it "creates a new game"
			(should= (merge new-game new-players) (start-game)))

	(it "does not move out-of-bounds"
			(should= {} (execute-move (make-move game -1)))
			(should= {} (execute-move (make-move game 9))))

	(it "executes a legal move"
			(should= (p1-takes-4) (execute-move (make-move game 4))))

	(it "keeps a history"
			(let [executed (execute-moves (make-moves [4] [0]))]
				(should= (make-move-response [4] [0] 0 p1 p2 true nil)
								 (second executed))

				(reset game)
				(should= executed
								 (for [move @moves] (move)))))

	(it "does not move to occupied space"
			(executing-moves-should-yield-response
				[4] [4]
				{}))

	(it "finds a top-row winner"
			(executing-moves-should-yield-response
				[0 1 2] [3 4]
				(make-move-response [0 1 2] [3 4] 2 p2 p1 false p1)))

	(it "finds a second-row winner"
			(executing-moves-should-yield-response
				[0 2 6] [3 4 5]
				(make-move-response [0 2 6] [3 4 5] 5 p1 p2 false p2)))

	(it "finds a third-row winner"
			(executing-moves-should-yield-response
				[6 7 8] [3 4]
				(make-move-response [6 7 8] [3 4] 8 p2 p1 false p1)))

	(it "finds a first-column winner"
			(executing-moves-should-yield-response
				[2 4 8] [0 3 6]
				(make-move-response [2 4 8] [0 3 6] 6 p1 p2 false p2)))

	(it "finds a second-column winner"
			(executing-moves-should-yield-response
				[1 4 7] [2 5]
				(make-move-response [1 4 7] [2 5] 7 p2 p1 false p1)))

	(it "finds a third-column winner"
			(executing-moves-should-yield-response
				[0 4 6] [2 5 8]
				(make-move-response [0 4 6] [2 5 8] 8 p1 p2 false p2)))

	(it "finds a first-diagonal winner"
			(executing-moves-should-yield-response
				[0 4 8] [2 6]
				(make-move-response [0 4 8] [2 6] 8 p2 p1 false p1)))

	(it "finds a second-diagonal winner"
			(executing-moves-should-yield-response
				[0 1 8] [2 4 6]
				(make-move-response [0 1 8] [2 4 6] 6 p1 p2 false p2)))

	(it "finds a cat's game"
			(executing-moves-should-yield-response
				[0 8 3 2 7] [4 5 6 1]
				(make-move-response [0 8 3 2 7] [4 5 6 1] 7 p2 p1 false nil)))

	(it "distinguishes a tie from a win"
			(executing-moves-should-yield-response
				[0 1 5 6 2] [3 4 7 8]
				(make-move-response [0 1 5 6 2] [3 4 7 8] 2 p2 p1 false p1))
			(should-not (tie? (:board @game))))
	)
