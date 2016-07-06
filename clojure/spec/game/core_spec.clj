(ns game.core-spec
	(:require [speclj.core :refer :all]
						[game.core :refer :all]
						[game.board :refer :all]
						[game.game-maker :as maker]
						[game.coach :as coach]))

(def game (atom (maker/setup-game {:type 0 :t1 'X :t2 'O})))

(defn p1-takes-4 [p1 p2]
	{:board (assoc empty-board 4 (:token p1)) :space 4 :p1 p2 :p2 p1 :ongoing true :winner nil})

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

(defn make-board-state [p1-moves p2-moves t1 t2]
	(let [place-tokens (fn [board spaces token]
											 (vec (reduce (fn [board idx] (assoc board idx token))
																		board spaces)))]
		(place-tokens
			(place-tokens empty-board p1-moves t1) p2-moves t2)))

(defn state-maker [t1 t2]
	(fn [p1-moves p2-moves] (make-board-state p1-moves p2-moves t1 t2)))

(defn make-move-response [board space p1 p2 ongoing winner]
	{:board board :space space :p1 p1 :p2 p2 :ongoing ongoing :winner (:token winner)})

(describe "game.core"
	(before
		(reset game)
		(swap! moves (fn [oldval] []))
		(def p1 (:p1 @game))
		(def p2 (:p2 @game))
		(def state-mkr (state-maker (:token p1) (:token p2))))

	(it "does not move out-of-bounds"
			(should= {} (execute-move (make-move game -1)))
			(should= {} (execute-move (make-move game size))))

	(it "executes a legal move"
			(should= (p1-takes-4 p1 p2) (execute-move (make-move game 4))))

	(it "keeps a history"
			(let [executed (execute-moves (make-moves [4] [0]))]
				(should= (make-move-response (state-mkr [4] [0]) 0 p1 p2 true nil)
								 (second executed))

				(reset game)
				(should= executed
								 (for [move @moves] (move)))))

	(it "does not move to occupied space"
			(executing-moves-should-yield-response
				[4] [4]
				{}))

	(it "recognizes a top-row winner"
			(executing-moves-should-yield-response
				[0 1 2] [3 4]
				(make-move-response (state-mkr [0 1 2] [3 4]) 2 p2 p1 false p1)))

	(it "recognizes a second-row winner"
			(executing-moves-should-yield-response
				[0 2 6] [3 4 5]
				(make-move-response (state-mkr [0 2 6] [3 4 5]) 5 p1 p2 false p2)))

	(it "recognizes a third-row winner"
			(executing-moves-should-yield-response
				[6 7 8] [3 4]
				(make-move-response (state-mkr [6 7 8] [3 4]) 8 p2 p1 false p1)))

	(it "recognizes a first-column winner"
			(executing-moves-should-yield-response
				[2 4 8] [0 3 6]
				(make-move-response (state-mkr [2 4 8] [0 3 6]) 6 p1 p2 false p2)))

	(it "recognizes a second-column winner"
			(executing-moves-should-yield-response
				[1 4 7] [2 5]
				(make-move-response (state-mkr [1 4 7] [2 5]) 7 p2 p1 false p1)))

	(it "recognizes a third-column winner"
			(executing-moves-should-yield-response
				[0 4 6] [2 5 8]
				(make-move-response (state-mkr [0 4 6] [2 5 8]) 8 p1 p2 false p2)))

	(it "recognizes a first-diagonal winner"
			(executing-moves-should-yield-response
				[0 4 8] [2 6]
				(make-move-response (state-mkr [0 4 8] [2 6]) 8 p2 p1 false p1)))

	(it "recognizes a second-diagonal winner"
			(executing-moves-should-yield-response
				[0 1 8] [2 4 6]
				(make-move-response (state-mkr [0 1 8] [2 4 6]) 6 p1 p2 false p2)))

	(it "recognizes a cat's game"
			(executing-moves-should-yield-response
				[0 8 3 2 7] [4 5 6 1]
				(make-move-response (state-mkr [0 8 3 2 7] [4 5 6 1]) 7 p2 p1 false nil)))

	(it "distinguishes a tie from a win"
			(executing-moves-should-yield-response
				[0 1 5 6 2] [3 4 7 8]
				(make-move-response (state-mkr [0 1 5 6 2] [3 4 7 8]) 2 p2 p1 false p1))
			(should-not (tie? (:board @game))))

	(it "handles automatic moves - is this all?"
			(let [mini-gm (minify-game game)
						result (move game)]
				(should= (coach/choose-move mini-gm) (:space result))))
	)
