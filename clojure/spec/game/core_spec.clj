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

	(it "does not move to an occupied space"
			(let [executed (execute-moves (make-moves [4] [4]))]
				(should= {} (second executed))))

	(it "handles automatic moves"
			(let [mini-gm (minify-game game)
						result (move game)]
				(should= (coach/choose-move mini-gm) (:space result))))


	(it "resets a game to the correct type after 0 moves"
			(doall
				(for [type [0 1 2 3]]
					(let [gm (atom (maker/setup-game {:type type :t1 'X :t2 'O}))]
						(should= type (maker/game-type (reset gm)))))))

	(it "resets a game to the correct type after 1 move"
			(doall
				(for [type [0 1 2 3]]
					(let [gm (atom (maker/setup-game {:type type :t1 'X :t2 'O}))]
						(move gm 0)
						(should= type (maker/game-type (reset gm))))))))
