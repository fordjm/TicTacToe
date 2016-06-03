(ns game.core
	(:require [game.board :refer :all]
						[game.coach :as coach]))

(def moves (atom []))
(def new-game {:board empty-board :ongoing true :winner nil})

(defn new-player [token type]
	{:token token :type type})

(defn player-types [game-type]
	(cond
		(= game-type 0) [:manual :automatic]
		(= game-type 1) [:automatic :manual]
		(= game-type 2) [:manual :manual]
		(= game-type 3) [:automatic :automatic]
		:else [nil nil]))

(defn make-players [type]
	(let [[t1 t2] (player-types type)]
		{:p1 (new-player 'X t1) :p2 (new-player 'O t2)}))

(defn boolean? [value]
	(or (false? value) (true? value)))

(defn contains-all-keys? [map keys]
	(every? (fn [key] (contains? map key)) keys))

(defn game-valid? [game]
	"TODO:  Test player validity"
	(let [board (:board game)]
		(and (map? game) (coll? board) (= size (count board))
				 (boolean? (:ongoing game)) (contains-all-keys? game [:p1 :p2 :winner]))))

(defn make-game [type]
	(let [game (atom (merge new-game (make-players type)))]
		(set-validator! game (fn [newval]
													 (game-valid? newval)))
		game))

(defn setup-game[type]
	@(make-game type))

(defn minify-game [game]
	(let [gameval @game
				p1val (:p1 gameval)
				p2val (:p2 gameval)]
		{:board (:board gameval) :p1 (:token p1val) :p2 (:token p2val)}))

(defn move
	([game] (move game (coach/advise (minify-game game))))
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

(defn heterogeneous-players? [players]
	(reduce not= (map :type players)))

(defn homogeneous-players? [type players]
	(every? (fn [player] (= type (:type player))) players))

(defn extract-players [game]
	[(:p1 game) (:p2 game)])

(defn game-type [game]
	"TODO:  Distinguish type 0 from type 1"
	(let [players (extract-players game)]
		(cond
			(heterogeneous-players? players) 0
			(homogeneous-players? :manual players) 2
			(homogeneous-players? :automatic players) 3
			:else nil)))

(defn reset [game]
	(let [type (game-type @game)]
		(swap! game (fn [oldval] (setup-game type)))))

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
