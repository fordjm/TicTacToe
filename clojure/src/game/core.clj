(ns game.core
	(:require [game.board :refer :all]
						[game.coach :as coach]))

(def moves (atom []))
(def new-game {:board empty-board :ongoing true :winner nil})
(def not-nil? (complement nil?))

(defn make-player [token type position]
	{:token token :type type :position position})

(defn player-types [game-type]
	(cond
		(= game-type 0) [:manual :automatic]
		(= game-type 1) [:automatic :manual]
		(= game-type 2) [:manual :manual]
		(= game-type 3) [:automatic :automatic]
		:else [nil nil]))

(defn make-players [request]
	(if (= (:t1 request) (:t2 request))
		(throw (Error. (str "Duplicate tokens: " (:t1 request) " & " (:t2 request)))))
	(let [[tp1 tp2] (player-types (:type request))]
		{:p1 (make-player (:t1 request) tp1 0)
		 :p2 (make-player (:t2 request) tp2 1)}))

(defn boolean? [value]
	(or (false? value) (true? value)))

(defn player-valid? [player]
	(every? not-nil? (map player [:token :type :position])))

(defn game-valid? [game]
	(let [board (:board game)]
		(and (map? game) (coll? board) (= size (count board))
				 (boolean? (:ongoing game)) (every? player-valid? (map game [:p1 :p2]))
				 (contains? game :winner))))

(defn make-game [request]
	(let [game (atom (merge new-game (make-players request)))]
		(set-validator! game
										(fn [newval] (game-valid? newval)))
		game))

(defn setup-game[request]
	@(make-game request))

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

(defn heterogeneous-players? [players]
	(reduce not= (map :type players)))

(defn homogeneous-players? [type players]
	(every? (fn [player] (= type (:type player))) players))

(defn extract-players [game]
	[(:p1 game) (:p2 game)])

(defn manual? [player]
	(= :manual (:type player)))

(defn automatic? [player]
	(= :automatic (:type player)))

(defn first-player [players]
	(some (fn [player] (if (= 0 (:position player)) player)) players))

(defn game-type [game]
	(let [players (extract-players game)
				p1 (first-player players)]
		(cond
			(and (heterogeneous-players? players) (manual? p1)) 0
			(and (heterogeneous-players? players) (automatic? p1)) 1
			(homogeneous-players? :manual players) 2
			(homogeneous-players? :automatic players) 3
			:else nil)))

(defn extract-request [game]
	(let [type (game-type game)
				ordered-players (sort-by :position (extract-players game))
				[t1 t2] (map :token ordered-players)]
		{:type type :t1 t1 :t2 t2}))

(defn reset [game]
	(let [request (extract-request @game)]
		(swap! game (fn [oldval] (setup-game request)))))

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
