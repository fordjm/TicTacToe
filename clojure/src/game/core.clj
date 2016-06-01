(ns game.core)

(def size 9)
(def moves (atom []))
(def new-game {:board (vec (range size)) :ongoing true :winner nil})

(defn new-player [token type]
	{:token token :type type})

(defn player-types [game-type]
	(cond
		(= game-type 0) [:manual :automatic]
		(= game-type 1) [:manual :manual]
		(= game-type 2) [:automatic :automatic]
		:else [nil nil]))

(defn new-players [type]
	(let [[t1 t2] (player-types type)]
		{:p1 (new-player 'X t1) :p2 (new-player 'O t2)}))

(defn boolean? [value]
	(or (false? value) (true? value)))

(defn contains-keys? [map keys]
	(every? (fn [key] (contains? map key)) keys))

(defn game-valid? [game]
	"TODO:  Test player validity"
	(let [board (:board game)]
		(and (map? game) (coll? board) (= size (count board))
				 (boolean? (:ongoing game)) (contains-keys? game [:p1 :p2 :winner]))))

(defn make-game [type]
	(let [game (atom (merge new-game (new-players type)))]
		(set-validator! game (fn [newval]
													 (game-valid? newval)))
		game))

(defn setup-game
	([] (setup-game 1))
	([type] @(make-game type)))

(defn rows [board]
	(partition 3 board))

(defn cols [board]
	(apply map vector (rows board)))

(defn diags [board]
	"Doesn't read well"
	(for [outer [(rows board) (reverse (cols board))]]
		(for [inner (range (count outer))]
			(nth (nth outer inner) inner))))

(defn sections [board]
	(concat (rows board) (cols board) (diags board)))

(defn win? [board]
	(some (fn [coll] (apply = coll))
				(sections board)))

(defn tie? [board]
	(and (empty? (filter integer? board))
			 (not (win? board))))

(defn game-over? [board]
	(or (win? board) (tie? board)))

(defn winner [board]
	(if (win? board)
		(apply first (filter (fn [coll] (= 1 (count (distinct coll))))
												 (sections board)))))

(defn move [game space]
	(if (contains? (set (:board @game)) space)
		(let [p1 (:p1 @game)
					board (assoc (:board @game) space (:token p1))]
			(swap! game assoc
						 :board board
						 :p1 (:p2 @game)
						 :p2 p1
						 :space space
						 :ongoing (not (game-over? board))
						 :winner (winner board)))
		{}))

(defn heterogeneous-players? [players]
	(reduce not= (map :type players)))

(defn homogeneous-players? [type players]
	(every? (fn [player] (= type (:type player))) players))

(defn extract-players [gm]
	[(:p1 gm) (:p2 gm)])

(defn game-type [gm]
	(let [players (extract-players gm)]
		(cond
			(heterogeneous-players? players) 0
			(homogeneous-players? :manual players) 1
			(homogeneous-players? :automatic players) 2
			:else nil)))

(defn reset [game]
	"Not liking this"
	(let [type (game-type @game)
				gm (if type
						 (setup-game type)
						 (setup-game))]
		(swap! game (fn [oldval] gm))))

(defn make-move [game space]
	(fn [] (move game space)))

(defn add-move-to-history [move newval]
	(if (not (empty? newval))
		(swap! moves conj move)))

(defn execute-move [move]
	(let [newval (move)]
		(add-move-to-history move newval)
		newval))
