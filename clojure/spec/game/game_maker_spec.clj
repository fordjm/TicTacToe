(ns game.game-maker-spec
	(:require [speclj.core :refer :all]
						[game.game-maker :refer :all]))

(defn setup-request-with-type [type]
	{:type type :t1 'X :t2 'O})

(defn player-should-have-type [plr type]
	(should= type (:type plr)))

(defn both-players-should-have-type [players type]
	(doall
		(map (fn [plr] (player-should-have-type plr type)) players)))

(it "sets up a game with one human and one computer player"
		(let [[p1 p2] (extract-players (setup-game (setup-request-with-type 0)))]
			(player-should-have-type p1 :manual)
			(player-should-have-type p2 :automatic)))

(it "sets up a game with one human and one computer player"
		(let [[p1 p2] (extract-players (setup-game (setup-request-with-type 1)))]
			(player-should-have-type p1 :automatic)
			(player-should-have-type p2 :manual)))

(it "sets up a game with two human players"
		(both-players-should-have-type
			(extract-players (setup-game (setup-request-with-type 2)))
			:manual))

(it "sets up a game with two computer players"
		(both-players-should-have-type
			(extract-players (setup-game (setup-request-with-type 3)))
			:automatic))

(it "throws an exception on invalid type"
		(should-throw IllegalStateException
									(setup-game (setup-request-with-type 4))))

(it "resets a game to the correct type after 0 moves"
		(doall
			(for [type [0 1 2 3]]
				(let [gm (atom (setup-game (setup-request-with-type type)))]
					(should= type (game-type (reset gm)))))))

(it "resets a game to the correct type after 1 move"
		(doall
			(for [type [0 1 2 3]]
				(let [gm (atom (setup-game (setup-request-with-type type)))]
					(move gm 0)
					(should= type (game-type (reset gm)))))))

(it "sets player tokens to P and Q"
		(let [gm (setup-game {:type 0 :t1 'P :t2 'Q})]
			(should= 'P (:token (:p1 gm)))
			(should= 'Q (:token (:p2 gm)))))

(it "does not set duplicate tokens"
		(should-throw Error "Duplicate tokens: P & P" (setup-game {:type 0 :t1 'P :t2 'P})))

(it "should know an empty map is an invalid game"
		(should= false (game-valid? {})))
