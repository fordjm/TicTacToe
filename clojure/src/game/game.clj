(ns game.game
  (:require [game.board :as board]
            [game.maker :as maker]
            [game.coach :as coach]))

(def moves (atom []))

(defn minify-game [game]
  (let [gameval @game
        p1val (:p1 gameval)
        p2val (:p2 gameval)]
		(coach/game-pieces (:board gameval) (:token p1val) (:token p2val))))

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
                           :ongoing (not (board/game-over? board))
                           :winner (board/winner board)))
                  {})))

(defn reset [game]
  "TODO:  Move reset to maker and keep params atom there?"
  (let [request (maker/extract-request @game)]
    (swap! game (fn [oldval] (maker/setup-game request)))))

(defn make-move
  ([game] (fn [] (move game)))
  ([game space] (fn [] (move game space))))

(defn maybe-add-move-to-history [move newval]
  (if (not (empty? newval))
    (swap! moves conj move)))

(defn execute-move [move]
  (let [newval (move)]
    (maybe-add-move-to-history move newval)
    newval))
