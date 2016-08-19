(ns game.game
  (:require [game.board-evaluator :refer [game-over? winner]]
            [game.coach :refer [game-pieces choose-move]]))

(def moves (atom []))

(defn minify-game [game]
  (let [gameval @game
        p1val (:p1 gameval)
        p2val (:p2 gameval)]
    (game-pieces (:board gameval) (:token p1val) (:token p2val))))

(defn move
  ([game] (move game (choose-move (minify-game game))))
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
