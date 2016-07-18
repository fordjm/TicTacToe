(ns game.game-spec
  (:require [speclj.core :refer :all]
            [game.game :refer :all]
            [game.board :refer :all]
            [game.maker :as maker]
            [game.coach :as coach]
            [game.test-util :as util]))

(def gm-atom (atom (maker/setup-game {:type 0 :t1 'X :t2 'O})))

(defn p1-takes-4 [p1 p2]
  {:board (assoc empty-board 4 (:token p1)) :space 4 :p1 p2 :p2 p1 :ongoing true :winner nil})

(defn interleave-moves [p1-moves p2-moves]
  (if (= (count p1-moves) (count p2-moves))
    (interleave p1-moves p2-moves)
    (cons (first p1-moves) (interleave p2-moves (rest p1-moves)))))

(defn make-moves [p1-moves p2-moves]
  (map (fn [space] (make-move gm-atom space))
       (interleave-moves p1-moves p2-moves)))

(defn execute-moves [moves]
  (map execute-move moves))

(def assoc-all util/assoc-all)

(defn make-board-state [p1-moves p2-moves t1 t2]
  (let [p1-moved (assoc-all empty-board p1-moves t1)]
    (assoc-all p1-moved p2-moves t2)))

(defn state-maker [t1 t2]
  (fn [p1-moves p2-moves]
    (make-board-state p1-moves p2-moves t1 t2)))

(defn make-move-response [board space p1 p2 ongoing winner]
  {:board board :space space :p1 p1 :p2 p2 :ongoing ongoing :winner (:token winner)})

(describe "game.game"
  (before
    (maker/reset gm-atom)
    (swap! moves (fn [oldval] []))
    (def p1 (:p1 @gm-atom))
    (def p2 (:p2 @gm-atom))
    (def state-mkr (state-maker (:token p1) (:token p2))))

  (it "does not move out-of-bounds"
      (should= {} (execute-move (make-move gm-atom -1)))
      (should= {} (execute-move (make-move gm-atom size))))

  (it "executes manual moves"
      (should= (p1-takes-4 p1 p2) (execute-move (make-move gm-atom 4))))

  (it "keeps a history"
      (let [executed (execute-moves (make-moves [4] [0]))]
        (should= (make-move-response (state-mkr [4] [0]) 0 p1 p2 true nil)
                 (second executed))

        (maker/reset gm-atom)
        (should= executed
                 (for [move @moves] (move)))))

  (it "does not move to an occupied space"
      (let [executed (execute-moves (make-moves [4] [4]))]
        (should= {} (second executed))))

  (it "executes automatic moves"
      (let [mini-gm (minify-game gm-atom)
            result (move gm-atom)]
        (should= (coach/choose-move mini-gm) (:space result))))
  )
