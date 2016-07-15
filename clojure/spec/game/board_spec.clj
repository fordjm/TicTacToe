(ns game.board-spec
  (:require [speclj.core :refer :all]
            [game.board :refer :all]
            [game.test-util :as util]))

(defn game-should-not-be-over [state]
  (should-not (or (game-over? state) (win? state) (tie? state))))

(defn game-should-have-winner [token state]
  (should= token (winner state)))

(defn game-should-be-over-with-ending [ending state]
  (should (and (game-over? state) (ending state))))

(defn game-should-not-have-ending [ending state]
  (should-not (ending state)))

(defn recognizes-winner-by-section [sections token]
  (doall
    (for [section sections]
      (let [state (util/assoc-all empty-board section token)]
        (game-should-be-over-with-ending win? state)
        (game-should-not-have-ending tie? state)
        (game-should-have-winner token state)))))

(describe "game.board"
  (it "computes the center"
    (should= (sorted-set 0 1 2 3) (compute-center 2))
    (should= (sorted-set 4) (compute-center 3))
    (should= (sorted-set 5 6 9 10) (compute-center 4))
    (should= (sorted-set 12) (compute-center 5))
    (should= (sorted-set 14 15 20 21) (compute-center 6))
    (should= (sorted-set 24) (compute-center 7))
    (should= (sorted-set 27 28 35 36) (compute-center 8)))

  (it "recognizes an unfinished game"
    (let [state empty-board]
      (game-should-not-be-over state)
      (game-should-have-winner nil state)))
  (it "recognizes a row winner"
    (recognizes-winner-by-section (rows empty-board) 'X))
  (it "recognizes a column winner"
    (recognizes-winner-by-section (cols empty-board) 'O))
  (it "recognizes a diagonal winner"
    (recognizes-winner-by-section (diags empty-board) 'X))
  (it "recognizes a tie"
    (let [state (map str empty-board)]
      (game-should-be-over-with-ending tie? state)
      (game-should-not-have-ending win? state)
      (game-should-have-winner nil state))))
