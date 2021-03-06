(ns game.coach-spec
  (:require [speclj.core :refer :all]
            [game.coach :refer :all]
            [game.board :refer :all]
            [game.board-evaluator :refer :all]
            [game.test-util :refer [assoc-all]]))

(def new-gm (game-pieces empty-board 'X 'O))
(def to-take (- line-size 2))
(def second-last (- line-size 2))

(defn move-does-not-create-threat [move game]
  (let [board (:board game)
        p1 (:p1 game)]
    (should-not (has-threat? board p1))
    (should-not (has-threat? (assoc board move p1) p1))))

(defn move-creates-threat [move game]
  (let [before (:board game)
        p1 (:p1 game)
        after (assoc before move p1)]
    (should-not (has-threat? before p1))
    (should (has-threat? after p1))))

(defn blocks-opponent-win [move game]
  (let [before (:board game)
        p2 (:p2 game)
        after (assoc before move (:p1 game))]
    (should (has-threat? before p2))
    (should-not (has-threat? after p2))))

(defn blocking-threat-prevents-fork [move game]
  (let [before (assoc (:board game) move (:p1 game))
        p2 (:p2 game)
        threat (first (threats before (:p1 game)))
        after (assoc before threat (:p2 game))]
    (has-threat? before (:p1 game))
    (should-not (has-fork? after p2))))

(defn move-creates-fork [move game]
  (let [before (:board game)
        p1 (:p1 game)
        after (assoc before move p1)]
    (should-not (has-fork? before p1))
    (should (has-fork? after p1))))

(defn had-been-vulnerable-to-fork [game]
  (should (game-pieces (:board game) (:p2 game) (:p1 game))))

(defn not-vulnerable-to-fork-after-move [gm move]
  (let [newboard (assoc (:board gm) move (:p1 gm))
        after (game-pieces newboard (:p2 gm) (:p1 gm))]
    (should-not (create-fork after))))

(describe "game.coach"
  (it "handles nulls" (should= nil (choose-move nil)))

  ; START PRIORITY-ORDER-SPECIFIC TESTS
  (it "takes center when board is empty"
      (let [gm new-gm
            move (choose-move gm)]
        (should (contains? center move))
        (move-does-not-create-threat move gm)))

  (it "takes a corner when center is taken"
      (let [board (assoc-all empty-board center 'X)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (should (contains? corners move))
        (move-does-not-create-threat move gm)))

  (it "takes an opposite corner"
      (let [ohs (assoc empty-board (first corners) 'O)
            board (assoc-all ohs center 'X)
            gm (game-pieces board 'X 'O)
            move (choose-move gm)]
        (should (contains? (opposite-corners gm) move))
        (move-does-not-create-threat move gm)))

  (it "takes an empty side"
      (let [exes (assoc-all empty-board (cons (first sides) (concat center (drop 2 corners))) 'X)
            board (assoc-all exes (cons (last sides) (take 2 corners)) 'O)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (should (contains? sides move))
        (move-does-not-create-threat move gm)))
  ; END PRIORITY-ORDER-SPECIFIC TESTS

  (it "makes a winning move"
      (let [ohs (assoc-all empty-board (butlast (second (rows empty-board))) 'O)
            board (assoc-all ohs (butlast (first (rows ohs))) 'X)
            gm (game-pieces board 'X 'O)
            move (choose-move gm)]
        (should= (:p1 gm) (winner (assoc board move 'X)))))

  (it "blocks the opponent from winning"
      (doall
        (for [section (sections empty-board)]
          (let [board (assoc-all empty-board (butlast section) 'O)
                gm (game-pieces board 'X 'O)
                move (choose-move gm)]
            (blocks-opponent-win move gm)))))

  (it "creates a fork for X"
      (let [ohs (assoc-all empty-board (take to-take (rest (second (cols empty-board)))) 'O)
            board (assoc-all ohs (concat (take to-take (first (cols ohs)))
                                              (drop 2 (last (rows ohs)))) 'X)
            gm (game-pieces board 'X 'O)
            move (choose-move gm)]
        (move-creates-fork move gm)))

  (it "creates a fork for O"
      (let [exes (assoc-all empty-board (butlast (second (diags empty-board))) 'X)
            board (assoc-all exes (concat (take to-take (first (cols exes)))
                                               (drop 2 (last (rows exes)))) 'O)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (move-creates-fork move gm)))

  (it "prevents a fork by forcing a block elsewhere A"
      (let [ohs (assoc-all empty-board (take to-take (rest (nth (cols empty-board) second-last))) 'O)
            board (assoc-all ohs (concat (take to-take (first (rows ohs))) (drop 2 (last (cols ohs)))) 'X)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (move-creates-threat move gm)
        (blocking-threat-prevents-fork move gm)))

  (it "prevents a fork by forcing a block elsewhere B"
      (let [ohs (assoc-all empty-board (take to-take (first (rows empty-board))) 'O)
            board (assoc-all ohs
                                  (concat (take to-take (last (rows ohs)))
                                          (take to-take (rest (nth (cols ohs) second-last)))) 'X)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (move-creates-threat move gm)
        (blocking-threat-prevents-fork move gm)))

  (it "prevents a fork by moving to intersection space"
      (let [exes (assoc-all empty-board (concat (take to-take (rest (first (cols empty-board))))
                                                     (drop 2 (first (rows empty-board)))) 'X)
            board (assoc-all exes (take to-take (rest (last (cols exes)))) 'O)
            gm (game-pieces board 'O 'X)
            move (choose-move gm)]
        (had-been-vulnerable-to-fork gm)
        (not-vulnerable-to-fork-after-move gm move))))
