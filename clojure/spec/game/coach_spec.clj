(ns game.coach-spec
  (:require [speclj.core :refer :all]
            [game.coach :refer :all]
            [game.board :refer :all]
            [game.test-util :as util]))

(def new-gm (coachs-game empty-board 'X 'O))
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

(describe "game.coach"
  (it "handles nulls" (should= nil (choose-move nil)))

  ; START PRIORITY-ORDER-SPECIFIC TESTS
  (it "takes center when board is empty"
      (let [gm new-gm
            move (choose-move gm)]
        (should (contains? center move))
        (move-does-not-create-threat move gm)))

  (it "takes a corner when center is taken"
      (let [board (util/assoc-all empty-board center 'X)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
        (should (contains? corners move))
        (move-does-not-create-threat move gm)))

  (it "takes an opposite corner"
      (let [ohs (assoc empty-board (first corners) 'O)
            board (util/assoc-all ohs center 'X)
            gm (coachs-game board 'X 'O)
            move (choose-move gm)]
        (should (contains? (opposite-corners gm) move))
        (move-does-not-create-threat move gm)))

  (it "takes an empty side"
      (let [exes (util/assoc-all empty-board (cons (first sides) (concat center (drop 2 corners))) 'X)
            board (util/assoc-all exes (cons (last sides) (take 2 corners)) 'O)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
        (should (contains? sides move))
        (move-does-not-create-threat move gm)))
  ; END PRIORITY-ORDER-SPECIFIC TESTS

  (it "makes a winning move"
      (let [ohs (util/assoc-all empty-board (butlast (second (rows empty-board))) 'O)
            board (util/assoc-all ohs (butlast (first (rows ohs))) 'X)
            gm (coachs-game board 'X 'O)
            move (choose-move gm)]
        (should= (:p1 gm) (winner (assoc board move 'X)))))

  (it "blocks the opponent from winning"
      (doall
        (for [section (sections empty-board)]
          (let [board (util/assoc-all empty-board (butlast section) 'O)
                gm (coachs-game board 'X 'O)
                move (choose-move gm)]
            (blocks-opponent-win move gm)))))

  (it "creates a fork for X"
      (let [ohs (util/assoc-all empty-board (take to-take (rest (second (cols empty-board)))) 'O)
            board (util/assoc-all ohs (concat (take to-take (first (cols ohs)))
                                              (drop 2 (last (rows ohs)))) 'X)
            gm (coachs-game board 'X 'O)
            move (choose-move gm)]
        (move-creates-fork move gm)))

  (it "creates a fork for O"
      (let [exes (util/assoc-all empty-board (butlast (second (diags empty-board))) 'X)
            board (util/assoc-all exes (concat (take to-take (first (cols exes)))
                                               (drop 2 (last (rows exes)))) 'O)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
        (move-creates-fork move gm)))

  (it "prevents a fork by forcing a block elsewhere A"
      (let [ohs (util/assoc-all empty-board (take to-take (rest (nth (cols empty-board) second-last))) 'O)
            board (util/assoc-all ohs (concat (take to-take (first (rows ohs))) (drop 2 (last (cols ohs)))) 'X)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
        (move-creates-threat move gm)
        (blocking-threat-prevents-fork move gm)))

  (it "prevents a fork by forcing a block elsewhere B"
      (let [ohs (util/assoc-all empty-board (take to-take (first (rows empty-board))) 'O)
            board (util/assoc-all ohs
                                  (concat (take to-take (last (rows ohs)))
                                          (take to-take (rest (nth (cols ohs) second-last)))) 'X)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
        (move-creates-threat move gm)
        (blocking-threat-prevents-fork move gm)))

  (it "prevents a fork by moving to intersection space"
			(let [exes (util/assoc-all empty-board (concat (take to-take (rest (first (cols empty-board))))
                                                     (drop 2 (first (rows empty-board)))) 'X)
            board (util/assoc-all exes (take to-take (rest (last (cols exes)))) 'O)
            gm (coachs-game board 'O 'X)
            move (choose-move gm)]
				(should (create-fork (coachs-game board (:p2 gm) (:p1 gm))))
				(should-not (create-fork (coachs-game (assoc board move (:p1 gm)) (:p2 gm) (:p1 gm))))))

  (it "tests opposite-corners"
			(should= #{} (opposite-corners new-gm))
			(doall
				(for [[p1 p2] [['X 'O] ['O 'X]]]
					(should= (for [opposite (reverse (sort corners))] #{opposite})
									 (for [corner (sort corners)]
										 (opposite-corners (coachs-game (assoc empty-board corner p1) p2 p1))))))))
