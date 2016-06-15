(ns game.coach-spec
	(:require [speclj.core :refer :all]
						[game.coach :refer :all]
						[game.board :refer :all]))

(def new-gm (make-game empty-board 'X 'O))
(def x4-board (assoc empty-board 4 'X))
(def x4-o0-board (assoc x4-board 0 'O))
(def x4-o0-x1-board (assoc x4-o0-board 1 'X))
(def x4-o0-x2-board (assoc x4-o0-board 2 'X))
(def x2-o4-x1-board (assoc empty-board 2 'X 4 'O 1 'X))

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
				(should= center move)
				(move-does-not-create-threat move gm)))
	(it "takes a corner when center is taken"
			(let [gm (make-game x4-board 'O 'X)
						move (choose-move gm)]
				(should (contains? corners move))
				(move-does-not-create-threat move gm)))
	(it "takes an opposite corner"
			(let [board x4-o0-board
						gm (make-game board 'X 'O)
						move (choose-move gm)]
				(should (contains? (opposite-corners gm) move))
				(move-does-not-create-threat move gm)))
	(it "takes an empty side"
			(let [board (assoc x4-o0-board 6 'X 2 'O 1 'X 7 'O 8 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(should (contains? sides move))
				(move-does-not-create-threat move gm)))
	; END PRIORITY-ORDER-SPECIFIC TESTS

	(it "blocks a second column win"
			(let [gm (make-game x4-o0-x1-board 'O 'X)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-does-not-create-threat move gm)))
	(it "blocks a second row win"
			(let [board (assoc x4-o0-board 5 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "blocks a second diagonal win"
			(let [board x4-o0-x2-board
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "blocks a first column win"
			(let [gm (make-game (assoc x4-o0-x2-board 6 'O) 'X 'O)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "blocks a first row win"
			(let [gm (make-game x2-o4-x1-board 'O 'X)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "blocks a first diagonal win"
			(let [gm (make-game (assoc x2-o4-x1-board 0 'O) 'X 'O)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "blocks a third row win"
			(let [gm (make-game (assoc empty-board 8 'X 4 'O 7 'X) 'O 'X)
						move (choose-move gm)]
				(blocks-opponent-win move gm)
				(move-creates-threat move gm)))
	(it "makes a winning move"
			(let [board (assoc x2-o4-x1-board 5 'O)
						gm (make-game board 'X 'O)
						move (choose-move gm)]
				(should= (:p1 gm) (winner (assoc board move 'X)))))
	(it "creates a fork for X"
			(let [board (assoc empty-board 0 'X 1 'O 7 'X 5 'O)
						gm (make-game board 'X 'O)
						move (choose-move gm)]
				(move-creates-fork move gm)))
	(it "creates a fork for O"
			(let [board (assoc empty-board 0 'O 4 'X 8 'O 2 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(move-creates-fork move gm)))
	(it "prevents a fork by forcing a block elsewhere A"
			(let [board (assoc empty-board 0 'X 4 'O 8 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(move-creates-threat move gm)
				(blocking-threat-prevents-fork move gm)))
	(it "prevents a fork by forcing a block elsewhere B"
			(let [board (assoc x4-o0-board 8 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(move-creates-threat move gm)
				(blocking-threat-prevents-fork move gm)))
	(it "prevents a fork by moving to intersection space"
			(let [board (assoc empty-board 2 'X 5 'O 3 'X)
						gm (make-game board 'O 'X)
						move (choose-move gm)]
				(should-not (create-fork (make-game (assoc board move (:p1 gm)) (:p2 gm) (:p1 gm))))))

	(it "tests opposite-corners"
			(should= #{} (opposite-corners new-gm))
			(for [[p1 p2] [['X 'O] ['O 'X]]]
				(should= (for [opposite (reverse (sort corners))] #{opposite})
								 (for [corner (sort corners)]
									 (opposite-corners (make-game (assoc empty-board corner p1) p2 p1))))))
	)
