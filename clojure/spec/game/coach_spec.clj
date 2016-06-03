(ns game.coach-spec
	(:require [speclj.core :refer :all]
						[game.coach :refer :all]
						[game.board :refer :all]))

(def new-gm (mini-game empty-board 'X 'O))

(def x-takes-four
	(assoc empty-board center 'X))

(def o-takes-zero
	(assoc x-takes-four 0 'O))

(def x-takes-eight
	(assoc o-takes-zero 8 'X))

(def o-takes-two
	(assoc x-takes-eight 2 'O))

(def x-takes-one
	(assoc o-takes-two 1 'X))

(def o-takes-seven
	(assoc x-takes-one 7 'O))

(def x-takes-six
	(assoc o-takes-seven 6 'X))

(def x-fork-available
	['X 1 2
	 3 'O 5
	 'O 7 'X])

(defn returns-center-for-new-board []
	(should= center (advise new-gm)))

(defn returns-empty-corner-when-center-occupied []
	(should (corners (advise (mini-game x-takes-four 'O 'X)))))

(defn advised-move-comes-from-set? [fct game]
	(let [result ((set (fct game))
								 (advise game))]
		(false? (nil? result))))

(defn advised-move-blocks-win? [game]
	(advised-move-comes-from-set? blocking-moves game))

(defn blocks-horizontal-winning-move []
	(should= true (advised-move-blocks-win? (mini-game o-takes-two 'X 'O))))

(defn blocks-vertical-winning-move []
	(should= true (advised-move-blocks-win? (mini-game x-takes-one 'O 'X))))

(defn returns-side-as-last-resort []
	(should (sides (advise (mini-game x-takes-six 'O 'X)))))

(defn creates-fork []
	(should= true (advised-move-comes-from-set? forking-moves (mini-game x-fork-available 'X 'O))))

(def x-created-fork
	(assoc x-fork-available 2 'X))

(defn advises-winning-move []
	(should= true (advised-move-comes-from-set? winning-moves (mini-game x-created-fork 'X 'O))))

(defn returns-center-when-only-corner-occupied []
	(let [x-takes-zero (assoc empty-board 0 'X)]
		(should= center (advise (mini-game x-takes-zero 'O 'X)))))

(describe "game.coach"
	(it "tests advise"
			(advises-winning-move)
			(blocks-horizontal-winning-move)
			(blocks-vertical-winning-move)
			;blocks-diagonal-winning-move
			(creates-fork)
			(advised-move-comes-from-set? offensive-fork-blocks (mini-game ['X 1 2 3 'O 5 6 7 'X] 'O 'X)) ;vulnerability - getting forked while playing
			(advised-move-comes-from-set? defensive-fork-blocks (mini-game x-takes-eight 'O 'X))
			(returns-center-for-new-board)
			(advised-move-comes-from-set? opposite-corners (mini-game ['X 'X 'O 3 'O 5 6 7 8] 'X 'O))
			(returns-empty-corner-when-center-occupied)
			(returns-side-as-last-resort)
			(returns-center-when-only-corner-occupied)
			;(should= nil (advise ['O 'X 2 3 'X 5 6 'O 8] 'X))
			(should (advised-move-comes-from-set? offensive-fork-blocks (mini-game ['O 'X 2 3 'X 5 6 'O 8] 'X 'O)))
			)

	(it "passes likely-redundant tests moved from core_spec"
			(should= 4 (advise new-gm))

			(should= 5 (advise (mini-game ['X 1 'X 'O 'O 5 6 7 'X] 'O 'X)))
			(should= 2 (advise (mini-game ['O 1 2 'X 'X 'O 'X 7 8]
																		'O 'X)))
			(should= 0 (advise (mini-game [0 'X 2 3 'O 'X 'X 7 'O]
																		'O 'X)))
			(should= 6 (advise (mini-game ['O 'O 'X 'O 'X 'X 6 'X 8]
																		'O 'X))))
	)
