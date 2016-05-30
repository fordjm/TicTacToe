(ns game.ui-spec
	(:require [speclj.core :refer :all]
						[game.ui :refer :all]
						[game.console-ui :refer :all]
						[game.core :as core]))

(def empty-board (vec (range size)))

(defn rendering-move-outputs-result [move result]
	(should= result (with-out-str (render move-view move))))

(defn does-not-render-invalid-model [move]
	(rendering-move-outputs-result move ""))

(def no-moves-model {:board empty-board :ongoing true :winner nil})
(def empty-board-string (str "\n"
													 " 0 | 1 | 2"
													 "\n===+===+===\n"
													 " 3 | 4 | 5"
													 "\n===+===+===\n"
													 " 6 | 7 | 8"
													 "\n"))
(def prompt-str "\nEnter[0-8]:")

(defn player-takes-space [plr space]
	(assoc empty-board space plr))

(def cats-game ['X 'O 'X
								'X 'O 'O
								'O 'X 'X])
(def x-wins ['X 1 'O
						 'X 'O 5
						 'X 'O 'X])
(def o-wins ['X 'O 'X
						 3 'O 5
						 6 'O 'X])

(defn printed-line [line]
	(str line "\r\n"))

(defn make-move-stub [game space]
	(fn [] {:board (repeat size space) :ongoing true :winner nil}))

(defn execute-move-stub [move]
	(move))

(defn rendering-move-shows-board-and-status [move board status]
	(rendering-move-outputs-result
		move
		(printed-line (str board status))))

(def test-move-request {:path "/move" :space "4"})

(defn run-with-move-stubs [fct]
	(with-redefs [core/make-move make-move-stub
								core/execute-move execute-move-stub]
		(fct)))

(describe "game.ui"
	(it "does not plagiarize"
			(should (give-credit)))

	(it "does not render an invalid model"
			(does-not-render-invalid-model nil)
			(does-not-render-invalid-model 'X)
			(does-not-render-invalid-model {})
			(should= "" (with-out-str (render move-view {:board []})))
			(does-not-render-invalid-model {:board empty-board})
			(does-not-render-invalid-model {:board empty-board :ongoing nil})
			(should= "" (with-out-str (render move-view {:board empty-board :ongoing false}))))

	(it "renders no moves"
			(rendering-move-shows-board-and-status
				no-moves-model
				empty-board-string
				prompt-str))

	(it "renders x moving to 4"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space 'X 4) :p2 'X :space 4 :ongoing true :winner nil}
				(render-board (player-takes-space 'X 4))
				"X chose 4"))

	(it "renders X moving to 0"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space 'X 0) :p2 'X :space 0 :ongoing true :winner nil}
				(render-board (player-takes-space 'X 0))
				"X chose 0"))

	(it "renders O moving to 0"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space 'O 0) :p2 'O :space 0 :ongoing true :winner nil}
				(render-board (player-takes-space 'O 0))
				"O chose 0"))

	(it "renders a tie"
			(rendering-move-shows-board-and-status
				{:board cats-game :ongoing false :winner nil}
				(render-board cats-game)
				(render-result nil)))

	(it "renders a win for X"
			(rendering-move-shows-board-and-status
				{:board x-wins :ongoing false :winner 'X}
				(render-board x-wins)
				(render-result 'X)))

	(it "renders a win for O"
			(rendering-move-shows-board-and-status
				{:board o-wins :ongoing false :winner 'O}
				(render-board o-wins)
				(render-result 'O)))

	(it "throws an error when passed an invalid view"
			(should-throw Error "View is null"
										(render nil no-moves-model))
			(should-throw Error "Invalid view"
										(render "Alaska" no-moves-model))
			(should-throw Error "Invalid view"
										(render [] no-moves-model)))

	(it "filters non-integer space input"
			(should= "" (with-out-str (ui-instance {:path "/move" :space "w"}))))

	(it "tests ui-instance with start request"
			(should= (printed-line (str empty-board-string prompt-str))
							 (with-out-str (ui-instance {:path "/start"}))))

	(it "tests ui-instance with move request"
			(should= (printed-line (str (render-board (repeat size 4)) prompt-str))
							 (with-out-str
								 (run-with-move-stubs
									 (fn [] (ui-instance test-move-request))))))

	(it "tests ui-instance with end request - exits the tests"
			;(should= 0 (ui-instance {:path "/end"}))
			)
	)