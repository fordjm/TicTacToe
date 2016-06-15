(ns game.cli-spec
	(:require [speclj.core :refer :all]
						[game.ui :refer :all]
						[game.cli :refer :all]
						[game.core :as core]
						[game.board :as board]))

(defn rendering-move-outputs-result [move result]
	(should= result (with-out-str (render move-view move))))

(defn does-not-render-invalid-model [move]
	(rendering-move-outputs-result move ""))

(defn no-moves-model [p1 p2] {:board board/empty-board :p1 p1 :p2 p2 :ongoing true :winner nil})

(def empty-board-str (str "\n"
													" 0 | 1 | 2"
													"\n===+===+===\n"
													" 3 | 4 | 5"
													"\n===+===+===\n"
													" 6 | 7 | 8"
													"\n"))

(defn player-takes-space [plr space]
	(assoc board/empty-board space (:token plr)))

(defn cats-game [p1 p2] [p1 p2 p1
												 p1 p2 p2
												 p2 p1 p1])
(defn p1-wins [p1 p2] [p1 1 p2
											 p1 p2 5
											 p1 p2 p1])
(defn p2-wins [p1 p2] [p1 p2 p1
											 3 p2 5
											 6 p2 p1])

(defn printed-line [line]
	(str line "\r\n"))

(defn make-move-stub [game space]
	(let [next (:p2 @game)]
		(fn [] {:board (repeat board/size space) :p1 next :p2 (:p1 @game) :ongoing true :winner nil})))

(defn execute-move-stub [move]
	(move))

(defn rendering-move-shows-board-and-status [move board status]
	(rendering-move-outputs-result
		move
		(printed-line (str board status))))

(defn run-with-move-stubs [fct]
	(with-redefs [core/make-move make-move-stub
								core/execute-move execute-move-stub]
		(fct)))

(def setup-req {:path "/setup" :type 0 :t1 'X :t2 'O})

;START CLI-OPTIONS HELPER FUNCTIONS
(def summ (str "  -t, --type TYPE     0  Game type\n"
							 "  -f, --first TOKEN   X  First player token\n"
							 "  -s, --second TOKEN  O  Second player token\n"
							 "  -h, --help"))

(def default-args {:options {:type 0 :first 'X :second 'O} :arguments [] :summary summ :errors nil})
;END CLI-OPTIONS HELPER FUNCTIONS

(describe "game.ui"
	(before
		(def gm (core/setup-game setup-req))
		(def p1 (:p1 gm))
		(def p2 (:p2 gm)))

	(it "does not plagiarize"
			(should (give-credit)))

	; START PARSER TESTS
	(it "parses no arguments"
			(should= default-args (parse-args)))

	(it "parses non-option arguments as args"
			(should= (assoc default-args :arguments ["foo"])
							 (parse-args ["foo"])))

	(it "parses the game type"
			(should= (update-in default-args [:options] #(assoc % :type 0))
							 (parse-args ["-t 0"])))

	(it "parses the game type with extra spaces"
			(should= (update-in default-args [:options] #(assoc % :type 3))
							 (parse-args [" -t   3"])))

	(it "screens invalid types"
			(should= (assoc default-args
								 :errors ["Failed to validate \"-t 4\": Must be a number between 0 and 3"])
							 (parse-args ["-t 4"])))

	(it "handles missing option parameters"
			(should= (assoc default-args
								 :errors ["Missing required argument for \"-t TYPE\""])
							 (parse-args ["-t"])))

	(it "screens invalid type params"
			(should= (assoc default-args
								 :errors ["Error while parsing option \"-t w\": java.lang.NumberFormatException: For input string: \"w\""])
							 (parse-args ["-t w"])))

	(it "screens invalid options"
			(should= (assoc default-args :errors ["Unknown option: \"-w\""])
							 (parse-args ["-w"])))

	(it "parses help"
			(should= (update-in default-args [:options] #(assoc % :help true))
							 (parse-args ["-h"])))

	(it "parses first-player token"
			(should= (update-in default-args [:options] #(assoc % :first 'P))
							 (parse-args ["-f P"])))

	(it "parses second-player token"
			(should= (update-in default-args [:options] #(assoc % :second 'Q))
							 (parse-args ["-s Q"])))

	(it "screens invalid tokens"
			(should= (assoc default-args
								 :errors ["Failed to validate \"-f 4\": Must be a non-numerical character"])
							 (parse-args ["-f 4"]))
			(should= (assoc default-args
								 :errors ["Failed to validate \"-s 8\": Must be a non-numerical character"])
							 (parse-args ["-s 8"]))
			(should= (assoc default-args
								 :errors ["Failed to validate \"-f XO\": Must be a non-numerical character"])
							 (parse-args ["-f XO"])))
	; END PARSER TESTS

	(it "does not render an invalid model"
			(does-not-render-invalid-model nil)
			(does-not-render-invalid-model p1)
			(does-not-render-invalid-model {})
			(should= "" (with-out-str (render move-view {:board []})))
			(does-not-render-invalid-model {:board board/empty-board})
			(does-not-render-invalid-model {:board board/empty-board :ongoing nil}))

	(it "renders no moves"
			(rendering-move-shows-board-and-status
				(no-moves-model p1 p2)
				empty-board-str
				(prompt-str (:type p1))))

	(it "renders X moving to 4"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space p1 4) :p1 p2 :p2 p1 :space 4 :ongoing true :winner nil}
				(render-board (player-takes-space p1 4))
				"X chose 4"))

	(it "renders X moving to 0"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space p1 0) :p1 p2 :p2 p1 :space 0 :ongoing true :winner nil}
				(render-board (player-takes-space p1 0))
				"X chose 0"))

	(it "renders O moving to 0"
			(rendering-move-shows-board-and-status
				{:board (player-takes-space p2 0) :p1 p1 :p2 p2 :space 0 :ongoing true :winner nil}
				(render-board (player-takes-space p2 0))
				"O chose 0"))

	(it "renders a tie"
			(rendering-move-shows-board-and-status
				{:board (cats-game p1 p2) :p1 p2 :p2 p1 :ongoing false :winner nil}
				(render-board (cats-game p1 p2))
				(render-result nil)))

	(it "renders a win for X"
			(rendering-move-shows-board-and-status
				{:board (p1-wins p1 p2) :p1 p2 :p2 p1 :ongoing false :winner p1}
				(render-board (p1-wins p1 p2))
				(render-result p1)))

	(it "renders a win for O"
			(rendering-move-shows-board-and-status
				{:board (p2-wins p1 p2) :p1 p2 :p2 p1 :ongoing false :winner p2}
				(render-board (p2-wins p1 p2))
				(render-result p2)))

	(it "throws an error when passed an invalid view"
			(should-throw Error "View is null"
										(render nil no-moves-model))
			(should-throw Error "Invalid view"
										(render "Alaska" no-moves-model))
			(should-throw Error "Invalid view"
										(render [] no-moves-model)))

	(it "filters non-integer space input"
			(should= "" (with-out-str (ui-instance {:path "/manual-move" :space "w"}))))

	(it "tests ui-instance with setup request"
			(should= (printed-line (str empty-board-str (prompt-str :manual)))
							 (with-out-str (ui-instance setup-req))))

	(it "tests ui-instance with move request"
			(should= (printed-line (str (render-board (repeat board/size 4)) (prompt-str :automatic)))
							 (with-out-str
								 (run-with-move-stubs
									 (fn [] (ui-instance {:path "/manual-move" :space "4"}))))))

	(it "tests ui-instance with exit request - exits the tests"
			;(should= (usage summ) (with-out-str (ui-instance (exit-from-setup-request (usage summ)))))
			)

	(it "gets a human move request - not sure these should exist"
			(should= {:path "/manual-move" :space 0}
							 (with-in-str "0" (move-request :manual))))

	(it "gets a computer move request"
			(should= {:path "/automatic-move"} (move-request :automatic)))
	)