(ns game.cli-spec
  (:require [speclj.core :refer :all]
            [game.ui :refer :all]
            [game.cli :refer :all]
            [game.core :as core]
            [game.game-maker :as maker]
            [game.board :as board]
            [game.test-util :as util]))

(defn rendering-move-outputs-result [move result]
  (should= result (with-out-str (render move-view move))))

(defn no-moves-model [p1 p2]
  {:board board/empty-board :p1 p1 :p2 p2 :ongoing true :winner nil})

(defn player-takes-space [plr space]
  (assoc board/empty-board space (:token plr)))

(defn cats-game [p1 p2]
  (repeat board/size 'XO))

(defn p1-wins [p1 p2]
  (util/assoc-all board/empty-board (first (board/cols board/empty-board)) p1))

(defn p2-wins [p1 p2]
  (util/assoc-all board/empty-board (second (board/cols board/empty-board)) p2))

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

(describe "game.ui"
  (before
    (def gm (maker/setup-game setup-req))
    (def p1 (:p1 gm))
    (def p2 (:p2 gm)))

  (it "does not plagiarize"
      (should (give-credit)))

  (it "renders no moves"
      (rendering-move-shows-board-and-status
        (no-moves-model p1 p2)
        (render-board board/empty-board)
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
      (should= (printed-line "Cannot move to selected space.")
               (with-out-str (ui-instance {:path "/manual-move" :space "w"}))))

  (it "tests ui-instance with setup request"
      (should= (printed-line (str (render-board board/empty-board) (prompt-str :manual)))
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