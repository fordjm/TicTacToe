(ns game.cli-spec
  (:require [speclj.core :refer :all]
            [game.cli :refer :all]
            [game.maker :as maker]
            [game.board :as board]
            [game.test-util :as util]))

(defn rendering-game-outputs-result [move result]
  (should= result (with-out-str (game-view move))))

(defn no-moves-value [p1 p2]
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

(defn rendering-game-shows-board-and-status [move board status]
  (rendering-game-outputs-result
    move
    (printed-line (str board status))))

(def setup-req {:path "/setup" :type 0 :t1 'X :t2 'O})

(describe "game.cli"
  (before
    (def gm (maker/setup-game setup-req))
    (def p1 (:p1 gm))
    (def p2 (:p2 gm)))

  (it "renders no moves"
      (rendering-game-shows-board-and-status
        (no-moves-value p1 p2)
        (render-board board/empty-board)
        (prompt-str (:type p1))))

  (it "renders X moving to 4"
      (rendering-game-shows-board-and-status
        {:board (player-takes-space p1 4) :p1 p2 :p2 p1 :space 4 :ongoing true :winner nil}
        (render-board (player-takes-space p1 4))
        "X chose 4"))

  (it "renders X moving to 0"
      (rendering-game-shows-board-and-status
        {:board (player-takes-space p1 0) :p1 p2 :p2 p1 :space 0 :ongoing true :winner nil}
        (render-board (player-takes-space p1 0))
        "X chose 0"))

  (it "renders O moving to 0"
      (rendering-game-shows-board-and-status
        {:board (player-takes-space p2 0) :p1 p1 :p2 p2 :space 0 :ongoing true :winner nil}
        (render-board (player-takes-space p2 0))
        "O chose 0"))

  (it "renders a tie"
      (rendering-game-shows-board-and-status
        {:board (cats-game p1 p2) :p1 p2 :p2 p1 :ongoing false :winner nil}
        (render-board (cats-game p1 p2))
        (render-result nil)))

  (it "renders a win for X"
      (rendering-game-shows-board-and-status
        {:board (p1-wins p1 p2) :p1 p2 :p2 p1 :ongoing false :winner p1}
        (render-board (p1-wins p1 p2))
        (render-result p1)))

  (it "renders a win for O"
      (rendering-game-shows-board-and-status
        {:board (p2-wins p1 p2) :p1 p2 :p2 p1 :ongoing false :winner p2}
        (render-board (p2-wins p1 p2))
        (render-result p2)))

  (it "filters non-integer space input"
			(should= (printed-line "Cannot move to selected space.")
							 (with-out-str
								 (with-in-str "w"
															(game-view (move-handler (atom gm)))))))

  (it "tests game-view with new game"
      (should= (printed-line (str (render-board board/empty-board) (prompt-str :manual)))
               (with-out-str (game-view gm))))

  (xit "tests game-view with valid move"
			 (should= (printed-line (str (render-board (assoc board/empty-board 4 (:token p1))) (prompt-str :automatic)))
								(with-out-str
									(game-view (move-handler (atom gm))))))
  )
