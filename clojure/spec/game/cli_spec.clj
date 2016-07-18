(ns game.cli-spec
  (:require [speclj.core :refer :all]
            [game.cli :refer :all]
            [game.maker :refer [setup-game]]
            [game.board :refer [empty-board center]]
            [game.board-evaluator :refer [available game-over?]]))

(defn printed-line [line]
  (str line "\r\n"))

(def new-board empty-board)
(def new-game-t2 (setup-game {:type 2 :t1 'X :t2 'O}))
(def new-game-t3 (setup-game {:type 3 :t1 'O :t2 'X}))

(defn token-takes-space [token space]
  (assoc new-board space token))

(defn make-cats-game
  ([] (make-cats-game new-game-t3))
  ([game] (if (empty? (available (:board game)))
            game
            (make-cats-game (move-handler (atom game))))))

(defn make-p1-win
  ([] (make-p1-win new-game-t2))
  ([game] (let [brd (:board game)
                avail (available brd)
                move (fn [pos] (if (= 0 pos)
                                 (first avail)
                                 (last avail)))]
            (if (game-over? brd)
              game
              (make-p1-win
                (with-in-str
                  (str (move (:position (:p1 game))))
                  (move-handler (atom game))))))))

(describe "game.cli"
  (it "responds to invalid input"
      (should= (printed-line "Cannot move to selected space.")
               (with-out-str (game-view {}))))

  (it "views a new human vs computer game"
      (should= (printed-line (str (render-board new-board)
                                  (prompt-str :manual)))
               (with-out-str (game-view new-game-t2))))

  (it "views a new computer vs human game"
      (should= (printed-line (str (render-board new-board)
                                  (prompt-str :automatic)))
               (with-out-str (game-view new-game-t3))))

  (it "views a valid human move"
      (let [choice "4"
            token (:token (:p1 new-game-t2))
            result (with-in-str choice (move-handler (atom new-game-t2)))]
        (should= (printed-line (str (render-board
                                      (token-takes-space token
                                                         (try-parse-int choice)))
                                    (render-prompt result)))
                 (with-out-str (game-view result)))))

  (it "views a valid computer move"
      (let [token (:token (:p1 new-game-t3))
            choice (first center)
            result (move-handler (atom new-game-t3))]
        (should= (printed-line (str (render-board (token-takes-space token choice))
                                    (render-prompt result)))
                 (with-out-str (game-view result)))))

  (it "views a tie game"
      (let [tie (make-cats-game)]
        (should= (printed-line (str (render-board (:board tie))
                                    (render-result nil)))
                 (with-out-str (game-view tie)))))

  (it "views a win"
      (let [win (make-p1-win)]
        (should= (printed-line (str (render-board (:board win))
                                    (render-result (:token (:p2 win)))))
                 (with-out-str (game-view win)))))
  )
