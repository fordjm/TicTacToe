(ns game.main
  (:require [game.tic-tac-toe :refer [initialize]]
            [game.cli :refer [game-view move-handler]]
            [game.cli-parser :refer [interpret]]))

(defn -main [& args]
  (initialize game-view
                  move-handler
                  interpret
                  args))
