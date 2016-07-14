(ns game.main
  (:require [game.tic-tac-toe :as ttt]
						[game.cli :as cli]
            [game.cli-parser :as parser]))

(defn -main [& args]
  (ttt/initialize cli/game-view
									cli/move-handler
									parser/interpret
									args))
