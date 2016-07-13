(ns game.main
  (:require [game.tic-tac-toe :as ttt]
            [game.cli-parser :as parser]))

(defn -main [& args]
  (ttt/initialize parser/interpret args))
