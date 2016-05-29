(ns game.main
	(:require [game.console-ui :as ui]))

(defn -main []
	(ui/run-game))