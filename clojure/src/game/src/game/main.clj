(ns game.main
	(:require [game.ui :as ui]))

(defn -main []
	"How do we ever quit?"
	(ui/run-game))