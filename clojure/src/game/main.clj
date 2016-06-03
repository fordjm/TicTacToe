(ns game.main
	(:require [game.cli :as ui]))

(defn -main [& args]
	(ui/setup-game args))