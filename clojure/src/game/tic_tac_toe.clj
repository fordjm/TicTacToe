(ns game.tic-tac-toe
	(:require [game.cli :as cli]))

(defn exit-with-error [msg]
	(let [exit (fn [] (System/exit 1))]
		(println msg)
		(exit)))

(defn setup [interpreted-args]
	"TODO:  Remove setup-request, make game here, run game"
	(let [{:keys [msg options]} interpreted-args]
		(if msg
			(exit-with-error msg)
			(cli/run (cli/setup-request (:type options) (:first options) (:second options))))))

(defn initialize [interpret args]
	(setup (interpret args)))
