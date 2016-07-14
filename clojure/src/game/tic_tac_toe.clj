(ns game.tic-tac-toe
	(:require [game.cli :as cli]
						[game.game-maker :as maker]))

(def game-atom (atom {}))

(defn maybe-update-game-atom [game]
	(if (not (empty? game))
		(swap! game-atom (fn [oldval] game))))

(defn exit [status]
	(System/exit status))

(defn game-loop [game view]
	(loop [game game
				 view view]
		(do
			(maybe-update-game-atom game)
			(view game)
			(if (:ongoing @game-atom)
				(recur (cli/handle-move game-atom) view)
				(exit 0)))))

(defn exit-with-error [msg]
	(let [exit-fn (fn [] (exit 1))]
		(println msg)
		(exit-fn)))

(defn run [interpreted-args]
	(let [{:keys [msg options]} interpreted-args]
		(if msg
			(exit-with-error msg)
			(game-loop (maker/setup-game {:type (:type options) :t1 (:first options) :t2 (:second options)})
								 cli/game-view))))

(defn initialize [interpret args]
	(run (interpret args)))
