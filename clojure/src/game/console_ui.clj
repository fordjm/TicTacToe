(ns game.console-ui
	(:require [game.ui :refer :all]
						[game.core :as core]
						[clojure.string :as string]))

;Depending directly on the core means I have no boundaries (unless values are the boundaries.)
(def size core/size)

(defn render-rows [rows]
	(for [idx (range (count rows))]
		(str " " (string/join " " (string/join "|" (nth rows idx))))))

(defn render-board [board]
	"Clean me up - duplicated wrap-str call and (rows) definition"
	(let [wrap-str (fn [inner outer] (str outer inner outer))]
		(wrap-str
			(string/join
				(wrap-str "===+===+===" "\n")
				(render-rows (core/rows board)))
			"\n")))

(defn render-prompt [model]
	(let [space (:space model)]
		(if space
			(str (:token (:p2 model)) " chose " space)
			"\nEnter[0-8]:")))

(defn render-result [winner]
	(if winner
		(str winner " wins!")
		"It's a tie!"))

(defn render-status [model]
	(if (:ongoing model)
		(render-prompt model)
		(render-result (:winner model))))

(def game (atom {}))

(defn move-view [gm]
	(if (core/game-valid? gm)
		(do
			(swap! game (fn [oldval] gm))
			(println (str (render-board (:board gm))
										(render-status gm))))
		nil))

(defn handle-end [request]
	(System/exit 0))

(defn handle-move [request]
	"Why not pass the whole request?"
	(core/execute-move (core/make-move game (:space request))))

(defn handle-setup [request]
	(core/setup-game))

(def request-handlers
	{"/end" {:controller handle-end}
	 "/move" {:controller handle-move :view move-view}
	 "/setup" {:controller handle-setup :view move-view}})

(def ui-instance (ui request-handlers))

(defn try-parse-int [value]
	(try
		(Integer. value)
		(catch NumberFormatException e nil)))

(defn run-game []
	"Not so sure about this location"
	(loop [request {:path "/setup"}]
		(ui-instance request)
		(if (:ongoing @game)
			(recur {:path "/move" :space (try-parse-int (read-line))})
			(recur {:path "/end"}))))
