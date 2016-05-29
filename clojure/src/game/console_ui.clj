(ns game.console-ui
	(:require [game.ui :refer :all]
						[game.core :as core]
						[clojure.string :as string]))

;Depending directly on the core means I have no boundaries (maybe.)
(defn render-rows [rows]
	"Can I use reduce with string/join to fix duplication? (not so far)"
	(for [idx (range (count rows))]
		(str " " (string/join " " (string/join "|" (nth rows idx))))))

(defn render-board [board]
	"Clean me up - duplicated wrap-str call and (rows) definition"
	(let [wrap-str (fn [inner outer] (str outer inner outer))]
		(wrap-str
			(string/join
				(wrap-str "===+===+===" "\n")
				(render-rows (partition 3 board)))
			"\n")))

(defn render-prompt [model]
	(let [space (:space model)]
		(if space
			(str (:p2 model) " chose " space)
			"\nEnter[0-8]:")))

(defn render-result [winner]
	(if winner
		(str winner " wins!")
		"It's a tie!"))

(defn render-status [model]
	(if (:ongoing model)
		(render-prompt model)
		(render-result (:winner model))))

(defn boolean? [value]
	(or (false? value) (true? value)))

(defn model-valid? [model]
	"Why should UI know/care about model details if model has correct parts?"
	(let [board (:board model)]
		(and (map? model) (coll? board) (= size (count board))
				 (boolean? (:ongoing model)) (contains? model :winner))))

(defn move-view [model]
	(if (model-valid? model)
		(println (str (render-board (:board model))
									(render-status model)))
		nil))

(defn handle-end [request]
	(System/exit 0))

(defn handle-move [request]
	(core/execute-move (core/make-move (:space request))))

(defn handle-start [request]
	core/new-game)

(def request-handlers
	{"/end" {:controller handle-end}
	 "/move" {:controller handle-move :view move-view}
	 "/start" {:controller handle-start :view move-view}})
(def ui-instance (ui request-handlers))

(defn try-parse-int [value]
	(try
		(Integer. value)
		(catch NumberFormatException e nil)))

(defn run-game []
	"Not so sure about this location"
	(loop [request {:path "/start"}]
		(ui-instance request)
		(if (core/game-over?)
			(recur {:path "/end"})
			(recur {:path "/move" :space (try-parse-int (read-line))}))))