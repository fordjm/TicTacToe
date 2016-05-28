(ns game.ui
	(:require [clojure.string :as string]
						[game.core :as core]))

;Depending directly on the core means I have no boundaries.

(def size core/size)

(defn give-credit []
	(str "Adapted from Clojure TinyWeb (2.4) in "
			 "Functional Programming Patterns "
			 "by Michael Bevilacqua-Linn"))

(defn render [view model]
	(try (view model)
			 (catch NullPointerException _
				 (throw (Error. "View is null")))
			 (catch ClassCastException _
				 (throw (Error. "View is not a function")))
			 (catch IllegalArgumentException _
				 (throw (Error. "View is not a function")))))

(defn apply-filters [filters request]
	"TODO:  Remove me"
	(let [lone-filter (first filters)]
		(lone-filter request)))

(defn execute-request [request handler]
	(let [controller (handler :controller)
				view (handler :view)]
		(try (render view (controller request))
				 (catch NullPointerException e (throw e)))))

(defn ui [request-handlers filters]
	(fn [request]
		(let [filtered-request (apply-filters filters request)
					path (request :path)
					handler (request-handlers path)]
			(execute-request filtered-request handler))))

;===Move below to own namespace to allow different view implementations
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

(defn int-parsing-filter [request]
	"Don't think a filter should do this"
	(let [space (:space request)]
		(if space
			(try
				(assoc request :space (Integer. (:space request)))
				(catch NumberFormatException e nil))
			request)))

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
(def filters [int-parsing-filter])
(def ui-instance (ui request-handlers filters))

(defn try-parse-int [value]
	(try
		(Integer. value)
		(catch NumberFormatException e nil)))

(defn run-game []
	(loop [request {:path "/start"}]
		(ui-instance request)
		(if (core/game-over?)
			(recur {:path "/end"})
			(recur {:path "/move" :space (try-parse-int (read-line))})))
	)
