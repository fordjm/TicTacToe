(ns game.cli
	(:require [game.ui :refer :all]
						[game.core :as core]
						[game.board :as board]
						[clojure.string :as string]
						[clojure.tools.cli :refer [parse-opts]]))

;Depending directly on the core means I have no boundaries (unless values are the boundaries.)
(def size board/size)

(defn render-rows [rows]
	(for [idx (range (count rows))]
		(str " " (string/join " " (string/join "|" (nth rows idx))))))

(defn render-board [board]
	"Clean me up"
	(let [wrap-str (fn [inner outer] (str outer inner outer))]
		(wrap-str
			(string/join
				(wrap-str "===+===+===" "\n")
				(render-rows (board/rows board)))
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

(defn handle-automatic-move [request]
	"Why not pass the whole request?"
	(core/execute-move (core/make-move game)))

(defn handle-manual-move [request]
	"Why not pass the whole request?"
	(core/execute-move (core/make-move game (:space request))))

(defn handle-setup [request]
	(core/setup-game (:type request)))

(def request-handlers
	{"/end" {:controller handle-end}
	 "/automatic-move" {:controller handle-automatic-move :view move-view}
	 "/manual-move" {:controller handle-manual-move :view move-view}
	 "/setup" {:controller handle-setup :view move-view}})

(def ui-instance (ui request-handlers))

(defn try-parse-int [value]
	(try
		(Integer. value)
		(catch NumberFormatException e nil)))

(defn strip-whitespace [str]
	"Combined ideas from markhneedham.com/blog/2013/09/22/clojure-stripping-all-the-whitespace"
	(string/join "" (remove string/blank? (string/split str #"\s"))))

(def cli-options
	[["-t" "--type TYPE" "Game type"
		:default 0
		:parse-fn #(Integer/parseInt %)
		:validate [#(< 0 % 4) "Must be a number between 0 and 3"]]
	 ["-h" "--help"]])

(defn parse-args
	"TODO:  Check errors entry after returning"
	([] (parse-args []))
	([args] (let [clean-args (for [arg args]
														 (strip-whitespace arg))]
						(parse-opts clean-args cli-options))))

(defn move-request [type]
	(cond
		(= :automatic type) {:path "/automatic-move"}
		(= :manual type) {:path "/manual-move" :space (try-parse-int (read-line))}))

(defn run-game [args]
	"Not so sure about this location, arg-parsing from tools.cli example"
	(let [{:keys [options arguments errors summary]} (parse-args args)]
		(loop [request {:path "/setup" :type (:type options)}]
			(ui-instance request)
			(if (:ongoing @game)
				(recur (move-request (:type (:p1 @game))))          ;temporary hack
				(recur {:path "/end"})))
		)
	)
