(ns game.cli
	(:require [game.ui :refer :all]
						[game.core :as core]
						[game.board :as board]
						[clojure.string :as string]
						[clojure.tools.cli :refer [parse-opts]]))

(defn prompt-str [type]
	(cond
		(= :manual type) (str "Enter[0-" (dec board/size) "]:")
		(= :automatic type) "Game Start"
		:else nil))

(defn render-rows [rows]
	(for [idx (range (count rows))]
		(str " " (string/join " " (string/join "|" (nth rows idx))))))

(defn wrap-str [inner outer]
	(str outer inner outer))

(defn render-divider []
	(wrap-str
		(apply str (interpose "+" (repeat board/row-size "===")))
		"\n"))

(defn render-board [board]
	(wrap-str
		(string/join
			(render-divider)
			(render-rows (board/rows board)))
		"\n"))

(defn render-prompt [model]
	(let [space (:space model)]
		(if space
			(str (:token (:p2 model)) " chose " space)
			(prompt-str (:type (:p1 model))))))

(defn render-result [winner]
	(if winner
		(str winner " wins!")
		"It's a tie!"))

(defn render-status [model]
	(if (:ongoing model)
		(render-prompt model)
		(render-result (:winner model))))

(def game (atom {}))

(defn exit-view [[msg fct]]
	(println msg)
	(fct))

(defn move-view [gm]
	(if (core/game-valid? gm)
		(do
			(swap! game (fn [oldval] gm))
			(println (str (render-board (:board gm))
										(render-status gm))))
		nil))

(defn handle-exit [request]
	[(:msg request)
	 (fn [] (System/exit (:status request)))])

(defn handle-automatic-move [request]
	(core/execute-move (core/make-move game)))

(defn handle-manual-move [request]
	(core/execute-move (core/make-move game (:space request))))

(defn handle-setup [request]
	(core/setup-game request))

(def request-handlers
	{"/exit" {:controller handle-exit :view exit-view}
	 "/automatic-move" {:controller handle-automatic-move :view move-view}
	 "/manual-move" {:controller handle-manual-move :view move-view}
	 "/setup" {:controller handle-setup :view move-view}})

(def ui-instance (ui request-handlers))

(defn try-parse-int [value]
	(try
		(Integer/parseInt value)
		(catch NumberFormatException e nil)))

(defn move-request [type]
	(cond
		(= :automatic type) {:path "/automatic-move"}
		(= :manual type) {:path "/manual-move" :space (try-parse-int (read-line))}
		:else nil))

(defn exit-request [msg status]
	{:path "/exit" :msg msg :status status})

(defn run [request]
	(loop [request request]
		(ui-instance request)
		(if (:ongoing @game)
			(recur (move-request (:type (:p1 @game))))
			(recur (exit-request "Goodbye!" 0)))))

(def parse-token #(symbol %))
(def validate-token [#(and (not (nil? (re-find #"[\S&&[^0-9]]" (str %))))
													 (= 1 (count (str %))))
										 "Must be a non-numerical character"])

(defn strip-whitespace [str]
	"Combined ideas from markhneedham.com/blog/2013/09/22/clojure-stripping-all-the-whitespace"
	(string/join "" (remove string/blank? (string/split str #"\s"))))

(def cli-options
	[["-t" "--type TYPE" "Game type"
		:default 0
		:parse-fn #(Integer/parseInt %)
		:validate [#(< -1 % 4) "Must be a number between 0 and 3"]]
	 ["-f" "--first TOKEN" "First player token"
		:default 'X
		:parse-fn parse-token
		:validate validate-token]
	 ["-s" "--second TOKEN" "Second player token"
		:default 'O
		:parse-fn parse-token
		:validate validate-token]
	 ["-h" "--help"]])

(defn parse-args
	([] (parse-args []))
	([args] (let [clean-args (for [arg args]
														 (strip-whitespace arg))]
						(parse-opts clean-args cli-options))))

(defn exit-from-setup-request [msg]
	(exit-request msg 1))

(defn usage [options-summary]
	(->> ["Clojure Tic-Tac-Toe"
				""
				"Usage: lein run [options]"
				""
				"Options:"
				options-summary
				""
				"Types:"
				"  0    Human vs Computer"
				"  1    Computer vs Human"
				"  2    Human vs Human"
				"  3    Computer vs Computer"
				""
				"Enter lein run -- -h for help."]
			 (string/join \newline)))

(defn error-msg [errors]
	(str "The following errors occurred while parsing your command:\n\n"
			 (string/join \newline errors)))

(defn setup-request [type t1 t2]
	{:path "/setup" :type type :t1 t1 :t2 t2})

(defn setup-game [args]
	"Arg-parsing from tools.cli example"
	(let [{:keys [options arguments errors summary]} (parse-args args)
				request (cond
									(:help options) (exit-from-setup-request (usage summary))
									(not (empty? arguments)) (exit-from-setup-request (usage summary))
									errors (exit-from-setup-request (error-msg errors))
									:else (setup-request (:type options) (:first options) (:second options)))]
		(run request)))
