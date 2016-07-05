(ns game.cli-parser
	(:require [clojure.tools.cli :refer [parse-opts]]
						[clojure.string :as string]))

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
