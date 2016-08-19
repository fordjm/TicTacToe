(ns game.cli-parser
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join blank? split]]))

(def parse-token #(symbol %))

(def non-numeric "Must be a single non-numeric character")

(def validate-token [#(and (not (nil? (re-find #"[\S&&[^0-9]]" (str %))))
                           (= 1 (count (str %))))
                     non-numeric])

(defn strip-whitespace [str]
  "Combined ideas from markhneedham.com/blog/2013/09/22/clojure-stripping-all-the-whitespace"
  (join "" (remove blank? (split str #"\s"))))

(def type-msg "Must be a number between 0 and 3")

(def cli-options
  [["-t" "--type TYPE" "Game type"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< -1 % 4) type-msg]]
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
       (join \newline)))

(def errors-follow "The following errors occurred while parsing your command:\n\n")

(defn error-msg [errors]
  (str errors-follow (join \newline errors)))

(defn interpret-msg [parsed]
  (let [{:keys [options arguments errors summary]} parsed]
    (cond
      (:help options) (usage summary)
      (not (empty? arguments)) (usage summary)
      errors (error-msg errors))))

(defn interpret [args]
  (let [parsed (parse-args args)]
    {:msg (interpret-msg parsed) :options (:options parsed)}))
