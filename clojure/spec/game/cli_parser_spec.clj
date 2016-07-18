(ns game.cli-parser-spec
  (:require [speclj.core :refer :all]
            [game.cli-parser :refer :all]))

(def summ (str "  -t, --type TYPE     0  Game type\n"
               "  -f, --first TOKEN   X  First player token\n"
               "  -s, --second TOKEN  O  Second player token\n"
               "  -h, --help"))

(def default-args {:options {:type 0 :first 'X :second 'O} :arguments [] :summary summ :errors nil})

(defn default-args-with-type [type]
  (update-in default-args [:options] #(assoc % :type type)))

(defn default-args-with-error [error]
  (assoc default-args :errors [error]))

(def default-args-with-help
  (update-in default-args [:options] #(assoc % :help true)))

(def default-options (:options default-args))

(defn errors-msg [msg]
  (str errors-follow msg))

(defn invalid [opt reason]
  (str "Failed to validate \"" opt "\": " reason))

(defn should-have-msg-and-options [interpreted msg options]
  (should= msg (:msg interpreted))
  (should= options (:options interpreted)))

(describe "game.cli-parser"
  (it "parses no arguments"
      (should= default-args (parse-args)))

  (it "parses non-option arguments as args"
      (should= (assoc default-args :arguments ["foo"])
               (parse-args ["foo"])))

  (it "parses the game type"
      (should= (default-args-with-type 1)
               (parse-args ["-t 1"])))

  (it "parses the game type with extra spaces"
      (should= (default-args-with-type 3)
               (parse-args [" -t   3"])))

  (it "screens invalid types"
      (should= (default-args-with-error (invalid "-t 4" type-msg))
               (parse-args ["-t 4"])))

  (it "handles missing option parameters"
      (should= (default-args-with-error "Missing required argument for \"-t TYPE\"")
               (parse-args ["-t"])))

  (it "screens invalid type params"
      (should= (default-args-with-error
                 "Error while parsing option \"-t w\": java.lang.NumberFormatException: For input string: \"w\"")
               (parse-args ["-t w"])))

  (it "screens invalid options"
      (should= (default-args-with-error "Unknown option: \"-w\"")
               (parse-args ["-w"])))

  (it "parses help"
      (should= default-args-with-help (parse-args ["-h"])))

  (it "parses first-player token"
      (should= (update-in default-args [:options] #(assoc % :first 'P))
               (parse-args ["-f P"])))

  (it "parses second-player token"
      (should= (update-in default-args [:options] #(assoc % :second 'Q))
               (parse-args ["-s Q"])))

  (it "screens invalid tokens"
      (should= (default-args-with-error (invalid "-f 4" non-numeric))
               (parse-args ["-f 4"]))
      (should= (default-args-with-error (invalid "-s 8" non-numeric))
               (parse-args ["-s 8"]))
      (should= (default-args-with-error (invalid "-f XO" non-numeric))
               (parse-args ["-f XO"])))

  (it "interprets arguments"
      (should-have-msg-and-options (interpret ["-h"])
                                   (usage summ) (:options default-args-with-help))
      (should-have-msg-and-options (interpret ["foo"])
                                   (usage summ) default-options)
      (should-have-msg-and-options (interpret ["-t 4"])
                                   (errors-msg (invalid "-t 4" type-msg)) default-options)
      (should-have-msg-and-options (interpret ["-f 4"])
                                   (errors-msg (invalid "-f 4" non-numeric)) default-options)
      (should-have-msg-and-options (interpret ["-t 3"])
                                   nil (:options (default-args-with-type 3))))
  )
