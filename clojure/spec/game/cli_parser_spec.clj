(ns game.cli-parser-spec
	(:require [speclj.core :refer :all]
						[game.cli-parser :refer :all]))

(def summ (str "  -t, --type TYPE     0  Game type\n"
							 "  -f, --first TOKEN   X  First player token\n"
							 "  -s, --second TOKEN  O  Second player token\n"
							 "  -h, --help"))

(def default-args {:options {:type 0 :first 'X :second 'O} :arguments [] :summary summ :errors nil})

(it "parses no arguments"
		(should= default-args (parse-args)))

(it "parses non-option arguments as args"
		(should= (assoc default-args :arguments ["foo"])
						 (parse-args ["foo"])))

(it "parses the game type"
		(should= (update-in default-args [:options] #(assoc % :type 0))
						 (parse-args ["-t 0"])))

(it "parses the game type with extra spaces"
		(should= (update-in default-args [:options] #(assoc % :type 3))
						 (parse-args [" -t   3"])))

(it "screens invalid types"
		(should= (assoc default-args
							 :errors ["Failed to validate \"-t 4\": Must be a number between 0 and 3"])
						 (parse-args ["-t 4"])))

(it "handles missing option parameters"
		(should= (assoc default-args
							 :errors ["Missing required argument for \"-t TYPE\""])
						 (parse-args ["-t"])))

(it "screens invalid type params"
		(should= (assoc default-args
							 :errors ["Error while parsing option \"-t w\": java.lang.NumberFormatException: For input string: \"w\""])
						 (parse-args ["-t w"])))

(it "screens invalid options"
		(should= (assoc default-args :errors ["Unknown option: \"-w\""])
						 (parse-args ["-w"])))

(it "parses help"
		(should= (update-in default-args [:options] #(assoc % :help true))
						 (parse-args ["-h"])))

(it "parses first-player token"
		(should= (update-in default-args [:options] #(assoc % :first 'P))
						 (parse-args ["-f P"])))

(it "parses second-player token"
		(should= (update-in default-args [:options] #(assoc % :second 'Q))
						 (parse-args ["-s Q"])))

(it "screens invalid tokens"
		(should= (assoc default-args
							 :errors ["Failed to validate \"-f 4\": Must be a non-numerical character"])
						 (parse-args ["-f 4"]))
		(should= (assoc default-args
							 :errors ["Failed to validate \"-s 8\": Must be a non-numerical character"])
						 (parse-args ["-s 8"]))
		(should= (assoc default-args
							 :errors ["Failed to validate \"-f XO\": Must be a non-numerical character"])
						 (parse-args ["-f XO"])))
