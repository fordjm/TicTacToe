(ns game.test-util)

(defn assoc-all [vect kys value]
	"Credit:  http://stackoverflow.com/questions/22730726/idiomatic-way-to-assoc-multiple-elements-in-vector"
	(reduce #(assoc %1 %2 value) vect kys))
