(ns game.ui)

(defn give-credit []
	(str "Adapted from Clojure TinyWeb (2.4) in "
			 "Functional Programming Patterns "
			 "by Michael Bevilacqua-Linn"))

(defn render [view value]
	(try (view value)
			 (catch NullPointerException _
				 (throw (Error. "View is null")))
			 (catch ClassCastException _
				 (throw (Error. "Invalid view")))
			 (catch IllegalArgumentException _
				 (throw (Error. "Invalid view")))))

(defn execute-request [request handler]
	(let [controller (handler :controller)
				view (handler :view)]
		(try (render view (controller request))
				 (catch NullPointerException e (throw e)))))

(defn ui [request-handlers]
	(fn [request]
		(let [path (request :path)
					handler (request-handlers path)]
			(execute-request request handler))))
