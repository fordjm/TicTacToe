(ns game.cli
  (:require [game.ui :refer :all]
            [game.game-maker :as maker]
            [game.turn :as turn]
            [game.board :as board]
            [clojure.string :as string]))

(def highest (dec board/size))

(defn prompt-str [type]
  (cond
    (= :manual type) (str "Enter[0-" highest "]:")
    (= :automatic type) "Game Start"
    :else nil))

(defn wrap-str [inner outer]
  (str outer inner outer))

(defn render-three-char-space [contents]
  "Max supported board size = 31x31 (much larger than available CLI space)"
  (let [length (count (str contents))]
    (cond
      (= 2 length) (str " " contents)
      (= 1 length) (wrap-str contents " ")
      :else
      contents)))

(defn render-row [row]
  (string/join "|" (map render-three-char-space row)))

(defn render-rows [rows]
  (for [row (range (count rows))]
    (render-row (nth rows row))))

(defn render-divider []
  (wrap-str
    (apply str (interpose "+" (repeat board/line-size "===")))
    "\n"))

(defn render-board [board]
  (wrap-str
    (string/join
      (render-divider)
      (render-rows (board/rows board)))
    "\n"))

(defn render-prompt [value]
  (let [space (:space value)]
    (if space
      (str (:token (:p2 value)) " chose " space)
      (prompt-str (:type (:p1 value))))))

(defn render-result [winner]
  (if winner
    (str winner " wins!")
    "It's a tie!"))

(defn render-status [value]
  (if (:ongoing value)
    (render-prompt value)
    (render-result (:winner value))))

(def game (atom {}))

(defn exit-view [[msg fct]]
  (println msg)
  (fct))

(defn game-view [gm]
  (if (not (empty? gm))
    (do
      (swap! game (fn [oldval] gm))
      (println (str (render-board (:board gm))
                    (render-status gm))))
    (println "Cannot move to selected space.")))

(defn handle-exit [request]
  [(:msg request)
   (fn [] (System/exit (:status request)))])

(defn handle-automatic-move [request]
  (turn/execute-move (turn/make-move game)))

(defn handle-manual-move [request]
  (turn/execute-move (turn/make-move game (:space request))))

(defn handle-setup [request]
  (maker/setup-game request))

(def request-handlers
  {"/exit" {:controller handle-exit :view exit-view}
   "/automatic-move" {:controller handle-automatic-move :view game-view}
   "/manual-move" {:controller handle-manual-move :view game-view}
   "/setup" {:controller handle-setup :view game-view}})

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

(defn setup-request [type t1 t2]
  {:path "/setup" :type type :t1 t1 :t2 t2})
