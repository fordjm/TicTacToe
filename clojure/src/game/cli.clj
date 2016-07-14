(ns game.cli
  (:require [game.game :as game]
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

(defn game-view [gm]
	(if (not (empty? gm))
		(do
			(println (str (render-board (:board gm))
										(render-status gm))))
		(println "Cannot move to selected space.")))

(defn try-parse-int [value]
  (try
    (Integer/parseInt value)
    (catch NumberFormatException e nil)))

(defn handle-manual-move [game]
	(game/execute-move (game/make-move game (try-parse-int (read-line)))))

(defn handle-automatic-move [game]
	(game/execute-move (game/make-move game)))

(defn move-handler [game]
	(let [p1 (:p1 @game)
				type (:type p1)]
		(cond
			(= :manual type) (handle-manual-move game)
			(= :automatic type) (handle-automatic-move game))))
