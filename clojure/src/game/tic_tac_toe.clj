(ns game.tic-tac-toe
  (:require [game.maker :refer [setup-game]]))

(def game-atom (atom {}))

(defn maybe-update-game-atom [game]
  (if (not (empty? game))
    (swap! game-atom (fn [oldval] game))))

(defn exit [status]
  (System/exit status))

(defn game-loop [game view move-handler]
  (loop [game game
         view view]
    (do
      (maybe-update-game-atom game)
      (view game)
      (if (:ongoing @game-atom)
        (recur (move-handler game-atom) view)
        (exit 0)))))

(defn exit-with-error [msg]
  (let [exit-fn (fn [] (exit 1))]
    (println msg)
    (exit-fn)))

(defn run [view move-handler interpreted-args]
  (let [{:keys [msg options]} interpreted-args]
    (if msg
      (exit-with-error msg)
      (game-loop (setup-game {:type (:type options)
                                    :t1 (:first options)
                                    :t2 (:second options)})
                 view
                 move-handler))))

(defn initialize [view move-handler interpret args]
  (run view move-handler (interpret args)))
