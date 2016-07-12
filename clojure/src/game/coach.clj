(ns game.coach
  (:require [game.board :refer :all]))

(defn make-game [board p1 p2]
  {:board board :p1 p1 :p2 p2})

(defn threats [board token]
  (filter (fn [space] (win? (assoc board space token)))
          (available board)))

(defn has-threat? [board token]
  (not (empty? (threats board token))))

(defn win-game [game]
  (some identity (threats (:board game) (:p1 game))))

(defn block-win [game]
  (some identity (threats (:board game) (:p2 game))))

(defn has-fork? [board token]
  (< 1 (count (filter (fn [section]
                        (and (= (dec line-size) (count (filter (fn [space] (= token space)) section)))
                             (= 1 (count (filter (fn [space] (integer? space)) section)))))
                    (sections board)))))

(defn create-fork [game]
  (let [board (:board game)
        p1 (:p1 game)]
    (some identity (filter (fn [space] (has-fork? (assoc board space p1) p1))
                           (available board)))))

(defn swap-players [game]
  (make-game (:board game) (:p2 game) (:p1 game)))

(defn update-game [game space]
  (let [newboard (assoc (:board game) space (:p1 game))]
    (assoc (swap-players game) :board newboard)))

(defn create-threats [game]
  (let [board (:board game)
        p1 (:p1 game)]
    (filter (fn [space] (has-threat? (assoc board space p1) p1))
            (available board))))

(defn block-fork [game]
  (let [fork (create-fork (swap-players game))]
    (if fork
      (let [threat (some #(and (not= fork (block-win (update-game game %))) %)
                         (create-threats game))]
        (if threat
          threat
          fork)))))

(defn opposite-corners [game]
  (let [to-oppose (filter (fn [corner] (= (:p2 game) (nth (:board game) corner))) corners)]
    (set (for [corner to-oppose]
           (get opposites corner)))))

(defn best-by-position [game]
  (some identity (filter (fn [space] (selectable? (:board game) space))
                         (concat center (opposite-corners game) corners sides))))

(defn choose-move [game]
  "Chooses best available move in Newell and Simon priority order"
  (some identity
        (list (win-game game)
              (block-win game)
              (create-fork game)
              (block-fork game)
              (best-by-position game))))
