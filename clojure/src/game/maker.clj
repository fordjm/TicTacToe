(ns game.maker
  (:require [game.board :as board]))

(def new-game {:board board/empty-board :ongoing true :winner nil})
(def not-nil? (complement nil?))
(def creation-params (atom {}))

(defn make-player [token type position]
  {:token token :type type :position position})

(defn player-types [game-type]
  (cond
    (= game-type 0) [:manual :automatic]
    (= game-type 1) [:automatic :manual]
    (= game-type 2) [:manual :manual]
    (= game-type 3) [:automatic :automatic]
    :else [nil nil]))

(defn make-players [params]
  (if (= (:t1 params) (:t2 params))
    (throw (Error. (str "Duplicate tokens: " (:t1 params) " & " (:t2 params)))))
  (let [[tp1 tp2] (player-types (:type params))]
    {:p1 (make-player (:t1 params) tp1 0)
     :p2 (make-player (:t2 params) tp2 1)}))

(defn boolean? [value]
  (or (false? value) (true? value)))

(defn player-valid? [player]
  (every? not-nil? (map player [:token :type :position])))

(defn game-valid? [game]
  (let [board (:board game)]
    (and (map? game) (coll? board) (= board/size (count board))
         (boolean? (:ongoing game)) (every? player-valid? (map game [:p1 :p2]))
         (contains? game :winner))))

(defn make-game [params]
  (let [game (atom (merge new-game (make-players params)))]
    (swap! creation-params (fn [oldval] params))
    (set-validator! game
                    (fn [newval] (game-valid? newval)))
    game))

(defn setup-game[params]
  @(make-game params))

(defn heterogeneous-players? [players]
  (reduce not= (map :type players)))

(defn homogeneous-players? [type players]
  (every? (fn [player] (= type (:type player))) players))

(defn extract-players [game]
  [(:p1 game) (:p2 game)])

(defn manual? [player]
  (= :manual (:type player)))

(defn automatic? [player]
  (= :automatic (:type player)))

(defn first-player [players]
  (some (fn [player] (if (= 0 (:position player)) player)) players))

(defn game-type [game]
  (let [players (extract-players game)
        p1 (first-player players)]
    (cond
      (and (heterogeneous-players? players) (manual? p1)) 0
      (and (heterogeneous-players? players) (automatic? p1)) 1
      (homogeneous-players? :manual players) 2
      (homogeneous-players? :automatic players) 3
      :else nil)))

(defn reset [game]
  (swap! game (fn [oldval] (setup-game @creation-params))))
