(ns game.board-spec
  (:require [speclj.core :refer :all]
            [game.board :refer :all]))

(describe "game.board"
  (it "computes board size"
      (let [line-sizes (vec (range 2 9))
            sizes [4 9 16 25 36 49 64]]
        (doall
          (for [idx (range 7)]
            (should= (nth sizes idx) (compute-size (nth line-sizes idx)))))))

  (it "computes the center"
    (should= (sorted-set 0 1 2 3) (compute-center 2))
    (should= (sorted-set 4) (compute-center 3))
    (should= (sorted-set 5 6 9 10) (compute-center 4))
    (should= (sorted-set 12) (compute-center 5))
    (should= (sorted-set 14 15 20 21) (compute-center 6))
    (should= (sorted-set 24) (compute-center 7))
    (should= (sorted-set 27 28 35 36) (compute-center 8)))

  (it "computes corners"
    (should= (sorted-set 0 1 2 3) (compute-corners 2))
    (should= (sorted-set 0 2 6 8) (compute-corners 3))
    (should= (sorted-set 0 3 12 15) (compute-corners 4)))

  (it "computes opposite corners"
      (doall
        (for [line-sz (range 2 8)]
          (let [corners (compute-corners line-sz)
                opposites (compute-opposites corners)]
            (should= (reverse corners)
                     (for [corner corners] (get opposites corner)))))))

  (it "computes sides"
      (should= #{} (compute-sides 2))
      (should= (sorted-set 1 3 5 7) (compute-sides 3))
      (should= (sorted-set 1 2 4 7 8 11 13 14) (compute-sides 4)))
  )
