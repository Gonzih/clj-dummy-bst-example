(ns clj-bst.core
  (:require [clojure.pprint :refer [pprint]]))

(defn bst [k v] {:key k :value v :left nil :right nil})

(defn insert [{:keys [key value left right] :as tree} k v]
  (let [node (bst k v)]
    (cond
    (< key k) (if right
                (assoc tree :right (insert right k v))
                (assoc tree :right node))
    (> key k) (if left
                (assoc tree :left (insert left k v))
                (assoc tree :left node)))))

(defn lookup [{:keys [key value left right] :as tree} k]
  (cond
    (> key k) (when right (lookup right k))
    (< key k) (when left (lookup left k))
    :else value))

(defn in-order [{:keys [key value left right] :as tree}]
  (lazy-cat
    (when left (in-order left))
    [value]
    (when right (in-order right))))

(defn depth [{:keys [left right] :as tree}]
  (max
    (inc (if left (depth left) 0))
    (inc (if right (depth right) 0))))

(defn balanced? [{:keys [left right] :as tree}]
  (= (inc (if left (depth left) 0))
     (inc (if right (depth right) 0))))

(defn smallest [{:keys [left] :as tree}]
  (if left
    (smallest left)
    tree))

(defn delete [{:keys [left right key] :as tree} k]
  (cond
    (< key k) (when right (assoc tree :right (delete right k)))
    (> key k) (when left  (assoc tree :left  (delete left k)))
    :else (cond
            (and left right) (let [small (smallest right)
                                   sk (:key small)
                                   sv (:value small)
                                   sright (:right small)]
                               {:key sk :value sv :left left :right (delete right sk)})
            left left
            right right
            :else nil)))

#_(-> (bst 15 15)
    (insert 10 10)
    (insert 11 11)
    (insert 20 20)
    (insert 16 16)
    (insert 14 14)
    (insert 22 22)
    (insert 50 50)
    ; (lookup 16)
    ; in-order
    ; depth
    ; balanced?
    (delete 20)
    ; smallest
    pprint
    )
