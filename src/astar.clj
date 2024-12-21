(ns astar
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

;; --- A* algorithm code adapted from https://github.com/arttuka/astar

(defn ^:private generate-route [node came-from]
  (loop [route '()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      (cons node route) ;; slight adaptation to include the start in the path
      )))

(defn a*
    "Finds the shortest route from start to goal in a graph.
  Graph is a function (eg. a map) from nodes to a collection of adjacent nodes.
  Dist is a function from two nodes to the distance (as a number) from the first node to the second.
  H is a function from a node to the heuristic distance from that node to the goal. It should never overestimate the distance.
  Start and goal are two nodes.
  Returns a list of nodes on the route, excluding the start node and including the goal node. If a route can't be found, returns nil."
    [graph start goal distance heuristic]
    (loop [visited {}
           queue (priority-map-keyfn first start [0 0 nil])]
      (when (seq queue)
        (let [[current [_ current-score previous]] (peek queue)
              visited (assoc visited current previous)]
          (if (= current goal)
            (generate-route goal visited)
            (recur visited
                   (reduce (fn [queue node]
                             (let [score (+ current-score (distance current node))]
                               (if (and (not (contains? visited node))
                                        (or (not (contains? queue node))
                                            (< score (get-in queue [node 1]))))
                                 (assoc queue node [(+ score (heuristic node goal)) score current])
                                 queue)))
                           (pop queue)
                           (graph current))))))))

;---

(defn bfs-shortest-paths
  [graph start end]
  (letfn [(bfs [queue visited shortest-length paths]
            (if (empty? queue)
              paths
              (let [[path & rest-queue] queue
                    current (last path)
                    path-length (count path)]
                (cond
                  ;; If the current path exceeds the shortest length found, stop exploring.
                  (and shortest-length (> path-length shortest-length))
                  (set paths)

                  ;; If we reached the end node:
                  (= current end)
                  (let [new-shortest-length (if shortest-length
                                              (min shortest-length path-length)
                                              path-length)]
                    ;; Add the path to the results and continue BFS.
                    (recur rest-queue visited new-shortest-length (conj paths path)))

                  ;; Otherwise, explore neighbors.
                  :else
                  (let [new-visited (conj visited current)
                        neighbors (filter #(not (visited %)) (get graph current []))]
                    (recur (concat rest-queue
                                   (map #(conj path %) neighbors))
                           new-visited
                           shortest-length
                           paths))))))]
    ;; Start BFS from the start node, with no shortest length yet and no paths found.
    (bfs [[start]] #{} nil [])))
