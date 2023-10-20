(defrecord Graph [vertices edges])
(defrecord Edge [from to weight label])
(defrecord Vertex [label neighbors latitude longitude status distance])

(def ^:const vertex-status-unseen 0)
(def ^:const vertex-status-in-queue 1)
(def ^:const vertex-status-current 2)
(def ^:const vertex-status-visited 3)

;; Graph Structure

(defn make-graph []
  (Graph. (ref {}) (ref {})))

(defmacro unless [pred & body]
  `(if (not ~pred) (do ~@body)))


(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn graph-add-vertex! [graph label lat lon]
  (unless (graph-has-vertex? graph label)
          (dosync
           (alter (:vertices graph) assoc label
                  (Vertex. label (ref '()) lat lon
                           (ref vertex-status-unseen)
                           (ref -1))))))

(defn graph-edge-key [from to]
  (sort (list from to)))

(defn graph-has-edge? [graph from to]
  (contains? @(:edges graph) (graph-edge-key from to)))

;; Graph Edges

(defn graph-add-edge! [graph from to label weight]
  (unless (graph-has-edge? graph from to)
          (dosync
           (alter (:edges graph) assoc (graph-edge-key from to) (Edge. from to weight label))
           (alter (:neighbors (get @(:vertices graph) from)) conj to)
           (alter (:neighbors (get @(:vertices graph) to)) conj from))))

(defn graph-get-vertex [graph label]
  (get @(:vertices graph) label))

(defn graph-vertex-status? [graph label status]
  (= @(:status (graph-get-vertex graph label)) status))

(defn set-status! [graph label status]
  (dosync (ref-set (:status (graph-get-vertex graph label)) status)))

(defn status-to-queue [graph vertex]
  (doseq [neighbor-label (filter #(graph-vertex-status? graph % vertex-status-unseen)
                                 @(:neighbors vertex))]
    (set-status! graph neighbor-label vertex-status-in-queue)))

(defn graph-get-edge [graph from to]
  (get @(:edges graph) (graph-edge-key from to)))

(defn enqueue! [queue element]
  (dosync
   (alter queue conj element)))

(defn dequeue! [queue]
  (let [first-element (first @queue)]
    (dosync
     (alter queue rest))
    first-element))

;; Graph Reset

(defn graph-reset [graph]
  (dosync
   (doseq [vertex (vals @(:vertices graph))]
     (ref-set (:status vertex) vertex-status-unseen)
     (ref-set (:distance vertex) -1))))

(defn graph-reset-status [graph]
  (dosync
   (doseq [vertex (vals @(:vertices graph))]
     (ref-set (:status vertex) vertex-status-unseen))))

(defn vertex-get-best-neighbor [graph vertex]
  (->> @(:neighbors vertex)
       (filter #(= (graph-vertex-status? graph % vertex-status-visited) true))
       (sort-by #(-> % (graph-get-vertex graph) :distance))
       first))


;; BFS functions

(defn bfs [graph start end]
  (let [queue (ref [start])
        prev (ref {})]
    (graph-reset-status graph)
    (set-status! graph start vertex-status-current)

    (loop [current (last @queue)]
      (if current
        (do
          (dosync (alter queue butlast))
          (set-status! graph current vertex-status-visited)

          (if (= current end)
            nil ; exit the loop if the destination is reached
            (do
              (doseq [neighbor-label @(:neighbors (graph-get-vertex graph current))]
                (when (graph-vertex-status? graph neighbor-label vertex-status-unseen)
                  (dosync
                   (alter queue conj neighbor-label)
                   (alter prev assoc neighbor-label current))
                  (set-status! graph neighbor-label vertex-status-in-queue)))
              (recur (last @queue)))))
        nil))
    (let [path (loop [path [] current end i 0]
                 (if current
                   (recur (cons {:p current :c i} path) (@prev current) (+ i 1))
                   path))]
      (if (or (empty? path) (= (count path) 1))
        (println "No Path found!\n")
        (do
          (doseq [city (butlast path)]
            (println (str ">> " (:p city) " :: " (:c city))))
          (println "**" (:p (last path)))
          (println "Arrived at finish!\n")))
      path)))

;; Dijkstra Function

(defn dijkstra [graph start-node]
  (dosync
   (graph-reset-status graph)
   (let [distances (atom {start-node 0})
         unvisited (atom (set (keys @(:vertices graph))))]
     (loop []
       (if (empty? @unvisited)
         @distances
         (let [current-node (first (sort-by (fn [v] (if (get @distances v)
                                                      (get @distances v 0)
                                                      Double/POSITIVE_INFINITY)) (seq @unvisited)))
               current-distance (get @distances current-node)]
           (swap! unvisited disj current-node)
           (doseq [neighbor @(:neighbors (graph-get-vertex graph current-node))]
             (let [edge (graph-get-edge graph current-node neighbor)]
               (let [new-distance (if current-distance
                                    (+ current-distance (:weight edge))
                                    (:weight edge))]
                 (if (or (not (contains? @distances neighbor))
                         (< new-distance (get @distances neighbor)))
                   (swap! distances assoc neighbor new-distance)))))
           (recur)))))))

;; Shortest Path Finder that calls both functions

(defn shortest-path-finder [graph start-node end-node]
  (dosync
   (let [distances (dijkstra graph start-node)
         edistances (dijkstra graph end-node)
         sdist (get distances end-node Double/POSITIVE_INFINITY)
         edist (get edistances start-node Double/POSITIVE_INFINITY)]
     (println (if (< sdist edist) sdist edist))
     (bfs graph start-node end-node)
     nil)))


(def g (make-graph))
(load-file "/Users/artndrd/Desktop/Clojure-code/e-roads-2020.clj")

(defn print-graph [graph]
  (println "Vertices:")
  (doseq [[label vertex] @(:vertices graph)]
    (println "Label:" label "Neighbors:" @(:neighbors vertex) "Distance:" @(:distance vertex) "Status:" @(:status vertex)))

  (println "\nEdges:")
  (doseq [[key edge] @(:edges graph)]
    (println "From:" (:from edge) "To:" (:to edge) "Weight:" (:weight edge)))
  (println))

;; Printing the shortest path by calling shortest-path-finder

(println "Shortest path (by road length) from Navoiy to Prague:")
(println (shortest-path-finder g "Navoiy" "Prague"))

(graph-reset-status g)
(println "Shortest path (by road length) from Bonifacio, Corse-du-Sud to Prague:")
(println (shortest-path-finder g "Bonifacio, Corse-du-Sud" "Prague"))


