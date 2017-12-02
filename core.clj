(ns pr.core
  (:gen-class))

(defn positions [e coll] (keep-indexed #(if (= e %2) %1) coll))

(def preprocess #(->> % (re-seq #"\w+") (map clojure.string/lower-case)))

(defn create-node [word] {:word word :freq 1})

(defn create-chain [word-seq]
  (cond (empty? word-seq) nil
        (empty? (rest word-seq)) {:node (create-node (first word-seq))}
        :else {:node (create-node (first word-seq))
               :children [(create-chain (rest word-seq))]}))

(defn freq-bump [tree]
  (update-in tree [:node :freq] inc))

(defn merge-into-tree [tree tweet]  
  (cond
    (empty? tweet) tree
    (not= (first tweet) (:word (:node tree))) tree
    (empty? (rest tweet)) (freq-bump tree)
    :else
      (let [tree (freq-bump tree)
            pos (positions
                  (second tweet)
                  (->> tree
                    :children
                    (map (comp :word :node))
                    vec))]
        (if (empty? pos)
          (update-in tree [:children] conj (create-chain (rest tweet)))  
          (update-in tree [:children (first pos)] merge-into-tree (rest tweet))))))

(defn compress-phrase [phrase-vec tweet]
  (let [init-state {:tweet []
                    :phrase (vec phrase-vec)
                    :to-match (vec phrase-vec)
                    :joined-phrase (clojure.string/join " " phrase-vec)}
        reducer
          (fn [state word]
            (if (= word (first (:to-match state)))
              (if (= 1 (count (:to-match state)))  
                (assoc state
                  :tweet (conj (:tweet state) (:joined-phrase state))
                  :to-match (:phrase state))
                (update-in state [:to-match] rest))
              (assoc state
                :tweet (conj (:tweet state) word)
                :to-match (:phrase state))))]
    (->> tweet (reduce reducer init-state) :tweet)))

(defn split-tweet [clean-phrase tweet-vec]
  (let [pos (positions clean-phrase tweet-vec)]
    (if (empty? pos) nil
      (let [ind (first pos)]
        [(->> tweet-vec (take (+ ind 1)) reverse)
         (->> tweet-vec (drop ind))]))))


(defn build-tree [sentences phrase]
  (let [empty-tree {:node (create-node phrase) :children []}]
    (reduce merge-into-tree empty-tree sentences)))

(defn tree-seq-path
  "Like core's tree-seq but returns a lazy sequence of vectors of the
  paths of the nodes in a tree, via a depth-first walk. It optionally
  applies node-fn to each node before adding it to the path.

  branch? must be a fn of one arg that returns true if passed a node
  that can have children (but may not).
  
  children must be a fn of one arg that returns a sequence of the children.
  Will only be called on nodes for which branch? returns true.

  root is the root node of the tree."
  [branch? children root & [node-fn]]
  (let [node-fn (or node-fn identity)
        walk (fn walk [path node]
               (let [new-path (conj path (node-fn node))]
                (lazy-seq
                 (cons new-path
                   (when (branch? node)
                     (mapcat (partial walk new-path) (children node)))))))]
    (walk [] root)))

(def stopwords
  (-> "/Users/jaihindhreddy/pr/stopwords.txt"
    slurp
    (clojure.string/split #"\n")
    set))

(defn score-path [path]
  (->> path
    rest
    (map #(if (contains? stopwords (:word %)) 0 (:freq %)))
    (apply +)))

(defn tap [n coll] (do (print (take n coll)) coll))


(defn best-phrase [tree]
  (let
    [valid-paths
      (->> tree
        (tree-seq-path :children :children)
        (filter #(empty? (:children (last %))))
        (map #(map :node %))
        (map (fn [x] (take-while #(> (:freq %) 1) x)))
        (remove #(or (empty? %) (empty? (rest %)))))]
    (if
      (empty? valid-paths)
      {:phrase (:word (:node tree)) :score 0}
      (->> valid-paths
        (apply max-key score-path)
        (#(assoc {} :phrase (map :word %)
                    :score (score-path %)))))))
    
(defn process-tweets [tweets phrase-vec]
  (let [clean-phrase (clojure.string/join " " phrase-vec)
        split-tweet-vec
          (into []
            (comp (map preprocess)
                  (map #(compress-phrase phrase-vec %))
                  (map #(split-tweet clean-phrase %))
                  (filter (complement nil?)))
            tweets)]
    [(map first split-tweet-vec)
     (map second split-tweet-vec)]))

(defn pr-alg [tweets phrase]
  (let [phrase-vec (preprocess phrase)
        clean-phrase (clojure.string/join " " phrase-vec)
        [back-tweets forward-tweets] (process-tweets tweets phrase-vec)
        back-tree (build-tree back-tweets clean-phrase)
        forward-tree (build-tree forward-tweets clean-phrase)
        forward-max-phrase (best-phrase forward-tree)
        back-max-phrase (best-phrase back-tree)]
    (if (> (:score forward-max-phrase) (:score back-max-phrase))
      (let [phrase-vec (:phrase forward-max-phrase)
            clean-phrase (clojure.string/join " " phrase-vec)]
        (-> tweets
          (process-tweets phrase-vec)
          first
          (build-tree clean-phrase)
          best-phrase
          :phrase))
      (let [phrase-vec (reverse (:phrase back-max-phrase))
            clean-phrase (clojure.string/join " " phrase-vec)]
        (-> tweets
          (process-tweets phrase-vec)
          second
          (build-tree clean-phrase)
          best-phrase
          :phrase)))))
          
; (#(cons (first %) (reverse (rest %))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
