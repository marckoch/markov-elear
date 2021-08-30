(ns markov-elear.generator
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def example "And the Golden Grouse And the Pobble who")

(def words (str/split example #" "))

(def word-transitions (partition-all 3 1 words))

word-transitions

(merge-with concat {:a [1]} {:a [2]})

(merge-with clojure.set/union {:a [1]} {:a [2]})

(reduce (fn [r t] (merge-with set/union r
                              (let [[a b c] t]
                                {[a b] (if c #{c} #{})})))
        {}
        word-transitions)

(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with set/union r
                                (let [[a b c] t]
                                  {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->word-chain [s]
  (let [words (str/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn chain->text [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn generate-text
  [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file [fname]
  (text->word-chain
   (slurp (io/resource fname))))

(generate-text "And the" (process-file "quangle-wangle.txt"))