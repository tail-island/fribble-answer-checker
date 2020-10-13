(ns fribble-answer-checker.program
  (:require [fribble-answer-checker.core :as core  ]
            [clojure.string              :as string])
  (:gen-class))

(defn -main [& [question-path answer-path]]
  (try
    (let [[question answer] (core/read-question-and-answer question-path answer-path)]
      (println (if (core/check-answer question answer)
                 (string/join "\t" (core/calc-score question answer))
                 "NG")))
    (catch Exception e
      (println (.getMessage e)))))
