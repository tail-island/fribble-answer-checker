(ns fribble-answer-checker.program
  (:require [fribble-answer-checker.core :as core])
  (:gen-class))

(defn -main [& [question-path answer-path]]
  (try
    (println (if (apply core/check-answer (core/read-question-and-answer question-path answer-path))
               "OK"
               "NG"))
    (catch Exception e
      (println (.getMessage e)))))
