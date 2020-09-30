(ns fribble-answer-checker.core
  (:require [clojure.data.json      :as    json                  ]
            [clojure.string         :as    string                ]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]))

(defn read-json
  [path]
  (json/read-str (slurp path) :key-fn ->kebab-case-keyword))

(defn read-question-and-answer
  [question-path answer-path]
  (map read-json [question-path answer-path]))

(defn check
  [spec-f message x]
  (when (not (spec-f x))
    (throw (Exception. message)))
  x)

(defn check-answer
  [question answer]
  (letfn [(find-skill [id]
            (->> (filter #(= (:id %) id) (:skills question))
                 (check #(= (count %) 1) (str "スキル" id "が重複しているか、存在しません。"))
                 (first)))
          (find-task [id]
            (->> (filter #(= (:id %) id) (:tasks question))
                 (check #(= (count %) 1) (str "タスク" id "が重複しているか、存在しません。"))
                 (first)))
          (find-member [id]
            (->> (filter #(= (:id %) id) (:members question))
                 (check #(= (count %) 1) (str "メンバー" id "が重複しているか、存在しません。"))
                 (first)))
          (find-assign [task-id]
            (->> (filter #(= (:task-id %) task-id) answer)
                 (check #(= (count %) 1) (str "タスク" task-id "のアサインが重複しているか、存在しません。"))
                 (first)))
          (task-is-assigned? [task]
            (let [assign              (find-assign (:id task))
                  duration-is-correct (->> (= (:duration task) (+ (apply - ((juxt :end-day :start-day) assign)) 1))
                                           (check identity (str "タスク" (:id task) "のアサイン期間が不正です。")))
                  member              (find-member (:member-id assign))
                  member-has-skills   (->> (every? (set (:skill-ids member)) (:skill-ids task))
                                           (check identity (str "タスク" (:id task) "にアサインされたメンバーは必要なスキルを持っていません。")))]
              (and duration-is-correct member-has-skills)))
          (member-works-one-task-per-day? [member]
            (let [assigns (->> (filter #(= (:member-id %) (:id member)) answer)
                               (sort-by :start-day))]
              (->> (every? identity (map #(> (:start-day %2) (:end-day %1)) assigns (next assigns)))
                   (check identity (str "メンバー" (:id member) "が、同じ日に複数のタスクにアサインされています。")))))
          (predecessors-are-finished? [task]
            (letfn [(find-parent-task [task]
                      (->> (filter #(contains? (set (:child-ids %)) (:id task)) (:tasks question))
                           (first)))
                    (ancestors [task]
                      (lazy-seq (when task
                                  (cons task (ancestors (find-parent-task task))))))
                    (predecessors [task]
                      (map find-task (:predecessor-ids task)))
                    (descendants [task]
                      (lazy-seq (cons task (mapcat (comp descendants find-task) (:child-ids task)))))]
              (let [assign              (find-assign (:id task))
                    predecessor-assigns (->> task
                                             (ancestors)
                                             (mapcat predecessors)
                                             (mapcat descendants)
                                             (distinct)
                                             (filter :duration)
                                             (map :id)
                                             (map find-assign))]
                (->> (every? #(< (:end-day %) (:start-day assign)) predecessor-assigns)
                     (check identity (str "タスク" (:id task) "の先行タスクが完了していません。"))))))]
    (and (->> question :tasks   (filter :duration) (every? task-is-assigned?))
         (->> question :members                    (every? member-works-one-task-per-day?))
         (->> question :tasks   (filter :duration) (every? predecessors-are-finished?)))))
