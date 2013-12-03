(ns workout-parser.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as tools])
  (:gen-class :main true))

;; Pattern for date lines, which must start with # character, followed by arbitrary
;; number of whitespaces, then by date in the format dd.MM.yyyy which could be
;; followed by arbitrary number of whitespaces (but no other characters)
(def line-date-pattern #"^#\s*([0-9]{2})\.([0-9]{2})\.([0-9]{4})\s*$")

;; Pattern for amount lines, which must start with the asterix (*) character, followed
;; by arbitrary number of whitespaces, then by a unbounded number of whitespace or word
;; characters
(def line-amount-pattern #"^\*\s*([0-9]+)[\s\w]*$")

;; Pattern for matching dates from command line options, which must be strings in the
;; format dd.MM.yyyy with no other characters before or after such pattern
(def input-date-pattern #"\A([0-9]{2})\.([0-9]{2})\.([0-9]{4})\z")

(defn parse-date-string
  "Parses date-string with the help of regex pattern which should capture day, month and year in exact this order.
   The last parameter throw-on-mismatch? indicates if the function should throw exception on mismatch or not."
  [^java.util.regex.Pattern Pattern date-string throw-on-mismatch?]
  (if-let [[_ day month year] (re-find Pattern date-string)]
    [(Integer. year) (Integer. month) (Integer. day)]
    (when throw-on-mismatch?
      (throw (Exception. (str "bad date format encountered, be sure to supply date in format: " (.pattern Pattern)))))))

(defn parse-markdown
  "Parses files (specified by paths in argument filenames) in markdown format containing log of workout activities.
   This log must adhere to some rules -> record for each day starts with the line '# dd.MM.yyyy' and each successive
   line containes workout amount which starts with the asterix (*) sign followed by unbounded number and (optional)
   any combination of whitespace and word characters (description of the workout). Record for each day ends by the
   start of the next record or by end of file. Output from this function is a map of workout days (vector of three
   integers [year month day]) to sequences of workout amounts (sequence of integers (amount1, amount2 ...))."
  [filenames]
  (let [readers (map io/reader filenames)]
    (try
      (first (reduce (fn [[workout-map current-date] line]
                       (if-let [new-date (parse-date-string line-date-pattern line false)]
                         [workout-map new-date]
                         (if-let [[amount-match amount] (re-find  line-amount-pattern line)]
                           [(update-in workout-map [current-date] conj (Integer. amount)) current-date]
                           [workout-map current-date])))
                     [{} nil]
                     (apply concat (map line-seq readers))))
      (finally
        (doseq [rdr readers]
          (. rdr close))))))

(defn query-workout
  "Queries workout map from the function parse-markdown by the start and end date parameters (each of those parameters
   could be nil, in this case, it's ignored in filtering). Returns lazy sequence of filtered [k v] pairs from workout map."
  [workout-map start end]
  (let [filter-fn `(fn [[~'date ~'_]]
                     ~(let [start-clause (when start `(<= (compare ~(parse-date-string input-date-pattern start true) ~'date) 0))
                            end-clause (when end `(>= (compare ~(parse-date-string input-date-pattern end true) ~'date) 0))
                            clauses (filter identity (list start-clause end-clause))]
                        `(and ~@clauses)))]
    (filter (eval filter-fn) workout-map)))

(defn sum-workout
  "Reduces workout map by summing values under each key (the value must be sequence of numbers) and finally summing them
   all into one result."
  [workout-map]
  (reduce (fn [acc [_ v]] (+ acc (reduce + 0 v))) 0 workout-map))

(defn -main
  [& args]
  (let [[{:keys [help start end]} paths banner] (tools/cli args (str "This program can be used to summarize amount in workout files written in markdown format\n"
                                                                     "The information in the workout file must adhere to some simple rules:\n"
                                                                     "Workout days must be defined by lines with format # dd.MM.yyyy\n"
                                                                     "Lines where workout amounts is mentioned must start with asterix (*)\n"
                                                                     "Start date parameter (optional) must have format dd.MM.yyyy\n"
                                                                     "End date parameter (optional) must have format dd.MM.yyyy")
                                                           ["-h" "--help" "Show help" :flag true :default false]
                                                           ["-s" "--start" "Start date (including) of summary in format dd.MM.yyyy"]
                                                           ["-e" "--end" "End date (including) of summary in format dd.MM.yyyy"])]
    (when help
      (println banner)
      (System/exit 0))
    (if (seq paths)
      (try (do (let [summary (-> (parse-markdown paths)
                                 (query-workout start end)
                                 (sum-workout))]
                 (println (str "Your workout summary is: " summary)))
               (System/exit 0))
        (catch Exception e (do (println (str "Exception encountered - " (.getMessage e)))
                               (System/exit 1))))
      (do (println "You need to provide path/s to workout file/s")
          (System/exit 0)))))
