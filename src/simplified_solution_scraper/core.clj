(ns simplified-solution-scraper.core
  (:gen-class)
  (:use simplified-solution-scraper.instructions)
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]))


; Goal:
#_(def problem-instruction-map
  {:number-io '[integer_add exec_do*times 5 "hi there" \h 23.42 [2 1 3] string_first]
   :checksum '[in1 in2 integer_mult [true false] true "what\"ever" 1]
   })

; How used later:
#_(defn read-instructions-file
  "For now, a test of how reading the instructions file would go"
  []
  (let [instructions (repeatedly 20 #(rand-nth (rand-nth (vals problem-instruction-map))))]
    (doseq [i instructions]
      (println)
      (println i)
      (println (type i)))))

(defn print-nice-solution-map
  "Nicely prints the solution map in the right format."
  [solution-map]
  (println "{")
  (doseq [[problem instructions] solution-map]
    (print " " problem "'[")
    (doseq [ins instructions]
      (pr ins)
      (print " "))
    (println "]"))
  (println "}"))

(defn process-solution-maps
  "Returns map of the form:
  {:number-io [integer_add 5 ...]
   :compare-string-lengths [string_count 2 in1 ...]
   ...
   }
  solution-maps is a sequence of maps, each of which has keys of :problem-name and
  :simplified-program."
  [solution-maps]
  (loop [result {}
         solution-maps solution-maps]
    (if (empty? solution-maps)
      result
      (let [sol-map (first solution-maps)]
        (recur (assoc result
                      (:problem-name sol-map)
                      (concat (get result (:problem-name sol-map) '())
                            (:simplified-program-instructions sol-map)))
               (rest solution-maps))))))

(defn extract-instructions-from-program
  "Takes a simplified Push program and extracts its instructions."
  [program]
  (loop [zipper (zip/seq-zip program)
         instructions []]
    (if (zip/end? zipper)
      instructions
      (recur (zip/next zipper)
             (if (or (nil? (zip/node zipper))
                     (seq? (zip/node zipper)))
               instructions
               (conj instructions (zip/node zipper)))))))

(defn scrape-solutions
  ""
  [dir]
  (assert (.isDirectory (io/file dir)))
  (let [d (io/file dir)
        files (file-seq d)]
    (let [solution-maps
          (filter
           identity
           (for [file (filter #(and (.isFile %)
                                    (re-matches #".*log.*\.txt" (.getName %)))
                              files)]
             (with-open [rdr (clojure.java.io/reader file)]
               (let [lines (line-seq rdr)
                     simplified-solution-0-test-error
                     (and (not (empty? (filter #(= % ";; Problem-Specific Report of Simplified Solution")
                                               lines)))
                          (= "Test total error for best: 0"
                             (last (filter #(re-matches #"^Test total error for best:.*" %)
                                           lines))))]
                                        ; Only look at files with simplified solution test error of 0
                 (when simplified-solution-0-test-error
                   (let [problem-name (keyword (last (re-matches #".*/(.*)/log\d{1,2}\.txt" (.getPath file))))
                         simplified-program-str (subs (last (filter (fn [line]
                                                                      (re-matches #"^program:.*" line))
                                                                    lines))
                                                      (count "program: "))
                         simplified-program (read-string simplified-program-str)
                         instructions (extract-instructions-from-program simplified-program)]
                     {:problem-name problem-name
                      :simplified-program-instructions instructions}))))))
          result-map (process-solution-maps solution-maps)]
      (print-nice-solution-map result-map))))



(defn -main
  "Starts scraping solutions. Takes command line arg of directory location"
  [& args]
  (assert (= 1 (count args))
          "Requires 1 command line argument, which is the directory containing the problem directories, each of which contains the log files.")
  (let [dir (first args)]
    (scrape-solutions dir)))



