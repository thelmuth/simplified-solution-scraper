(ns simplified-solution-scraper.core
  (:gen-class))


(defn scrape-solutions
  ""
  [dir]
  nil)



(defn -main
  "Starts scraping solutions. Takes command line arg of directory location"
  [& args]
  (assert (= 1 (count args))
          "Requires 1 command line argument, which is the directory containing the problem directories, each of which contains the log files.")
  (let [dir (first args)]
    (scrape-solutions dir)))
