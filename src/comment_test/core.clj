(ns comment-test.core
  (:gen-class)
  (:require [clojure.string :as str]
            [comment-test.midex :refer [midex]]))

(defn word-split [line]
  (str/split line #"\s"))

(def exclusion-words (set (apply concat (map word-split (map str midex)))))

(def school-words ["초" "중" "고" "대"])

(defn include-school-words? [word]
  (some #(str/includes? word %) school-words))

(defn ends-with-shcool-words? [word]
  (some #(str/ends-with? word %) school-words))

(defn word-filter [word-vec]
  (filter include-school-words? word-vec))

(defn remove-symbol [line]
  (str/replace line #"[!@#$%^&*()-=_+~`'\";:?/\\★❤♥❣♡❗✋✨❄️]" ""))

(defn entries [zipfile]
  (enumeration-seq (.entries zipfile)))

(defn get-line-from-zip [file-name]
  (with-open [zip-file (java.util.zip.ZipFile. file-name)]
    (doall (apply concat (map #(doall (line-seq (clojure.java.io/reader (.getInputStream zip-file %))))
                              (filter #(and (not (.isDirectory %))
                                            (not (str/starts-with? (.getName %) "__MACOSX")))
                                      (enumeration-seq (.entries zip-file))))))))

(defn get-names [file-name]
  (with-open [zip-file (java.util.zip.ZipFile. file-name)]
    (doall (filter #(and (not (.isDirectory %))
                              (not (str/starts-with? (.getName %) "__MACOSX")))
                   (enumeration-seq (.entries zip-file))))))

(defn make-school-str [word]
  (cond
    (and (str/includes? word "초")
         (str/includes? word "초등"))
    (subs word 0 (inc (str/last-index-of word "초")))

    (and (str/includes? word "중")
         (str/includes? word "중학"))
    (subs word 0 (inc (str/last-index-of word "중")))

    (and (str/includes? word "고")
         (str/includes? word "고등"))
    (subs word 0 (inc (str/last-index-of word "고")))

    (and (str/includes? word "대")
         (str/includes? word "대학"))
    (subs word 0 (inc (str/last-index-of word "대")))

    :else word))

(defn exclude-word? [word]
  (or (< (count word) 2)
      (and (str/includes? word "초")
           (not (ends-with-shcool-words? word)))
      (and (str/includes? word "중")
           (not (ends-with-shcool-words? word)))
      (and (str/includes? word "고")
           (not (ends-with-shcool-words? word)))
      (and (str/includes? word "대")
           (not (ends-with-shcool-words? word)))))

(defn make-school-keyword [word]
  (let [ school-str (make-school-str (apply str (distinct word)))]
    (if (exclude-word? school-str)
      "etc"
      school-str)))

(defn school-counter [school-map word]
  (if (contains? exclusion-words word)
    school-map
    (let [school-keyword (make-school-keyword word)]
      (if (contains? school-map school-keyword)
        (update school-map school-keyword inc)
        (into school-map {school-keyword 1})))))

(defn get-schools[line]
  (->> (map remove-symbol line)
       (map word-split)
       (map word-filter)
       (apply concat)))

(defn -main [& args]
  (let [schools (sort-by val > (dissoc (->> (first args)
                                            get-line-from-zip
                                            get-schools
                                            (reduce school-counter {}))
                                       "etc"))]
    (println (str/join (map #(str (str/join "\t" %) "\n") schools)))
    (println (count schools))))

