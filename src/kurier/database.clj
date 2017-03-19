(ns kurier.database
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(defn load-file []
  (if-let [data (slurp (io/file (io/resource "database.json")))]
    (try (json/read-str data :key-fn keyword)
      (catch Exception e {}))))

(defn write-file [data]
  (with-open [out-file (io/writer "./resources/database.json" :encoding "UTF-8")]
    (.write out-file (json/write-str data)))
  data)

(defn get [key]
  (-> (load-file)
      (keyword key)))

(defn save [key value]
  (as-> (load-file) data
        (assoc data (keyword key) value)
        (write-file data)))

(defn save-in [keys value]
  (as-> (load-file) data
        (assoc-in data (map keyword keys) value)
        (write-file data)))
