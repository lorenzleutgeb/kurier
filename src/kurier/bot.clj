(ns kurier.bot
  (:gen-class)
  (:require [clojure.string :as s]
            [environ.core :refer [env]]
            [kurier.facebook :as fb]
            [kurier.database :as db]))

(defn on-message [payload]
  (println "on-message payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        message-text (get-in payload [:message :text])
        user-info (fb/get-user-profile sender-id)
        first-name (:first_name user-info)]
    (println user-info)
    (cond
      (s/includes? (s/lower-case message-text) "help") (fb/send-message sender-id (fb/text-message "Hi there, happy to help :)"))
      (s/includes? (s/lower-case message-text) "image") (fb/send-message sender-id (fb/image-message "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c5/M101_hires_STScI-PRC2006-10a.jpg/1280px-M101_hires_STScI-PRC2006-10a.jpg"))
      ;;; If no rules apply echo the user's message-text input
      :else (fb/send-message sender-id (fb/text-message message-text)))))

(def forms (atom {}))

(defn digest-form [request]
  (reset! forms (assoc @forms (:id request) request)))

(defn get-match [p coll]
  (first (keep-indexed #(when (p %2) %1) coll)))

(defn take-after [p coll]
  (drop (+ (get-match p coll) 1) coll))

(defn take-before [p coll]
  (take (- (get-match p coll) 1) coll))

(defn page-break? [item]
  (= (:type item) "PAGE_BREAK"))

(defn before-questions [items]
  (take-before page-break? items))

(defn after-questions [items]
  (take-after page-break? items))

(defn refer-article [form]
  (:description form))

(defn choice-to-button [n choice]
  {:type "postback"
   :title choice
   :payload (str "CHOICE_" n)})

(defn choices-to-buttons [choices]
  (map-indexed choice-to-button choices))

(defn ask [item]
  (case (:type item)
        "MULTIPLE_CHOICE"
        (fb/button-template (get-in item [:payload :title]) (choices-to-buttons (get-in item [:payload :choices])))
        nil))

(defn on-message [payload]
  (println "on-message payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        message-text (get-in payload [:message :text])]
    (cond
      (s/includes? (s/lower-case message-text) "forms") [(fb/text-message (str @forms))]
      (s/includes? (s/lower-case message-text) "ask") [(ask (first (:items (first (vals @forms)) (fb/text-message (str @forms)))))]
      (s/includes? (s/lower-case message-text) "image") [(fb/image-message "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c5/M101_hires_STScI-PRC2006-10a.jpg/1280px-M101_hires_STScI-PRC2006-10a.jpg")]
      ; If no rules apply echo the user's message-text input
      :else [(fb/text-message message-text)])))

(defn react-to-get-started [sender-id]
  (let [user-info (fb/get-user-profile sender-id)
        first-name (:first_name user-info)]
  ;(db/save sender-id (fb/get-user-profile sender-id))
    [(fb/text-message (str "Willkommen " first-name ", ich bin Ihr persönlicher Kurier."))
     (fb/text-message "Ich freue mich über Ihr wertvolles Feedback. Im Gegenzug dazu können Sie einen qualitativ hochwertigen Artikel gratis lesen.")
     (fb/button-message "Welchen der folgenden Artikel möchten Sie lesen?"
                                                  [{ :type "postback"
                                                     :title "Artikel 1"
                                                     :payload "ARTICLE~0"}

                                                   { :type "postback"
                                                     :title "Artikel 2"
                                                     :payload "ARTICLE~1"}

                                                   { :type "postback"
                                                     :title "Artikel 3"
                                                     :payload "ARTICLE~2"}])]))

(defn on-postback [payload]
  (println "on-postback payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        postback (get-in payload [:postback :payload])
        referral (get-in payload [:postback :referral :ref])]
    (cond
      (= postback "GET_STARTED")
      (react-to-get-started sender-id)

      :else
      [(fb/text-message "Sorry, I don't know how to handle that postback")])))

(defn on-attachments [payload]
  (println "on-attachment payload:")
  (println payload)
  (let [sender-id (get-in payload [:sender :id])
        recipient-id (get-in payload [:recipient :id])
        time-of-message (get-in payload [:timestamp])
        attachments (get-in payload [:message :attachments])]
    [(fb/text-message "Thanks for your attachments :)")]))
