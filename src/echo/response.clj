(ns echo.response)

; Contains functions for generating a response
(defn simple-card
  "Builds a map representing a simple card from the provided entries."
  [title content]
  {"type"    "Simple"
   "title"   title
   "content" content})

(defn link-account-card
  "Builds a map representing a LinkAccountCard."
  []
  {"type" "LinkAccount"})

(defn with-card
  "Returns a version of the provided response with the card inserted
   into the response."
  [response
   card]
  (assoc-in response ["response" "card"] card))

(defn plaintext-speech
  "Builds a map representing plaintext speech output with the provided type and text."
  [text]
  {"type" "PlainText"
   "text" text})

(defn ssml-speech
  "Builds a map representing speech specified by SSML."
  [ssml]
  {"type" "SSML"
   "ssml" ssml})

(defn with-speech
  "Returns a version of the provided response with the speech inserted
   into the response."
  [response
   speech]
  (assoc-in response ["response" "outputSpeech"] speech))

(defn with-attributes
  "Returns a version of the provided response with its attributes
   replaced with the provided attributes."
  [response
   attributes]
  (assoc response "sessionAttributes" attributes))

(def directive-types "Legal directive type names"
  #{"Dialog.Delegate" "Dialog.ElicitSlot" "Dialog.ConfirmSlot"})

(defn dialog-directive
  "Returns a map representing a directive to be sent back to Alexa.  Directive must
   be one of the known directive types (see directive-types), and updated-intent
   must be an intent, stemming from the intent that would have been sent in the prior
   request."
  [directive updated-intent]
  {:pre [(directive-types directive)
         (map? updated-intent)]}
  {"type" directive
   "updatedIntent" {"name" (get updated-intent "name")
                    "confirmationStatus" (get updated-intent "confirmationStatus" "NONE")
                    "slots" (into {}
                                  (for [[slot spec] (get updated-intent "slots")]
                                    [slot (merge {"name" (get spec "name")
                                                  "confirmationStatus" (get spec "confirmationStatus" "NONE")}
                                                 (when (get spec "value")
                                                   {"value" (get spec "value")}))]))}})

(defn delegate
  "Adds a Dialog.Delegate directive to a response."
  [response updated-intent]
  {:pre [(not (get response "outputSpeech"))
         (not (get response "reprompt"))]}
  (-> response
      (assoc-in ["response" "shouldEndSession"] false)
      (update-in ["response" "directives"]
                 (fn [old]
                   (conj (or old [])
                         (dialog-directive "Dialog.Delegate" updated-intent))))))

(defn elicit-slot
  "Adds a Dialog.ElicitSlot directive to a response.  The response should ultimately
   contain an outputSpeech entry, but elicit-slot does not enforce this."
  [response slot-to-elicit updated-intent]
  {:pre [(not= (get updated-intent "dialogState") "COMPLETED")]}
  (-> response
      (assoc-in ["response" "shouldEndSession"] false)
      (update-in ["response" "directives"]
                 (fn [old]
                   (conj (or old [])
                         (assoc (dialog-directive "Dialog.ElicitSlot" updated-intent)
                                "slotToElicit" slot-to-elicit))))))

(defn confirm-slot
  "Adds a Dialog.ConfirmSlot directive to a response. The response should ultimately
   contain an outputSpeech entry, but confirm-slot does not enforce this."
  [response slot-to-confirm updated-intent]
  {:pre [(not= (get updated-intent "dialogState") "COMPLETED")]}
  (-> response
      (assoc-in ["response" "shouldEndSession"] false)
      (update-in ["response" "directives"]
                 (fn [old]
                   (conj (or old [])
                         (assoc (dialog-directive "Dialog.ConfirmSlot" updated-intent)
                                "slotToConfirm" slot-to-confirm))))))

(defn respond
  "Builds a complete response with the provided arguments.
   The argument is a hash of options. Supported values:
   :attributes should point to a map of {string object}, which will overwrite any existing session attribute map.
   :card should point to a valid card, or not be present if no card is sent
   :speech should point to valid speech, or not be present if no speech is sent
   :should-end? is whether the app session should close. Defaults to true.
   If the second map is not sent, a response with no card, no speech, and a should-end? value of true will be created."
  ([] (respond {}))
  ([{:keys [attributes card speech should-end?]
     :or   {should-end? true}}]
   (let [response {"version"  "1.0"
                   "response" {"shouldEndSession" should-end?}}
         response (if attributes
                    (with-attributes response attributes)
                    response)
         response (if speech
                    (with-speech response speech)
                    response)
         response (if card
                    (with-card response card)
                    response)]
     response)))
