(definflate (stamp 'timestamp)
  ;; Inflate a timestamp value
  ;; into a timestamp object
  (local-time:universal-to-timestamp stamp))

(defdeflate (stamp local-time:timestamp)
  ;; Deflate a timestamp object
  ;; into a string
  (local-time:format-timestring nil stamp))
