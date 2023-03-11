(ns user
  (:require [hyperfiddle.rcf]
            [nextjournal.clerk :as clerk]))

(hyperfiddle.rcf/enable!)

;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
(clerk/serve! {:browse? true})


;; or let Clerk watch the given `:paths` for changes
(clerk/serve! {:watch-paths ["src"]})