#+cljs
(ns cats.testrunner
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.test :as test]
            [cljs.core.async :refer [chan put! take! <! >!]]
            [cats.builtin-spec]
            [cats.core-spec]
            [cats.monad.writer-spec]
            [cats.monad.reader-spec]
            [cats.monad.either-spec]
            [cats.monad.maybe-spec]
            [cats.monad.identity-spec]
            [cats.monad.exception-spec]
            [cats.monad.continuation-spec]
            [cats.monad.channel-spec]
            [cljs.nodejs :as nodejs]))

#+cljs
(nodejs/enable-util-print!)

#+cljs
(defn success?
  [env]
  (and (zero? (:fail env 0))
       (zero? (:error env 0))))

#+cljs
(defmethod test/report [::cats :begin-test-ns]
  [m]
  (println "[test]" (name (:ns m))))

#+cljs
(defmethod test/report [::cats :pass] [m]
  (test/inc-report-counter! :pass))

#+cljs
(defmethod test/report [::cats :error] [m]
  (test/inc-report-counter! :error)
  (println "\n  ERROR in" (test/testing-vars-str m))
  (if-let [message (:message m)]
    (println "    message:" message)
    (println "    message: Unexpected exception."))
  (when-let [value (:actual m)]
    (println "        exc:" value)))

#+cljs
(defmethod test/report [::cats :fail] [m]
  (test/inc-report-counter! :fail)
  (println "\n  FAIL in" (test/testing-vars-str m))
  (when (seq (:testing-contexts (test/get-current-env)))
    (println (test/testing-contexts-str)))
  (when-let [message (:message m)]
    (println message))
  (println "    expected:" (pr-str (:expected m)))
  (println "      actual:" (pr-str (:actual m)))
  (println))

#+cljs
(defmethod test/report [::cats :summary]
  [m]
  (println "\nRan" (:test m) "tests containing"
    (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors.")
  (when-not (success? m)
    (set! (.-exitCode js/process) 1)))

#+cljs
(defn main
  []
  (let [env (test/empty-env ::cats)]
    (test/run-tests env
                    'cats.core-spec
                    'cats.builtin-spec
                    'cats.monad.exception-spec
                    'cats.monad.identity-spec
                    'cats.monad.continuation-spec
                    'cats.monad.either-spec
                    'cats.monad.exception-spec
                    'cats.monad.maybe-spec
                    'cats.monad.identity-spec
                    'cats.monad.reader-spec
                    'cats.monad.writer-spec
                    'cats.monad.channel-spec)))

#+cljs
(set! *main-cli-fn* main)
