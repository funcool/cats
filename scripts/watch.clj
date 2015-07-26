(require '[cljs.build.api :as b])

(b/watch (b/inputs "test" "src")
  {:main 'cats.runner
   :target :nodejs
   :output-to "tests.js"
   :output-dir "out"
   :static-fns true
   :verbose true})
