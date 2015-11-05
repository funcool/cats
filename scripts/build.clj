(require '[cljs.build.api :as b])

(println "Building ...")

(let [start (System/nanoTime)]
  (b/build
   (b/inputs "test" "src")
   {:main 'cats.runner
    :output-to "out/tests.js"
    :output-dir "out"
    :optimizations :advanced
    :target :nodejs
    :verbose true})
  (println "... done. Elapsed" (/ (- (System/nanoTime) start) 1e9) "seconds"))
