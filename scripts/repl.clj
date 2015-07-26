(require
  '[cljs.repl :as repl]
  '[cljs.repl.nashorn :as nashorn])

(cljs.repl/repl
 (cljs.repl.nashorn/repl-env)
 :output-dir "out"
 :cache-analysis true)
