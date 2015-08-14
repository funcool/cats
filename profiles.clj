{:dev
 {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                 [criterium "0.4.3"]]
  :source-paths ["dev"]
  :codeina {:sources ["src"]
            :reader :clojure
            :target "doc/dist/latest/api"
            :src-uri "http://github.com/funcool/cats/blob/master/"
            :src-uri-prefix "#L"}
  :plugins [[funcool/codeina "0.3.0-SNAPSHOT"]
            [lein-ancient "0.6.7" :exclusions [org.clojure/tools.reader]]]}
 :bench
 [:dev
  {:main ^:skip-aot benchmarks
   :jvm-opts ^:replace []}]}
