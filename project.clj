(defproject funcool/cats "1.3.0"
  :description "Category Theory abstractions for Clojure"
  :url         "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url  "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "1.7.189" :scope "provided"]
                 [org.clojure/core.async "0.2.374" :scope "provided"]
                 [org.clojure/test.check "0.9.0" :scope "provided"]
                 [manifold "0.1.1" :scope "provided"]]
  :deploy-repositories {"releases"  :clojars
                        "snapshots" :clojars}
  :source-paths   ["src"]
  :test-paths     ["test"]
  :jar-exclusions [#"\.swp|\.swo|user\.clj"])
