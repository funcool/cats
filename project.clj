(defproject funcool/cats "2.4.1"
  :description "Category Theory abstractions for Clojure"
  :url         "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url  "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.439" :scope "provided"]
                 [org.clojure/core.async "0.4.474" :scope "provided"]
                 [org.clojure/test.check "0.9.0" :scope "provided"]
                 [org.clojure/core.match "0.3.0-alpha4" :scope "provided"]
                 [manifold "0.1.6" :scope "provided"]
                 [funcool/promesa "5.1.0" :scope "provided"]]
  :deploy-repositories {"releases"  :clojars
                        "snapshots" :clojars}
  :source-paths   ["src"]
  :test-paths     ["test"]
  :jar-exclusions [#"\.swp|\.swo|user\.clj"])
