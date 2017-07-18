(defproject funcool/cats "2.2.1"
  :description "Category Theory abstractions for Clojure"
  :url         "https://github.com/funcool/cats"
  :license {:name "BSD (2 Clause)"
            :url  "http://opensource.org/licenses/BSD-2-Clause"}
  
  :plugins [[s3-wagon-private "1.3.0"]]
  
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.89" :scope "provided"]
                 [org.clojure/core.async "0.3.442" :scope "provided"]
                 [org.clojure/test.check "0.9.0" :scope "provided"]
                 [org.clojure/core.match "0.3.0-alpha4" :scope "provided"]
                 [manifold "0.1.5" :scope "provided"]
                 [funcool/promesa "1.8.1" :scope "provided"]]
  :repositories  [["central"  {:url "http://repo1.maven.org/maven2/" :snapshots false}]
                  ["clojars"  {:url "https://clojars.org/repo/"}]
                  ["nu-maven" {:url "s3p://nu-maven/releases/"
                               :sign-releases false}]]
  :source-paths   ["src"]
  :test-paths     ["test"]
  :jar-exclusions [#"\.swp|\.swo|user\.clj"])
