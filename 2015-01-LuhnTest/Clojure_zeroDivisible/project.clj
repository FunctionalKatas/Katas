(defproject luhn-check "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]]
  :main ^:skip-aot luhn-check.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]]
                   :plugins [[lein-midje "3.1.3"]
                             [cider/cider-nrepl "0.9.0-SNAPSHOT"]]
                   :env {:squiggly {:checkers [:eastwood]
                                      :eastwood-exclude-linters [:unlimited-use]}}}})

