{:user {:plugins [[cider/cider-nrepl "0.12.0"]
                  [lein-try "0.4.3"]
                  [lein-exec "0.3.6"]
                  [lein-pprint "1.1.2"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [pjstadig/humane-test-output "0.7.1"]]
        :figwheel {:nrepl-middleware ["cider.nrepl/cider-middleware"]}
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)
                     (use 'clojure.repl)
                     (use 'clojure.java.javadoc)
                     (use 'clojure.pprint)]
        :global-vars {*print-length* 100}}}


;; [lein-describe "0.3.0-SNAPSHOT"]
;; [com.jakemccrary/lein-test-refresh "0.3.0"]
