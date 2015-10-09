{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [lein-try "0.4.3"]
                  [lein-exec "0.3.5"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.10"]
                       [pjstadig/humane-test-output "0.7.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :repl-options
        {:init
         (do
           (set! *print-length* 100))}}}

;; [lein-describe "0.3.0-SNAPSHOT"]
;; [lein-difftest "2.0.0"]
;; [lein-pprint "1.1.1"]
;; [compojure/lein-template "0.3.0"]
;; [com.jakemccrary/lein-test-refresh "0.3.0"]
