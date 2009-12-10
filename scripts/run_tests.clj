(require '[clojure.contrib.test-is :as test]
         'com.trottercashion.bert-clj.bert-test
         'com.trottercashion.bert-clj.etf-encoder-test
         'com.trottercashion.bert-clj.bert-encoder-test
         'com.trottercashion.bert-clj.etf-decoder-test)

(test/run-all-tests)
