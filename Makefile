repl/nrepl repl/rebl test/unit test/lint:
	clj -M:$@

test/all:
	clj -M:$@ --reporter kaocha.report/documentation

test:
	clj -M:test/unit
	clj -M:test/lint
