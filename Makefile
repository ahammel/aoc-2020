repl/nrepl repl/rebl test/unit test/lint:
	clj -M:$@

test:
	clj -M:test/unit
	clj -M:test/lint
