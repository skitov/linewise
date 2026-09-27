.PHONY: test

test:
	emacs -Q --batch -l test/linewise-test.el -f ert-run-tests-batch-and-exit
