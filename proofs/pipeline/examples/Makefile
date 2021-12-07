all: test
	
run.out: run.sh
	sh run.sh > run.out

test: run.out run.expected
	@diff run.expected run.out

promote:
	cp run.out run.expected

clean: run.out

.PHONY: test diff promote clean all

