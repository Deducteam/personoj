LP_SRC = $(shell find . -type f -name "*.lp")
LP_OBJ = $(LP_SRC:%.lp=%.lpo)

all: $(LP_OBJ)

$(LP_OBJ)&: $(LP_SRC)
	lambdapi check --gen-obj $^

install: $(LP_OBJ) lambdapi.pkg
	lambdapi install lambdapi.pkg $(LP_OBJ) $(LP_SRC)

.PHONY: uninstall
uninstall:
	lambdapi uninstall lambdapi.pkg

.PHONY: clean
clean:
	$(RM) -r $(LP_OBJ)

tests:
	@echo "Checking [core] prelude"
	lambdapi prelude/core/*.lp
	@echo "Checking [cert] prelude"
	lambdapi prelude/cert_f/*.lp
