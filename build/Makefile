MAIN = main.native
EXE  = kawa

all:
	cd ../src && ocamlbuild $(MAIN) && cd ../build && rm -rf $(MAIN) && mv -f ../src/$(MAIN) ./$(EXE)

build_tests:
	@for f in $(shell find . -name '*.kawa'); do echo ./$(EXE) $${f} && ./$(EXE) $${f}; done

clean:
	rm -rf ../src/_build && rm -rf $(EXE)
