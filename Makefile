
.PHONY: all
all:
	cd compiler-service && cabal install

localrun:
	./compiler-service/dist/build/compiler-service/compiler-service


