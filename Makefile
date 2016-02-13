
.PHONY: all
all:
	cd compiler-service && cabal install

localrun:
	python -m SimpleHTTPServer 8001 &
	./compiler-service/dist/build/compiler-service/compiler-service


