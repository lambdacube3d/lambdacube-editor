
.PHONY: all
all:
	cd compiler-service && cabal install

editor:
	bower update
	pulp build -O --to editor.js

localrun:
	python -m SimpleHTTPServer 8001 &
	./compiler-service/dist/build/compiler-service/compiler-service


