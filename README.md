# lambdacube-editor

* install purescript
  - cabal update
  - cabal install purescript

* install node
* install npm
* install bower and pulp
  - npm install -g bower pulp

* clone lambdacube-editor
  - bower update
  - pulp build -O --to index.js
  - cd compiler-service
  - cabal install
  - run compiler-service, put prelude into current dir
  - open ../index.html
