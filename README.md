# lambdacube-editor


## run lambdacube-editor locally
  - cd compiler-service
  - cabal install or cabal new-build
  - run compiler-service from lambdacube-editor folder
  - open lambdacube-editor/editor.html
      1. open in browser
      2. start a local web server e.g. python -m SimpleHTTPServer 8002, then open 0.0.0.0:8002/editor.html in a browser


## setup build environment

* install purescript
  - cabal update
  - cabal install purescript
* install node
  Notes for Ubuntu users: you may need nodejs-legacy (`sudo apt-get install nodejs-legacy`)
* install npm
* install bower and pulp
  - npm install -g bower pulp
  or if you don't want to install bower and pulp globally
  - cd; npm install bower pulp
  - add "~/node_modules/.bin"  to $PATH

on NixOS:
    nix-env -iA nixos.pkgs.nodejs nixos.pkgs.haskellPackages.purescript nixos.pkgs.nodePackages.bower
    npm update
    (cd; npm install pulp)
    bower update
- add "~/node_modules/.bin"  to $PATH


## compile lambdacube-editor (editor.js)
  - cd lambdacube-editor
  - bower update
  - pulp build -O --to editor.js
