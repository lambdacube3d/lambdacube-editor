# lambdacube-editor

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

* clone lambdacube-editor
  - cd lambdacube-editor
  - bower update
  - pulp build -O --to editor.js
  - cd compiler-service
  - cabal install
  - run compiler-service
    - cd; mkdir lc_tmp; cd lc_tmp
    - cp Prelude.lc .
    - compiler-service
  - open lambdacube-editor/editor.html


