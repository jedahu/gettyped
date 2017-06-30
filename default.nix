#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {} }:

let
  yarn = pkgs.yarn;
  nodejs = pkgs.nodejs;
  ghcWith = pkgs.haskellPackages.ghcWithPackages;
  mkHsBin = name: mainDir: inputs: pkgs.stdenv.mkDerivation {
    inherit name;
    src = mainDir;
    phases = "unpackPhase buildPhase";
    buildInputs = [(ghcWith inputs)];
    buildPhase = ''
      mkdir -p "$out/bin"
      ghc -O2 --make Main.hs -o "$out/bin/${name}";
    '';
  };
  module-extractor = mkHsBin "extract-modules" ./generator/modules (p: [p.pandoc]);
  module-filter = mkHsBin "module-filter" ./generator/module-filter (p: [p.pandoc]);
  monaco = pkgs.stdenv.mkDerivation {
    name = "monaco-editor";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/monaco-editor/-/monaco-editor-0.8.3.tgz";
      sha256 = "0hxsspv0czrk5s94scf32gwy0x5m1hvx2fm44rz3z7qwk8jf2fzs";
    };
    phases = "unpackPhase buildPhase";
    buildPhase = ''
      cp -r "min/vs" "$out"
    '';
  };
  node-deps = pkgs.stdenv.mkDerivation {
    name = "node-deps";
    src = ./package.json;
    sourceRoot = "srcroot";
    phases = "unpackPhase buildPhase";
    buildInputs = [yarn];
    unpackPhase = ''
      mkdir "$sourceRoot"
      cp "$src" "$sourceRoot/package.json"
    '';
    buildPhase = ''
      mkdir "$out"
      export HOME="$out/.yarn-home"
      yarn --modules-folder "$out"
    '';
  };
  page-html = path:
    let template = ./template/page.html;
    in pkgs.stdenv.mkDerivation {
    name = "gettyped-page-html";
    srcs = [path template];
    sourceRoot = "srcroot";
    phases = "unpackPhase buildPhase";
    buildInputs = [pkgs.pandoc module-filter];
    unpackPhase = ''
      mkdir "$sourceRoot"
      cp "${path}" "$sourceRoot/page.org"
      cp "${template}" "$sourceRoot/template.html"
    '';
    buildPhase = ''
      mkdir "$out"
      pandoc -f org -t html5 -o "$out/page.html" \
        --parse-raw \
        --no-highlight \
        --section-divs \
        --toc \
        --standalone \
        --template template.html \
        --filter module-filter \
        page.org
  '';
  };
  page-modules = path: pkgs.stdenv.mkDerivation {
    name = "gettyped-page-modules";
    src = path;
    sourceRoot = "srcroot";
    phases = "unpackPhase buildPhase";
    buildInputs = [module-extractor];
    unpackPhase = ''
      mkdir "$sourceRoot"
      cp "$src" "$sourceRoot/page.org"
    '';
    buildPhase = ''
      extract-modules page.org "$out"
    '';
  };
  gen-page = {path, absPath, ...}:
    let html = page-html absPath;
        modules = page-modules absPath;
    in pkgs.stdenv.mkDerivation {
      name = "gettyped-page";
      src = absPath;
      phases = "buildPhase";
      buildInputs = [html modules];
      buildPhase = ''
        dir="$out/${path}"
        mkdir -p "$dir"
        ln -s "${html}/page.html" "$dir/index.html"
        ln -s "${modules}" "$out/modules"
      '';
    };
  page = {name, path}: {
    inherit name path;
    absPath = ./. + ("/" + path + ".org");
  };
  pageList = map page [
    { name = "Maybe: null done properly";
      path = "doc/type/Maybe";
    }
  ];
  pages = map gen-page pageList;

in pkgs.stdenv.mkDerivation {
  name = "gettyped-site";
  src = ./.;
  phases = "unpackPhase buildPhase checkPhase";
  buildInputs = [monaco pkgs.rsync] ++ pages;
  checkInputs = [node-deps nodejs];
  doCheck = true;
  buildPhase = ''
    mkdir -p "$out/static"
    ln -s "${monaco}" "$out/static/vs"
    for p in ${builtins.concatStringsSep " " pages}
    do
      rsync -a "$p/" "$out/"
    done
  '';
  checkPhase = ''
    set -e
    export PATH="$PATH:${nodejs}/bin"
    cp -r "${node-deps}" node_modules
    rsync -aL "$out/modules/demo" .
    NODE_PATH=. "./node_modules/.bin/ts-node" -P test test/demo.ts
  '';
}
