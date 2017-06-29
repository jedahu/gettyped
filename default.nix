#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {} }:

let
  yarn = pkgs.yarn;
  nodejs = pkgs.nodejs;
  site-generator = pkgs.stdenv.mkDerivation {
    name = "gettyped-site-generator";
    src = ./generator/site;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (p: with p; [hakyll]))
    ];
    buildPhase = ''
      mkdir -p "$out/bin"
      ghc -O2 --make Main.hs -o "$out/bin/generate-site"
    '';
  };
  modules-generator = pkgs.stdenv.mkDerivation {
    name = "gettyped-modules-generator";
    src = ./generator/modules;
    phases = "unpackPhase buildPhase";
    buildInputs = [
      (pkgs.haskellPackages.ghcWithPackages (p: with p; [pandoc]))
    ];
    buildPhase = ''
      mkdir -p "$out/bin"
      ghc -O2 --make Main.hs -o "$out/bin/generate-modules"
    '';
  };
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

in pkgs.stdenv.mkDerivation {
  name = "gettyped-site";
  src = ./.;
  phases = "unpackPhase buildPhase checkPhase";
  buildInputs = [site-generator monaco modules-generator node-deps nodejs yarn];
  checkInputs = [modules-generator node-deps nodejs];
  doCheck = true;
  buildPhase = ''
    generate-site build
    mkdir "$out"
    cp -r _site/* "$out"
    cp -r "${monaco}" "$out/vs"
  '';
  checkPhase = ''
    export PATH="$PATH:${nodejs}/bin"
    "${modules-generator}/bin/generate-modules" .
    cp -r "${node-deps}" node_modules
    NODE_PATH=. "./node_modules/.bin/ts-node" -P test test/demo.ts
  '';
}
