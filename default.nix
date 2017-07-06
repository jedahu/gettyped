#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with builtins;
rec {
  inherit yarn nodejs pandoc;
  site-root = "/gettyped";
  main-js = "main.js";
  scrollbar-size = "7";
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
  require-js = pkgs.fetchurl {
    url = "http://requirejs.org/docs/release/2.3.3/minified/require.js";
    sha256 = "1bxc9bcyl88bbil1vcgvxc2npfcz7xx96xmrbsx0dq7mx1yrp90c";
  };
  node-deps = pkgs.stdenv.mkDerivation {
    name = "node-deps";
    srcs = [./package.json ./yarn.lock];
    sourceRoot = "srcroot";
    phases = "unpackPhase buildPhase";
    buildInputs = [yarn];
    unpackPhase = ''
      mkdir "$sourceRoot"
      ln -s "${./package.json}" "$sourceRoot/package.json"
      ln -s "${./yarn.lock}" "$sourceRoot/yarn.lock"
    '';
    buildPhase = ''
      mkdir "$out"
      export HOME="$out/.yarn-home"
      yarn --pure-lockfile --modules-folder "$out"
    '';
  };
  compile-js = pkgs.stdenv.mkDerivation rec {
    name = "compile-js";
    srcs = [./ts ./tsconfig-base.json];
    buildInputs = [nodejs];
    phases = "unpackPhase buildPhase";
    sourceRoot = "srcroot";
    unpackPhase = ''
      mkdir "$sourceRoot"
      ln -s "${./ts}" "$sourceRoot/ts"
      ln -s "${./tsconfig-base.json}" "$sourceRoot/tsconfig-base.json"
      cp -r "${node-deps}" "$sourceRoot/node_modules"
    '';
    injected-js = writeTextFile {
      name = "injected.js";
      text = ''
        (function() {
          window.__gt = {
            tsconfig: ${readFile ./tsconfig-base.json},
            siteRoot: "${site-root}",
            scrollbarSize: ${scrollbar-size}
          };
        })();
      '';
    };
    buildPhase = ''
      mkdir "$out"
      outjs="$out/${main-js}"
      "${node-deps}/.bin/tsc" --project "./ts/tsconfig.json"
      cat "${injected-js}" \
        "built.js" \
        >"$outjs"
    '';
  };
  page-html = path: absPath:
    let template = ./template/page.html;
    in pkgs.stdenv.mkDerivation {
    name = "gettyped-page-html";
    phases = "buildPhase";
    buildInputs = [pandoc module-filter];
    buildPhase = ''
      mkdir -p "$out/${path}"
      pandoc -f org -t html5 -o "$out/${path}/index.html" \
        -V site-root=${site-root} \
        -V main-js=${main-js} \
        --parse-raw \
        --no-highlight \
        --section-divs \
        --toc \
        --standalone \
        --template "${template}" \
        --filter module-filter \
        "${absPath}"
  '';
  };
  page-modules = absPath: pkgs.stdenv.mkDerivation {
    name = "gettyped-page-modules";
    src = absPath;
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
  gen-page = {path, absPath, ...}: {
    html = page-html path absPath;
    modules = page-modules absPath;
  };
  page = {name, path}: {
    inherit name path;
    absPath = ./. + ("/" + path + "/index.org");
  };
  pageList = map page [
    { name = "Maybe: null done properly";
      path = "doc/type/Maybe";
    }
  ];
  pages = map gen-page pageList;
  demo-outfile = pkgs.stdenv.mkDerivation rec {
    name = "demo.js";
    phases = "unpackPhase buildPhase";
    sourceRoot = "srcroot";
    modules = map (x: x.modules) pages;
    buildInputs = [nodejs rsync];
    tsconfig = writeTextFile {
      name = "tsconfig.json";
      text = ''
        {
          "include": ["./**/*.ts"],
          "compilerOptions": {
            "module": "system"
          }
        }
      '';
    };
    unpackPhase = ''
      mkdir "$sourceRoot"
      ln -s "${tsconfig}" "$sourceRoot/tsconfig.json"
      for m in ${concatStringsSep " " modules}
      do
        rsync -aL "$m/" "$sourceRoot/"
      done
    '';
    buildPhase = ''
      "${node-deps}/.bin/tsc" -p . --outFile "$out" >/dev/null || {
        if [[ $? = 1 ]]
        then
          echo 'No code generated (check diagnostics)'
          exit 1
        fi
      }
    '';
  };
  site = pkgs.stdenv.mkDerivation rec {
    name = "gettyped-site";
    src = ./.;
    phases = "unpackPhase buildPhase checkPhase";
    buildInputs = [rsync];
    checkInputs = [nodejs];
    doCheck = true;
    htmls = map (x: x.html) pages;
    modules = map (x: x.modules) pages;
    buildPhase = ''
      mkdir -p "$out/modules"
      ln -s "${./static}" "$out/static"
      ln -s "${./css/main.css}" "$out/main.css"
      ln -s "${node-deps}/typescript/lib/lib.es6.d.ts" "$out/lib.es6.d.ts"
      ln -s "${compile-js}/${main-js}" "$out/${main-js}"
      for p in ${concatStringsSep " " htmls}
      do
        rsync -aL "$p/" "$out/"
      done
      for m in ${concatStringsSep " " modules}
      do
        rsync -aL "$m/" "$out/modules/"
      done
    '';
    checkPhase = ''
      export PATH="$PATH:${nodejs}/bin"
      cp -r "${node-deps}" node_modules
      rsync -aL "$out/modules/demo" .
      NODE_PATH=. \
        "./node_modules/.bin/ts-node" \
        -P test \
        test/demo.ts
    '';
  };
}
