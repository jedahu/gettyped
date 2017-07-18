#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {}, config-path ? ./config.json }:

with pkgs;
with builtins;
rec {
  inherit yarn nodejs pandoc rsync;
  main-js = "main.js";
  main-css = "main.css";
  ghcWith = pkgs.haskellPackages.ghcWithPackages;
  config-json = readFile config-path;
  config = fromJSON config-json;
  without-store = path:
    concatStringsSep "/" (lib.drop 3 (lib.splitString "/" path));
  unpack-tree = paths: runCommand "unpack-tree" {}
    ( ''mkdir -p "$out"
      ''
      +
      ( concatStringsSep "\n"
        ( map (p:
            let
              opName = elemAt p 0;
              from = elemAt p 1;
              to = elemAt p 2;
              ops = {cp = "cp -r"; ln = "ln -s"; rsync = "${rsync}/bin/rsync -a";};
              op = getAttr opName ops;
            in
              ''mkdir -p "$(dirname "$out/${to}")"
                ${op} "${from}" "$out/${to}"
              ''
          ) paths
        )
      )
    );
  mkHsBin = path: inputs: pkgs.stdenv.mkDerivation rec {
    name = baseNameOf path;
    buildInputs = [(ghcWith inputs)];
    src = unpack-tree [["cp" path "hs/${name}"]];
    phases = "unpackPhase buildPhase";
    buildPhase = ''
      mkdir -p "$out/bin"
      mkdir -p "tmp"
      exec ghc -O2 \
        --make "hs/${name}/Main.hs" \
        -odir "./tmp" \
        -hidir "./tmp" \
        -o "$out/bin/${name}"
    '';
  };
  module-extractor = mkHsBin ./hs/extract-modules (p: [p.pandoc]);
  # module-filter = mkHsBin "module-filter" ./generator/module-filter (p: [p.pandoc]);
  html-filter = mkHsBin ./hs/html-filter (p: [p.pandoc p.tagsoup]);
  node-deps = pkgs.stdenv.mkDerivation rec {
    name = "node-deps";
    src = unpack-tree [
      ["cp" ./package.json "package.json"]
      ["cp" ./yarn.lock "yarn.lock"]
    ];
    phases = "unpackPhase buildPhase";
    buildInputs = [yarn];
    buildPhase = ''
      mkdir "$out"
      export HOME="$out/.yarn-home"
      exec yarn --pure-lockfile --modules-folder "$out/node_modules"
    '';
  };
  config-js = writeTextFile {
    name = "config.js";
    text = ''
      (function() {
        window.__gt = {
          tsconfig: ${readFile ./tsconfig-base.json},
          config: ${config-json}
        }
      }());
    '';
  };
  compile-js = pkgs.stdenv.mkDerivation rec {
    inherit node-deps;
    name = "compile-js";
    src = unpack-tree [
      ["cp" ./ts "ts"]
      ["cp" ./js "js"]
      ["cp" ./css "css"]
      ["ln" ./tsconfig-base.json "tsconfig-base.json"]
      ["ln" ./webpack.config.ts "webpack.config.ts"]
      ["cp" (node-deps + "/node_modules") "node_modules"]
    ];
    buildInputs = [nodejs];
    phases = "unpackPhase buildPhase";
    buildPhase = ''
      mkdir "$out"

      export NODE_PATH="./node_modules"
      "./node_modules/.bin/webpack" \
          --config ./webpack.config.ts \
          --output-path . \
          || exit 1
      cat "${config-js}" \
          "node_modules/requirejs/require.js" \
          "js/page-config.js" \
          "main.js" \
          >"$out/${main-js}" \
          || exit 1
      cp "main.css" "$out/${main-css}"
    '';
  };
  page-html = path: absPath: pkgs.stdenv.mkDerivation rec {
    name = "gettyped-page-html";
    phases = "buildPhase";
    buildInputs = [pandoc html-filter];
    page-ns = baseNameOf (dirOf absPath);
    buildPhase = ''
      mkdir -p "$out${path}"
      pandoc -f org -t html5 --smart \
        -V site-root=${config.siteRoot} \
        -V main-js=${main-js} \
        -V page-ns=${page-ns} \
        ${if page-ns == "doc" then "" else ''\
          -V is-post=1 \
          --toc \
        ''} \
        --parse-raw \
        --no-highlight \
        --section-divs \
        --standalone \
        --template "${./template/page.html}" \
        "${absPath}" \
        | html-filter >"$out${path}/index.html"
  '';
  };
  page-modules = absPath: pkgs.stdenv.mkDerivation {
    name = "gettyped-page-modules";
    src = unpack-tree [
      ["cp" absPath "page.org"]
      ["cp" ./tsconfig-base.json "tsconfig-base.json"]
      ["cp" ./tsconfig.json "tsconfig.json"]
      ["cp" ./test "test"]
      ["cp" ./ts/gt-lib-shared.ts "ts/gt-lib-shared.ts"]
      ["cp" ./ts/d/lib.gt.d.ts "ts/d/lib.gt.d.ts"]
    ];
    phases = "unpackPhase buildPhase checkPhase";
    buildInputs = [module-extractor];
    checkInputs = [nodejs rsync];
    buildPhase = ''
      dest="$out/modules/${baseNameOf (dirOf absPath)}"
      mkdir -p "$dest"
      extract-modules page.org "$dest"
    '';
    doCheck = true;
    checkPhase = ''
      if [[ -n $(ls -A "$out") ]]
      then
        export PATH="$PATH:${nodejs}/bin"
        "${rsync}/bin/rsync" -aL "$out/modules/" modules/
        cp -r "${node-deps}/node_modules" node_modules
        NODE_PATH=.:./modules:./ts \
          "./node_modules/.bin/ts-node" \
          -P test \
          test/demo.ts
      fi
    '';
  };
  gen-page = {path, absPath, ...}: {
    html = page-html path absPath;
    modules = page-modules absPath;
  };
  page = orgPath: {
    path = lib.removeSuffix "/index.org" ("/" + orgPath);
    absPath = ./. + ("/doc/" + orgPath);
  };
  page-paths = runCommand "page-paths" {src = ./doc;} ''
    shopt -s globstar
    (cd "$src" && ls -1 **/index.org) >"$out"
  '';
  pageList = map page (filter (p: p != "") (lib.splitString "\n" (readFile page-paths)));
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
      "${node-deps}/node_modules/.bin/tsc" -p . --outFile "$out" >/dev/null || {
        if [[ $? = 1 ]]
        then
          echo 'No code generated (check diagnostics)'
          exit 1
        fi
      }
    '';
  };
  libs-d-ts = writeTextFile {
    name = "libs.d.ts";
    text = lib.concatMapStrings readFile [
      "${node-deps}/node_modules/typescript/lib/lib.es6.d.ts"
      "${node-deps}/node_modules/typescript/lib/lib.es2016.full.d.ts"
      "${node-deps}/node_modules/typescript/lib/lib.es2017.object.d.ts"
      ./ts/d/lib.gt.d.ts
    ];
  };
  site = pkgs.stdenv.mkDerivation rec {
    name = "gettyped-site";
    src = ./.;
    phases = "unpackPhase buildPhase";
    buildInputs = [rsync];
    htmls = map (x: x.html) pages;
    modules = map (x: x.modules) pages;
    buildPhase = ''
      mkdir -p "$out/modules"
      ln -s "${./static}" "$out/static"
      ln -s "${libs-d-ts}" "$out/libs.d.ts"
      ln -s "${compile-js}/${main-js}" "$out/${main-js}"
      ln -s "${compile-js}/${main-css}" "$out/${main-css}"
      for p in ${concatStringsSep " " htmls}
      do
        rsync -aL "$p/" "$out/"
      done
      for m in ${concatStringsSep " " modules}
      do
        rsync -aL "$m/modules/" "$out/modules/"
      done
    '';
  };
}
