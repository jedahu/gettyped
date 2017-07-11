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
  # module-filter = mkHsBin "module-filter" ./generator/module-filter (p: [p.pandoc]);
  html-filter = mkHsBin "html-filter" ./generator/html-filter (p: [p.pandoc p.tagsoup]);
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
    buildInputs = [pandoc html-filter];
    buildPhase = ''
      mkdir -p "$out${path}"
      pandoc -f org -t html5 \
        -V site-root=${site-root} \
        -V main-js=${main-js} \
        --parse-raw \
        --no-highlight \
        --section-divs \
        --toc \
        --standalone \
        --template "${template}" \
        "${absPath}" \
        | html-filter >"$out${path}/index.html"
  '';
  };
  page-modules = absPath: pkgs.stdenv.mkDerivation {
    name = "gettyped-page-modules";
    sourceRoot = "srcroot";
    phases = "unpackPhase buildPhase checkPhase";
    buildInputs = [module-extractor];
    checkInputs = [nodejs rsync];
    unpackPhase = ''
      mkdir "$sourceRoot"
      echo absPath ${absPath}
      ls ${absPath}
      cp "${absPath}" "$sourceRoot/page.org"
      cp "${./tsconfig-base.json}" "$sourceRoot/tsconfig-base.json"
      cp "${./tsconfig.json}" "$sourceRoot/tsconfig.json"
      cp -r "${./test}" "$sourceRoot/test"
      cp -r "${./ts}" "$sourceRoot/ts"
    '';
    buildPhase = ''
      mkdir "$out"
      extract-modules page.org "$out"
    '';
    doCheck = true;
    checkPhase = ''
      if [[ -n $(ls -A "$out") ]]
      then
        export PATH="$PATH:${nodejs}/bin"
        "${rsync}/bin/rsync" -aL "$out/" modules/
        cp -r "${node-deps}" node_modules
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
      "${node-deps}/.bin/tsc" -p . --outFile "$out" >/dev/null || {
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
      "${node-deps}/typescript/lib/lib.es2016.full.d.ts"
      "${node-deps}/typescript/lib/lib.es2017.object.d.ts"
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
      ln -s "${./css/main.css}" "$out/main.css"
      ln -s "${libs-d-ts}" "$out/libs.d.ts"
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
  };
}
