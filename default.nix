#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {}, config-path ? ./config.json }:

with pkgs;
with builtins;
rec {
  inherit (xorg) lndir;
  main-js = "main.js";
  main-css = "main.css";
  ghcWith = pkgs.haskellPackages.ghcWithPackages;
  config-json = readFile config-path;
  config = fromJSON config-json;
  without-store = path:
    concatStringsSep "/" (lib.drop 3 (lib.splitString "/" path));
  unpack-tree = {echo ? false, cmd ? ""}: paths: runCommand "unpack-tree" {}
    ( ''mkdir -p "$out"
      ''
      +
      ( concatStringsSep "\n"
        ( map (item:
            let
              entry = if typeOf item == "list" then item else [item];
              p = if cmd == "" then entry else [cmd] ++ entry;
              opName = elemAt p 0;
              from = elemAt p 1;
              to = if length p < 3 then "$out" else "$out/${elemAt p 2}";
              ops = {
                cp = "cp -r";
                ln = "ln -s";
                lndir = "${lndir}/bin/lndir -silent";
                rsync = "${rsync}/bin/rsync -a";
                rsyncL = "${rsync}/bin/rsync -aL";
              };
              op = getAttr opName ops;
              echo_ = ''
                echo ${opName} "${from}" "${to}"
              '';
              run = ''
                mkdir -p "$(dirname "${to}")"
                ${op} "${from}" "${to}"
              '';
            in
              if echo
              then echo_ + run
              else run
          ) paths
        )
      )
    );
  mkHsBin = path: inputs: pkgs.stdenv.mkDerivation rec {
    name = baseNameOf path;
    buildInputs = [(ghcWith inputs)];
    src = path;
    phases = "unpackPhase buildPhase";
    buildPhase = ''
      mkdir -p "$out/bin"
      mkdir -p "tmp"
      exec ghc -O2 \
        --make "Main.hs" \
        -odir "./tmp" \
        -hidir "./tmp" \
        -o "$out/bin/${name}"
    '';
  };
  module-extractor = mkHsBin ./cmd/extract-modules (p: [p.pandoc p.witherable]);
  html-filter = mkHsBin ./cmd/html-filter (p: [p.pandoc p.tagsoup]);
  node-deps = pkgs.stdenv.mkDerivation rec {
    name = "node-deps";
    src = unpack-tree {} [
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
  compile-css = writeTextFile {
    name = "main.css";
    destination = "/main.css";
    text = concatStringsSep "\n" (map readFile [
      ./css/main.css
      (node-deps + "/node_modules/vex-js/dist/css/vex.css")
      (node-deps + "/node_modules/vex-js/dist/css/vex-theme-plain.css")
    ]);
  };
  compile-js = pkgs.stdenv.mkDerivation rec {
    name = "compile-js";
    src = unpack-tree {} [
      ["cp" ./ts "ts"]
      ["cp" ./js "js"]
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
    '';
  };
  compress-js = runCommand "compress-js" {buildInputs = [nodejs];} ''
    mkdir "$out"

    export NODE_PATH="${node-deps}/node_modules";
    "${node-deps}/node_modules/.bin/uglifyjs" \
      "${compile-js}/${main-js}" \
      -m -c -o "$out/${main-js}"
  '';
  page-html = path: absPath: pkgs.stdenv.mkDerivation rec {
    name = "gettyped-page-html";
    phases = "buildPhase";
    buildInputs = [pandoc html-filter];
    page-name = baseNameOf (dirOf absPath);
    page-cwd = if page-name == "doc" then "/" else "/" + page-name;
    buildPhase = ''
      mkdir -p "$out${path}"
      pandoc -f org -t html5 --smart \
        -V site-root=${config.siteRoot} \
        -V main-js=${main-js} \
        -V page-cwd=${page-cwd} \
        ${if page-name == "doc" then "" else ''\
          -V is-post=1 \
          --toc \
        ''} \
        --parse-raw \
        --no-highlight \
        --section-divs \
        --standalone \
        --template "${./template/page.html}" \
        "${absPath}" \
        | html-filter \
        >"$out${path}/index.html"
  '';
  };
  page-modules = absPath: pkgs.stdenv.mkDerivation {
    name = "gettyped-page-modules";
    src = unpack-tree {} [
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
    destination = "/libs.d.ts";
    text = lib.concatMapStrings readFile [
      "${node-deps}/node_modules/typescript/lib/lib.es6.d.ts"
      "${node-deps}/node_modules/typescript/lib/lib.es2016.full.d.ts"
      "${node-deps}/node_modules/typescript/lib/lib.es2017.object.d.ts"
      ./ts/d/lib.gt.d.ts
    ];
  };
  site = prod: pkgs.stdenv.mkDerivation rec {
    name = "gettyped-site";
    phases = "unpackPhase buildPhase";
    htmls = map (x: x.html) pages;
    modules = map (x: x.modules) pages;
    files = [
      ./static
      libs-d-ts
      compile-css
      (if prod then compress-js else compile-js)
    ];
    src = unpack-tree
      {cmd = "rsyncL"; echo = true;}
      (map (x: "${x}/") (files ++ htmls ++ modules));
    buildPhase = ''
      ln -s "${src}" "$out"
    '';
    # buildPhase = ''
    #   mkdir "$out"
    #   cp -r ${./static} "$out/"
    #   cp -r ${libs-d-ts} "$out/"
    #   cp -r ${compile-js} "$out/"
    #   cp -r ${compile-css} "$out/"
    # '';
    # buildPhase = ''
    #   mkdir -p "$out/modules"
    #   ln -s "${./static}" "$out/static"
    #   ln -s "${libs-d-ts}" "$out/libs.d.ts"
    #   ln -s "${compile-js}/${main-js}" "$out/${main-js}"
    #   lndir -silent "${compile-css}" "$out"
    #   for p in ${concatStringsSep " " htmls}
    #   do
    #     rsync -aL "$p/" "$out/"
    #   done
    #   for m in ${concatStringsSep " " modules}
    #   do
    #     rsync -aL "$m/modules/" "$out/modules/"
    #   done
    # '';
  };
  main = site false;
  production = site true;
}
