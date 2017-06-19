with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    gitAndTools.git
    gnumake
    nodejs
    pandoc
    rsync
    yarn
  ];
  shellHook = ''
  '';
}
