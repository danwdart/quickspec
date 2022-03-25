{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/haskell-updates.tar.gz") {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {},
  compiler ? "ghc922"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      quickspec = lib.doJailbreak (self.callCabal2nix "quickspec" (gitignore ./.) {});
      twee-lib = lib.doJailbreak (self.callCabal2nixWithOptions "twee-lib" (builtins.fetchGit {
        url = "https://github.com/danwdart/twee.git";
        rev = "54f9a2231e5df6a3fa2bc93ae22ad99a1387643b";
      }) "--subpath src" {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.quickspec
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools;
    withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.quickspec);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  quickspec = myHaskellPackages.quickspec;
}

