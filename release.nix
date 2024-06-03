{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc96"
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "vega-view" =
        hself.callCabal2nix "vega-view" (gitignore ./.) {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."vega-view"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."vega-view");

  # docker = pkgs.dockerTools.buildImage {
  #   name = "{{cookiecutter.project_name}}";
  #   config.Cmd = [ "${exe}/bin/{{cookiecutter.project_name}}" ];
  # };
in
{
  inherit shell;
  inherit exe;
  # inherit docker;
  inherit myHaskellPackages;
  "vega-view" = myHaskellPackages."vega-view";
}
