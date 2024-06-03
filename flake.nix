{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "view Vega/Vega-Lite visualizations in a web browser";
  
  inputs = {
    nixpkgs.url = "nixpkgs";
  };
  
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      # supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        vega-view = final.haskellPackages.callCabal2nix "vega-view" ./. {};
      });
      packages = forAllSystems (system: {
         vega-view = nixpkgsFor.${system}.vega-view;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.vega-view);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.vega-view];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to vega-view development\e[0m ***"
  ghc --version
  cabal --version
  hlint --version
  echo -e ""
  export PS1='vega-view:\A \e[1;34m\w\e[0m '
	'';
        });
  };
}
