{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Explore a GIT repository.";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      # supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        hgitview = final.haskellPackages.callCabal2nix "hgitview" ./. {};
      });
      packages = forAllSystems (system: {
         hgitview = nixpkgsFor.${system}.hgitview;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.hgitview);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.hgitview];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            # haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to hgitview\e[0m ***"
  export PS1='hgitview:\A \e[1;34m\w\e[0m '
        '';
        });
  };
}
