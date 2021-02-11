{
  inputs = {
    elm-install.url = "github:ursi/elm-install";
    node-packages.url = "github:ursi/nix-node-packages";
  };

  outputs = { self, nixpkgs, utils, elm-install, node-packages }:
    utils.mkShell
      ({ pkgs, system }: with pkgs;
        {
          buildInputs = [
            elmPackages.elm
            node-packages.packages.${system}.elm-git-install
            nodejs
            nodePackages.parcel-bundler
            elm-install.defaultPackage.${system}
          ];
        }
      )
      nixpkgs;
}
