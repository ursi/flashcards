{ inputs =
    { dream2nix.url = "github:nix-community/dream2nix";

      elm-git-install =
        { flake = false;
          url = "github:robinheghan/elm-git-install";
        };

      elm-install.url = "github:ursi/elm-install";
      make-shell.url = "github:ursi/nix-make-shell/1";
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      utils.url = "github:ursi/flake-utils/8";
    };

  outputs = { dream2nix, utils, ... }@inputs:
    utils.apply-systems { inherit inputs; }
      ({ elm-install, make-shell, pkgs, system, ... }:
         let
           p = pkgs;

           d2n =
             dream2nix.lib.makeFlakeOutputs
               { systems = [ system ];
                 config.projectRoot = ./.;
                 source = inputs.elm-git-install;
               };

           inherit (d2n.packages.${system}) elm-git-install;
         in
         { apps.default =
             { type = "app";

               program =
                 (p.writeShellScript "serve"
                    "${p.nodePackages.parcel-bundler}/bin/parcel serve index.html"
                 ).outPath;
             };

           devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ elm-git-install
                     elm-install
                     elmPackages.elm
                     gcc
                     gnumake
                     nodejs
                     nodePackages.parcel-bundler
                     python3
                   ];

                 aliases =
                   { build = "elm-git-install && npm i";
                     serve = "parcel serve index.html;";
                   };
               };
         }
      );
}
