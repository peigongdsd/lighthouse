{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs }: let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      hsPackages = (x : with x; [ network aeson stm cereal containers ]);
    in
    {
      devShell = forAllSystems (system: let
        pkgs = import nixpkgs { inherit system; }; 
        ghc = pkgs.haskellPackages.ghcWithPackages hsPackages;
      in pkgs.mkShell {
          buildInputs = [ ghc pkgs.haskell-language-server ];
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
      });
      defaultPackage = forAllSystems (system: let
        pkgs = import nixpkgs { inherit system; }; 
        ghc = pkgs.haskellPackages.ghcWithPackages hsPackages;

      in pkgs.stdenv.mkDerivation { name = "site"; builder = "${pkgs.bash}/bin/bash"; args = [ "-c" ''  
          ${ghc}/bin/ghc -O1 -threaded -o $out ${./main.hs}
        '' ]; inherit system; });
    };
}
