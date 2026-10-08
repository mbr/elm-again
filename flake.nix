{
  description = "Pure retry schedules and attempt limits for Elm";

  inputs.nixpkgs.url = "nixpkgs/nixos-26.05";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      eachSystem = nixpkgs.lib.genAttrs systems;
      pkgsFor = eachSystem (system: nixpkgs.legacyPackages.${system});
      packageFor = eachSystem (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        pkgs.stdenvNoCC.mkDerivation {
          pname = "elm-again";
          version = (builtins.fromJSON (builtins.readFile ./elm.json)).version;

          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./check.sh
              ./elm-srcs.nix
              ./elm.json
              ./flake.nix
              ./format.sh
              ./LICENSE
              ./README.md
              ./registry.dat
              ./src
              ./tests
            ];
          };

          nativeBuildInputs = with pkgs; [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-test
            nixfmt
          ];

          postConfigure = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./elm-srcs.nix;
            elmVersion = pkgs.elmPackages.elm.version;
            registryDat = ./registry.dat;
          };

          buildPhase = ''
            runHook preBuild
            ./check.sh
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp elm.json README.md LICENSE elm-stuff/docs.json $out/
            cp -r src $out/
            runHook postInstall
          '';
        }
      );
    in
    {
      packages = eachSystem (system: {
        default = packageFor.${system};
      });

      checks = eachSystem (system: {
        default = packageFor.${system};
      });

      devShells = eachSystem (system: {
        default = pkgsFor.${system}.mkShell {
          inputsFrom = [ packageFor.${system} ];
          packages = [ pkgsFor.${system}.elm2nix ];
        };
      });

      formatter = eachSystem (system: pkgsFor.${system}.nixfmt);
    };
}
