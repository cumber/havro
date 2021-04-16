{
  description = "hAvro - Haskell implementation of Avro file format";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };


  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let projectName = "havro";
          compiler-nix-name = "ghc8104";
          index-state = "2021-04-14T00:00:00Z";

          mkProject = haskell-nix: haskell-nix.cabalProject' {
            src = ./.;
            inherit index-state compiler-nix-name;

            plan-sha256 = "0a3qkjdmi7ldlwz01fa3iq0v8i72nxqz6a4hi9q485f7fbbvnvkf";
            materialized = ./materializations + "/${projectName}";
          };

          overlays = [
            haskellNix.overlay
            (import ./fix-hls.nix)
            (self: super: { ${projectName} = mkProject self.haskell-nix; })
          ];

          pkgs = import nixpkgs { inherit system overlays; };
          project = pkgs.${projectName};
          flake = pkgs.${projectName}.flake {};

          exe = project.getComponent "${projectName}:exe:${projectName}";

          tools = {
            haskell-language-server = {
              inherit index-state;
              plan-sha256 = "0p2y2k1qpsrsfa0gay2hjmfp2wh9a9vahjh7mwgkg2pnvm8ik3l1";
              materialized = ./materializations/haskell-language-server;
            };

            hoogle = {
              inherit index-state;
              plan-sha256 = "06h1dnkishrpg4k6gzk1760cfa8bm23766d8vjhl944qbz9wxi66";
              materialized = ./materializations/hoogle;
            };
          };

          devShell = project.shellFor {
            packages = ps: [ ps.${projectName} ];
            exactDeps = true;
            inherit tools;
          };

      in flake // {
        defaultPackage = exe;

        inherit overlays devShell;
        nixpkgs = pkgs;

        packages = flake.packages // {
          gcroot = pkgs.linkFarmFromDrvs "${projectName}-shell-gcroot" [
            devShell
            devShell.stdenv
            project.plan-nix
            project.roots

            (
              let compose = f: g: x: f (g x);
                  flakePaths = compose pkgs.lib.attrValues (
                    pkgs.lib.mapAttrs
                      (name: flake: { name = name; path = flake.outPath; })
                  );
              in  pkgs.linkFarm "input-flakes" (flakePaths self.inputs)
            )

            (
              let getMaterializers = ( name: project:
                    pkgs.linkFarmFromDrvs "${name}" [
                      project.plan-nix.passthru.calculateMaterializedSha
                      project.plan-nix.passthru.generateMaterialized
                    ]
                  );
              in
                pkgs.linkFarmFromDrvs "materializers" (
                  pkgs.lib.mapAttrsToList getMaterializers (
                      { ${projectName} = project; }
                      // (pkgs.lib.mapAttrs (_: builtins.getAttr "project") (project.tools tools))
                  )
                )
            )
          ];
        };
      }
    );
}
