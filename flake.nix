{
  description = "directo — tienda de refacciones (Reflex-DOM frontends + Servant backend)";

  nixConfig = {
    allow-import-from-derivation = true;
    extra-substituters        = [ "https://nixcache.reflex-frp.org" ];
    extra-trusted-public-keys = [
      "ryantrinkle.com-1:JJiAKaRv9mwhkerZRpQmMkMsk+p2JXCetKFVJFgZB6Y="
    ];
  };

  inputs = {
    nixpkgs.url       = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url   = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    agenix.url        = "github:ryantm/agenix";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [ haskell-flake.flakeModule ];

      flake.nixosModules.default = import ./nixosModules/default.nix inputs.self;

      perSystem = { self', system, pkgs, lib, config, ... }:
        let
          ghcVer = "ghc910";

          # GHC's native JS backend via the javascript-unknown-ghcjs target.
          jsPkgs = pkgs.pkgsCross.ghcjs.haskell.packages.${ghcVer};

          directoSharedJs   = jsPkgs.callCabal2nix "directo-shared" ./shared {};
          directoFrontendJs = (jsPkgs.callCabal2nix "directo-frontend" ./frontend {
            directo-shared = directoSharedJs;
          }).overrideAttrs (_: { dontStrip = true; });

          staticAssets = builtins.path {
            name = "directo-static";
            path = ./frontend/static;
          };

          # Client SPA at /, admin SPA at /admin/.
          website = pkgs.runCommand "directo-website" {
            nativeBuildInputs = [ pkgs.rsync ];
          } ''
            mkdir -p "$out/static" "$out/admin"
            copy_jsexe () {
              local exe="$1" dest="$2"
              local jsexe="${directoFrontendJs}/bin/$exe.jsexe"
              if [ -d "$jsexe" ]; then
                cp -r "$jsexe"/. "$dest/"
              else
                echo "missing $jsexe" >&2; exit 1
              fi
            }
            copy_jsexe directo-client "$out"
            copy_jsexe directo-admin  "$out/admin"
            rsync -r --no-perms --chmod=Du+rwx,Fu+rw \
              ${staticAssets}/ "$out/static/"
            install -m644 ${./index.html}       "$out/index.html"
            install -m644 ${./admin-index.html} "$out/admin/index.html"
          '';
        in {
          # Native GHC project: shared + backend (HLS, devshell, native build)
          haskellProjects.default = {
            projectRoot = ./.;
            basePackages = pkgs.haskell.packages.${ghcVer};
            autoWire = [ "packages" "checks" ];
            settings = {
              directo-backend.justStaticExecutables = true;
            };
            devShell = {
              tools = hp: { inherit (hp) cabal-install ghcid; };
              hlsCheck.enable = false;
            };
          };

          packages.directo-frontend = directoFrontendJs;
          packages.website          = website;
          packages.default          = website;

          packages.directo-backend-bundle = pkgs.runCommand "directo-backend-bundle" {} ''
            mkdir -p "$out/bin" "$out/share/directo/migrations"
            cp -r ${self'.packages.directo-backend}/bin/. "$out/bin/"
            cp ${./backend/migrations}/*.sql "$out/share/directo/migrations/"
          '';

          devShells.default = pkgs.mkShell {
            inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
            packages = [
              inputs.agenix.packages.${system}.default
              pkgs.postgresql
            ];
          };

          apps.default = {
            type = "app";
            program = toString (pkgs.writeShellScript "serve-directo" ''
              ${pkgs.psmisc}/bin/fuser -k 8085/tcp 2>/dev/null || true
              echo "directo website served at http://localhost:8085 (static preview, no API)"
              exec ${pkgs.darkhttpd}/bin/darkhttpd ${self'.packages.website} \
                --port 8085 \
                --header 'Cache-Control: no-store, no-cache, must-revalidate, max-age=0'
            '');
          };

          checks = {
            website        = self'.packages.website;
            backend-bundle = self'.packages.directo-backend-bundle;
          };
        };
    };
}
