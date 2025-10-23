{
  description = "storeApp - Reflex FRP Application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Import reflex-platform (remove sha256 to let Nix calculate it)
        reflexPlatform = import (builtins.fetchTarball {
          url = "https://github.com/reflex-frp/reflex-platform/archive/develop.tar.gz";
          sha256 = "sha256:1vna7iyhl62sqicib34hs7haaaysxlpbj021qqp2v6fmsx25iyin";
        }) {
          inherit system;
          config.allowUnfree = true;
        };

        # Define our project using reflex-platform
        project = reflexPlatform.project ({ pkgs, ... }: {
          name = "storeApp";

          # Enable Warp for native builds
          useWarp = true;

          # Our packages - point to current directory
          packages = {
            storeApp = ./.;
          };

          # Shells configuration
          shells = {
            ghc = ["storeApp"];
            ghcjs = ["storeApp"];
          };

          # # Android configuration
          # android.storeApp = {
          #   executableName = "storeApp";
          #   applicationId = "com.example.rallm";
          #   displayName = "storeApp";
          # };

          # # iOS configuration (for future use)
          # ios.storeApp = {
          #   executableName = "storeApp";
          #   bundleIdentifier = "com.example.rallm";
          #   bundleName = "storeApp";
          # };

          # Additional overrides if needed
          overrides = self: super: {
            # Add any package overrides here if needed
          };
        });

        # Helper to get the correct package output
        getExe = pkg: "${pkg}/bin/storeApp";

      in
      {
        # Development shells
        devShells = {
          default = project.shells.ghc;
          ghc = project.shells.ghc;
          ghcjs = project.shells.ghcjs;
        };

        # Packages
        packages = rec {
          # Native executable
          default = native;
          native = project.ghc.storeApp;

          # Web build (GHCJS)
          web = project.ghcjs.storeApp;

          # # Android build
          # android = project.android.storeApp;

          # # iOS build
          # ios = project.ios.storeApp;
        };

        # Apps for easy running
        apps = {
          # Run native version
          native = {
            type = "app";
            program = getExe self.packages.${system}.native;
          };

          # Serve web version
          serve-web = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "serve-web" ''
              if [ ! -d "${self.packages.${system}.web}/bin/storeApp.jsexe" ]; then
                echo "Building web version first..."
                nix build .#web
              fi
              echo "Serving web app at http://localhost:8080"
              echo "Press Ctrl+C to stop the server"
              ${reflexPlatform.nixpkgs.python3}/bin/python3 -m http.server 8080 \
                --directory ${self.packages.${system}.web}/bin/storeApp.jsexe
            '';
          };

          # Development mode with ghcid
          dev = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "dev-storeApp" ''
              echo "Starting storeApp in development mode with auto-reload..."
              echo "Edit src/Main.hs and see changes instantly!"
              exec nix develop -c ghcid \
                --command "cabal repl exe:storeApp" \
                --run=":main"
            '';
          };
        };
      });
}
