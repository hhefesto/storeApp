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
          # android.saClientFE = {
          #   executableName = "saClientFE";
          #   applicationId = "com.example.rallm.client";
          #   displayName = "Store Client";
          # };

          # android.saAdminFE = {
          #   executableName = "saAdminFE";
          #   applicationId = "com.example.rallm.admin";
          #   displayName = "Store Admin";
          # };

          # # iOS configuration (for future use)
          # ios.saClientFE = {
          #   executableName = "saClientFE";
          #   bundleIdentifier = "com.example.rallm.client";
          #   bundleName = "Store Client";
          # };

          # ios.saAdminFE = {
          #   executableName = "saAdminFE";
          #   bundleIdentifier = "com.example.rallm.admin";
          #   bundleName = "Store Admin";
          # };

          # Additional overrides if needed
          overrides = self: super: {
            # Add any package overrides here if needed
          };
        });

        # Helper functions to get the correct package outputs
        getClientExe = pkg: "${pkg}/bin/saClientFE";
        getAdminExe = pkg: "${pkg}/bin/saAdminFE";
        getBackendExe = pkg: "${pkg}/bin/saBackend";

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
          # Native executables
          default = project.ghc.storeApp;

          # Web builds (GHCJS) - only for frontend apps

          # # Android builds
          # androidClient = project.android.saClientFE;
          # androidAdmin = project.android.saAdminFE;

          # # iOS builds
          # iosClient = project.ios.saClientFE;
          # iosAdmin = project.ios.saAdminFE;
        };

        # Apps for easy running
        apps = {
          # Run client frontend natively
          client = {
            type = "app";
            program = getClientExe self.packages.${system}.default;
          };

          # Run admin frontend natively
          admin = {
            type = "app";
            program = getAdminExe self.packages.${system}.default;
          };

          # Run backend
          backend = {
            type = "app";
            program = getBackendExe self.packages.${system}.default;
          };

          # Serve client web version
          serve-client = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "serve-client" ''
              if [ ! -d "${self.packages.${system}.webClient}/bin/saClientFE.jsexe" ]; then
                echo "Building web client first..."
                nix build .#webClient
              fi
              echo "Serving client web app at http://localhost:8080"
              echo "Press Ctrl+C to stop the server"
              ${reflexPlatform.nixpkgs.python3}/bin/python3 -m http.server 8080 \
                --directory ${self.packages.${system}.webClient}/bin/saClientFE.jsexe
            '';
          };

          # Serve admin web version
          serve-admin = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "serve-admin" ''
              if [ ! -d "${self.packages.${system}.webAdmin}/bin/saAdminFE.jsexe" ]; then
                echo "Building web admin first..."
                nix build .#webAdmin
              fi
              echo "Serving admin web app at http://localhost:8081"
              echo "Press Ctrl+C to stop the server"
              ${reflexPlatform.nixpkgs.python3}/bin/python3 -m http.server 8081 \
                --directory ${self.packages.${system}.webAdmin}/bin/saAdminFE.jsexe
            '';
          };

          # Development mode with ghcid for client
          dev-client = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "dev-client" ''
              echo "Starting Client Frontend in development mode with auto-reload..."
              echo "Edit src/ClientFE.hs and see changes instantly!"
              exec nix develop -c ghcid \
                --command "cabal repl exe:saClientFE" \
                --run=":main"
            '';
          };

          # Development mode with ghcid for admin
          dev-admin = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "dev-admin" ''
              echo "Starting Admin Frontend in development mode with auto-reload..."
              echo "Edit src/AdminFE.hs and see changes instantly!"
              exec nix develop -c ghcid \
                --command "cabal repl exe:saAdminFE" \
                --run=":main"
            '';
          };

          # Development mode with ghcid for backend
          dev-backend = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "dev-backend" ''
              echo "Starting Backend in development mode with auto-reload..."
              echo "Edit src/Backend.hs and see changes instantly!"
              exec nix develop -c ghcid \
                --command "cabal repl exe:saBackend" \
                --run=":main"
            '';
          };
        };
      });
}
