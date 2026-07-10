{
  description = "directo - Reflex FRP Application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    reflex-platform = {
      url = "github:reflex-frp/reflex-platform/develop";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        reflexPlatform = import inputs.reflex-platform {
          inherit system;
          config.allowUnfree = true;
        };
        # # Import reflex-platform (remove sha256 to let Nix calculate it)
        # reflexPlatform = import (builtins.fetchTarball {
        #   url = "https://github.com/reflex-frp/reflex-platform/archive/develop.tar.gz";
        #   sha256 = "sha256:19z0qa80a6l6kq115fzrkyp8mrknfmiir6q71f6a263g7f9iw5dc";
        # }) {
        #   inherit system;
        #   config.allowUnfree = true;
        # };

        # Define our project using reflex-platform
        project = reflexPlatform.project ({ pkgs, ... }: {
          name = "directo";

          # Enable Warp for native builds
          useWarp = true;

          # Our packages - point to current directory
          packages = {
            directo = ./.;
          };

          # Shells configuration
          shells = {
            ghc = ["directo"];
            ghcjs = ["directo"];
          };

          # # Android configuration
          # android.dirClientFE = {
          #   executableName = "dirClientFE";
          #   applicationId = "com.example.rallm.client";
          #   displayName = "Store Client";
          # };

          # android.dirAdminFE = {
          #   executableName = "dirAdminFE";
          #   applicationId = "com.example.rallm.admin";
          #   displayName = "Store Admin";
          # };

          # # iOS configuration (for future use)
          # ios.dirClientFE = {
          #   executableName = "dirClientFE";
          #   bundleIdentifier = "com.example.rallm.client";
          #   bundleName = "Store Client";
          # };

          # ios.dirAdminFE = {
          #   executableName = "dirAdminFE";
          #   bundleIdentifier = "com.example.rallm.admin";
          #   bundleName = "Store Admin";
          # };

          # Additional overrides if needed
          overrides = self: super: {
            # Add any package overrides here if needed
          };
        });

        # Helper functions to get the correct package outputs
        getClientExe = pkg: "${pkg}/bin/dirClientFE";
        getAdminExe = pkg: "${pkg}/bin/dirAdminFE";
        getBackendExe = pkg: "${pkg}/bin/dirBackend";

      in
      {
        # Development shells
        devShells = {
          default = project.shells.ghc;
          ghc = project.shells.ghc;
          ghcjs = project.shells.ghcjs;
        };

        # Packages
        packages = {
          # Native executables
          default = project.ghc.directo;

          # Web builds (GHCJS) - only for frontend apps

          # # Android builds
          # androidClient = project.android.dirClientFE;
          # androidAdmin = project.android.dirAdminFE;

          # # iOS builds
          # iosClient = project.ios.dirClientFE;
          # iosAdmin = project.ios.dirAdminFE;
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
              if [ ! -d "${self.packages.${system}.webClient}/bin/dirClientFE.jsexe" ]; then
                echo "Building web client first..."
                nix build .#webClient
              fi
              echo "Serving client web app at http://localhost:8080"
              echo "Press Ctrl+C to stop the server"
              ${reflexPlatform.nixpkgs.python3}/bin/python3 -m http.server 8080 \
                --directory ${self.packages.${system}.webClient}/bin/dirClientFE.jsexe
            '';
          };

          # Serve admin web version
          serve-admin = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "serve-admin" ''
              if [ ! -d "${self.packages.${system}.webAdmin}/bin/dirAdminFE.jsexe" ]; then
                echo "Building web admin first..."
                nix build .#webAdmin
              fi
              echo "Serving admin web app at http://localhost:8081"
              echo "Press Ctrl+C to stop the server"
              ${reflexPlatform.nixpkgs.python3}/bin/python3 -m http.server 8081 \
                --directory ${self.packages.${system}.webAdmin}/bin/dirAdminFE.jsexe
            '';
          };

          # Development mode with ghcid for client
          dev-client = {
            type = "app";
            program = reflexPlatform.nixpkgs.writeShellScript "dev-client" ''
              echo "Starting Client Frontend in development mode with auto-reload..."
              echo "Edit src/ClientFE.hs and see changes instantly!"
              exec nix develop -c ghcid \
                --command "cabal repl exe:dirClientFE" \
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
                --command "cabal repl exe:dirAdminFE" \
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
                --command "cabal repl exe:dirBackend" \
                --run=":main"
            '';
          };
        };
      });
}
