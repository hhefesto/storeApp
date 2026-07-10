# Canonical profile module for the directo store stack.
#
# Consumed as `inputs.directo.nixosModules.default`; the flake applies this
# file to its own `self` so packages and secret paths default correctly:
#
#   services.directo.profile = {
#     enable = true;
#     mode = "production";                 # or "development"
#     serverName = "store.directo-qro.com";
#     ports = { nginx = 80; backend = 3002; };
#   };
#
# Production mode declares its own agenix secrets (files under
# ${self}/secrets/*.age, mode 0400) — the consumer must import
# agenix.nixosModules.default. Development mode adds a local hosts alias and
# postgres trust auth for the directo user.
#
# Postgres-sharing invariant: every project on the same host MUST agree on
# `ports.database` (NixOS cannot merge a single-valued port across modules).

self:
{ config, lib, pkgs, options, ... }:

let
  cfg = config.services.directo.profile;

  production  = cfg.mode == "production";
  development = cfg.mode == "development";

  selfPackages = self.packages.${pkgs.system};

  devDatabaseUrl =
    "postgres://${cfg.database.user}@localhost:${toString cfg.ports.database}/${cfg.database.name}";

  publicBaseUrl =
    "${if production then "https" else "http"}://${cfg.serverName}"
    + lib.optionalString (development && cfg.ports.nginx != 80)
        ":${toString cfg.ports.nginx}";

  dbPasswordFile =
    if cfg.secrets.dbPasswordFile != null
    then cfg.secrets.dbPasswordFile
    else config.age.secrets.directo-db-password.path;

  backendEnvFile =
    if cfg.secrets.backendEnvFile != null
    then cfg.secrets.backendEnvFile
    else config.age.secrets.directo-backend-env.path;

  adminHashFile =
    if cfg.secrets.adminPasswordHashFile != null
    then cfg.secrets.adminPasswordHashFile
    else config.age.secrets.directo-admin-password-hash.path;

  secretDecls =
    lib.optionalAttrs (cfg.secrets.dbPasswordFile == null) {
      directo-db-password = {
        file  = self + "/secrets/directo-db-password.age";
        owner = "postgres";
        group = "postgres";
        mode  = "0400";
      };
    }
    // lib.optionalAttrs (cfg.secrets.backendEnvFile == null) {
      directo-backend-env = {
        file  = self + "/secrets/directo-backend-env.age";
        owner = "root";
        group = "root";
        mode  = "0400";
      };
    }
    // lib.optionalAttrs (cfg.secrets.adminPasswordHashFile == null) {
      directo-admin-password-hash = {
        file  = self + "/secrets/directo-admin-password-hash.age";
        owner = "root";
        group = "root";
        mode  = "0400";
      };
    };

  hardening = {
    NoNewPrivileges = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    ProtectControlGroups = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    LockPersonality = true;
    RestrictSUIDSGID = true;
    RestrictRealtime = true;
    SystemCallArchitectures = "native";
    UMask = "0077";
  };
in {
  options.services.directo.profile = {
    enable = lib.mkEnableOption "directo store stack profile";

    mode = lib.mkOption {
      type = lib.types.enum [ "development" "production" ];
      description = "Deployment mode used to select secure defaults.";
    };

    serverName = lib.mkOption {
      type = lib.types.str;
      description = "Public DNS name (vhost) served by nginx.";
    };

    ports = {
      nginx = lib.mkOption {
        type = lib.types.port;
        description = "Public HTTP port for nginx.";
      };

      backend = lib.mkOption {
        type = lib.types.port;
        description = "Port the directo backend listens on.";
      };

      database = lib.mkOption {
        type = lib.types.port;
        default = 5432;
        description = "PostgreSQL port (must match other co-located projects).";
      };
    };

    database = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "directo";
        description = "Database name.";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = cfg.database.name;
        description = "PostgreSQL user (defaults to the database name).";
      };
    };

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      default = "hhefesto@rdataa.com";
      description = "Email used for ACME registration in production mode.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Open the nginx port (and 443 in production) in the firewall.";
    };

    store = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Directo — Refacciones para Electrodomésticos";
        description = "Sender name printed on DHL sheets.";
      };

      addressLines = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "Av. Ejemplo 123, Col. Centro" "76000 Querétaro, Qro., México" ];
        description = "Sender address lines printed on DHL sheets.";
      };

      phone = lib.mkOption {
        type = lib.types.str;
        default = "442-215-4990";
        description = "Sender phone printed on DHL sheets.";
      };
    };

    fallbackWeightGrams = lib.mkOption {
      type = lib.types.int;
      default = 500;
      description = "Weight estimate for items without a recorded weight.";
    };

    packages = {
      backend = lib.mkOption {
        type = lib.types.package;
        default = selfPackages.directo-backend-bundle;
        description = "The directo backend bundle (executables + migrations).";
      };

      staticRoot = lib.mkOption {
        type = lib.types.package;
        default = selfPackages.website;
        description = "Static website bundle (client at /, admin at /admin/).";
      };
    };

    secrets = {
      dbPasswordFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the PostgreSQL password file (production defaults to the in-repo agenix secret).";
      };

      backendEnvFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the backend EnvironmentFile (DATABASE_URL, MERCADOPAGO_ACCESS_TOKEN, MERCADOPAGO_WEBHOOK_SECRET, STRIPE_SECRET_KEY, STRIPE_WEBHOOK_SECRET, DIRECTO_GOOGLE_CLIENT_ID/SECRET).";
      };

      adminPasswordHashFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the bcrypt admin password hash file.";
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      # ── postgres ──
      services.postgresql = {
        enable = true;
        settings.port = cfg.ports.database;
        ensureDatabases = [ cfg.database.name ];
        ensureUsers = [
          {
            name = cfg.database.user;
            ensureDBOwnership = true;
          }
        ];
      };

      # ── migrations ──
      systemd.services.directo-migrate = {
        description = "directo schema migrations";
        wantedBy    = [ "multi-user.target" ];
        before      = [ "directo-backend.service" ];
        after       = [ "postgresql.service" "postgresql-setup.service" ];
        requires    = [ "postgresql.service" "postgresql-setup.service" ];

        environment = {
          MIGRATIONS_DIR = "${cfg.packages.backend}/share/directo/migrations";
        } // lib.optionalAttrs development {
          DATABASE_URL = devDatabaseUrl;
        };

        serviceConfig = {
          Type            = "oneshot";
          RemainAfterExit = true;
          ExecStart       = "${cfg.packages.backend}/bin/directo-migrate";
          DynamicUser     = true;
        } // lib.optionalAttrs production {
          EnvironmentFile = [ backendEnvFile ];
        };
      };

      # ── backend ──
      systemd.services.directo-backend = {
        description = "directo Servant backend";
        wantedBy    = [ "multi-user.target" ];
        after       = [ "postgresql.service" "postgresql-setup.service" "directo-migrate.service" ];
        requires    = [ "postgresql.service" "postgresql-setup.service" "directo-migrate.service" ];

        environment = {
          DIRECTO_PORT                  = toString cfg.ports.backend;
          DIRECTO_PUBLIC_BASE_URL       = publicBaseUrl;
          DIRECTO_STORE_NAME            = cfg.store.name;
          DIRECTO_STORE_ADDRESS         = lib.concatStringsSep "|" cfg.store.addressLines;
          DIRECTO_STORE_PHONE           = cfg.store.phone;
          DIRECTO_FALLBACK_WEIGHT_GRAMS = toString cfg.fallbackWeightGrams;
        } // lib.optionalAttrs production {
          DIRECTO_COOKIE_SECURE = "1";
        } // lib.optionalAttrs development {
          DATABASE_URL = devDatabaseUrl;
        };

        serviceConfig = {
          ExecStart   = "${cfg.packages.backend}/bin/directo-backend";
          Restart     = "on-failure";
          DynamicUser = true;
        } // lib.optionalAttrs (production || cfg.secrets.backendEnvFile != null) {
          # In development an explicit backendEnvFile can supply optional
          # credentials (Mercado Pago token, Google OAuth client id/secret).
          EnvironmentFile = [ backendEnvFile ];
        };
      };

      # ── nginx ──
      services.nginx = {
        enable = true;
        virtualHosts.${cfg.serverName} = {
          listen = [
            { addr = "0.0.0.0"; port = cfg.ports.nginx; }
            { addr = "[::]";    port = cfg.ports.nginx; }
          ] ++ lib.optionals production [
            { addr = "0.0.0.0"; port = 443; ssl = true; }
            { addr = "[::]";    port = 443; ssl = true; }
          ];
          root = cfg.packages.staticRoot;
          locations."/" = {
            tryFiles = "$uri $uri/ /index.html";
          };
          locations."= /admin" = {
            return = "302 /admin/";
          };
          locations."/admin/" = {
            alias = "${cfg.packages.staticRoot}/admin/";
            tryFiles = "$uri $uri/ /admin/index.html";
          };
          locations."/api/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.ports.backend}";
            recommendedProxySettings = true;
          };
        };
      };

      networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall
        ([ cfg.ports.nginx ] ++ lib.optional production 443);
    }

    (lib.mkIf development {
      networking.hosts."127.0.0.1" = [ cfg.serverName ];

      services.postgresql.authentication = lib.mkAfter ''
        host all ${cfg.database.user} 127.0.0.1/32 trust
        host all ${cfg.database.user} ::1/128      trust
      '';
    })

    # `options ? age` guards the attribute so this module evaluates without
    # agenix (e.g. development mode); production asserts agenix is present.
    (lib.mkIf production (lib.optionalAttrs (options ? age) {
      age.secrets = secretDecls;
    }))

    (lib.mkIf production {
      assertions = [
        {
          assertion = options ? age;
          message = "services.directo.profile production mode requires the agenix NixOS module to be imported.";
        }
      ];

      services.postgresql.authentication = lib.mkAfter ''
        local all ${cfg.database.user} md5
        host  all ${cfg.database.user} 127.0.0.1/32 md5
        host  all ${cfg.database.user} ::1/128 md5
      '';

      systemd.services.postgresql-setup.postStart = lib.mkAfter ''
        pw="$(${pkgs.coreutils}/bin/cat ${dbPasswordFile})"
        ${config.services.postgresql.package}/bin/psql \
          -v ON_ERROR_STOP=1 -d postgres -v pw="$pw" <<'SQL'
        ALTER USER ${cfg.database.user} WITH PASSWORD :'pw';
        SQL
      '';

      services.nginx.virtualHosts.${cfg.serverName} = {
        enableACME = true;
        forceSSL   = true;
      };

      security.acme = {
        acceptTerms = true;
        defaults.email = cfg.acmeEmail;
      };

      # The backend runs as DynamicUser and cannot read a 0400 root-owned
      # secret directly; systemd hands it a private copy.
      systemd.services.directo-backend.environment.DIRECTO_ADMIN_PASSWORD_HASH_FILE =
        "/run/credentials/directo-backend.service/admin-hash";

      systemd.services.directo-backend.serviceConfig = hardening // {
        LoadCredential = [ "admin-hash:${adminHashFile}" ];
      };

      systemd.services.directo-migrate.serviceConfig = hardening;
    })
  ]);
}
