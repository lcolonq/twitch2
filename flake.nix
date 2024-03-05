{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    ps-tools.follows = "purs-nix/ps-tools";
    lcolonq-prelude.url = "github:lcolonq/prelude";
    lcolonq-prelude.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ps-tools = inputs.ps-tools.legacyPackages.${system};
      purs-nix = inputs.purs-nix { inherit system; };
      lcolonq-prelude = inputs.lcolonq-prelude.packages.${system}.default;

      haskellOverrides = self: super: {
        inherit lcolonq-prelude;
        scotty = self.callHackageDirect {
          pkg = "scotty";
          ver = "0.21";
          sha256 = "sha256-coeQZJT7COSmoyA1eiykoMFv3+xNnxkF5tX4mlFcd84=";
        } {};
        backend = self.callCabal2nix "backend" ./backend {};
      };
      haskellPackages = pkgs.haskell.packages.ghc94.override {
        overrides = haskellOverrides;
      };

      purescript = purs-nix.purs {
        dependencies = [
          "console"
          "effect"
          "prelude"
          "random"
          "refs"
          "web-html"
          "web-dom"
          "web-uievents"
          "canvas"
        ];
        dir = ./frontend;
        srcs = [ "src" ];
      };
      frontend = purescript.bundle {};

      module = { config, lib, ... }:
        let
          name = "mybackend";
          cfg = config.colonq.services.${name};
        in {
          options.colonq.services.${name} = {
            enable = lib.mkEnableOption "Enable the backend service";
            configFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to config file";
              default = pkgs.writeText "backend.toml" ''
                port = 8000
                asset_path = "./frontend"
              '';
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services."colonq.${name}" = {
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                Restart = "on-failure";
                ExecStart = "${haskellPackages.backend}/bin/backend --config ${cfg.configFile}";
                DynamicUser = "yes";
                RuntimeDirectory = "colonq.${name}";
                RuntimeDirectoryMode = "0755";
                StateDirectory = "colonq.${name}";
                StateDirectoryMode = "0700";
                CacheDirectory = "colonq.${name}";
                CacheDirectoryMode = "0750";
              };
            };
          };
        };
    in {
      devShells.x86_64-linux.default = haskellPackages.shellFor {
        packages = hspkgs: with hspkgs; [
          backend
        ];
        withHoogle = true;
        buildInputs = [
          haskellPackages.haskell-language-server
          pkgs.nodejs
          (purescript.command {})
          ps-tools.for-0_15.purescript-language-server
          purs-nix.esbuild
          purs-nix.purescript
        ];
      };
      packages.x86_64-linux = {
        default = haskellPackages.backend;
      };
      apps.x86_64-linux.default = {
        type = "app";
        program = "${haskellPackages.backend}/bin/backend";
      };
      nixosModules.default = module;
    };
}
