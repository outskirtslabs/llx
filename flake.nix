{
  description = "dev env";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devenv.url = "https://flakehub.com/f/ramblurr/nix-devenv/*";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    clj-nix.url = "github:jlesquembre/clj-nix";
    clj-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    inputs@{
      self,
      clj-nix,
      devenv,
      devshell,
      ...
    }:
    devenv.lib.mkFlake ./. {
      inherit inputs;

      treefmtConfig = {
        programs = {
          nixfmt.enable = true;
          cljfmt.enable = true;
          prettier.enable = true;
          mdformat.plugins =
            ps: with ps; [
              mdformat-gfm
              mdformat-gfm-alerts
            ];
        };
      };
      withOverlays = [
        devshell.overlays.default
        devenv.overlays.default
        clj-nix.overlays.default
      ];
      packages =
        let
          mkLlxPackage =
            pkgs:
            let
              root = toString ./.;
              gitRev =
                if self ? rev then
                  self.rev
                else if self ? dirtyRev then
                  self.dirtyRev
                else
                  "dirty";
              projectSrc = pkgs.lib.cleanSourceWith {
                src = ./.;
                filter =
                  path: _type:
                  let
                    rel = pkgs.lib.removePrefix (root + "/") (toString path);
                    base = builtins.baseNameOf path;
                  in
                  !(
                    base == ".git"
                    || rel == "result"
                    || pkgs.lib.hasPrefix "node_modules/" rel
                    || pkgs.lib.hasPrefix "target/" rel
                    || pkgs.lib.hasPrefix ".shadow-cljs/" rel
                    || pkgs.lib.hasPrefix ".cpcache/" rel
                    || pkgs.lib.hasPrefix "tmp/" rel
                  );
              };
            in
            pkgs.mkCljLib {
              inherit projectSrc;
              name = "com.outskirtslabs/llx";
              version = "0.0.1";
              nativeBuildInputs = [
                pkgs.coreutils
                pkgs.nodejs
              ];
              GIT_REV = gitRev;
              JAVA_HOME = pkgs.jdk25.home;
              buildCommand = ''
                export JAVA_HOME="${pkgs.jdk25.home}"
                export JAVA_CMD="${pkgs.jdk25}/bin/java"
                clojure -M:dev:kaocha :unit
                clojure -T:build jar
              '';
            };
        in
        {
          default = mkLlxPackage;
        };
      checks =
        let
          mkLlxPackage = pkgs: self.packages.${pkgs.system}.default;
        in
        {
          default = mkLlxPackage;
        };
      devShell =
        pkgs:
        pkgs.devshell.mkShell {
          imports = [
            devenv.capsules.base
            devenv.capsules.clojure
          ];
          # https://numtide.github.io/devshell
          commands = [
          ];
          packages = [
            pkgs.deps-lock
            pkgs.jdk25
            pkgs.nodejs
          ];
        };
    };
}
