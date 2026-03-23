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
          mkCljsCheck =
            pkgs:
            let
              root = toString ./.;
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
              npmDeps = pkgs.fetchNpmDeps {
                src = projectSrc;
                hash = "sha256-loXK2ZmTWxCnP/ym4F+5ZK8pWLAunrTTSmrjwy4wyWc=";
              };
            in
            pkgs.mkCljLib {
              inherit projectSrc;
              name = "com.outskirtslabs/llx-cljs-tests";
              version = "0.0.1";
              nativeBuildInputs = [
                pkgs.coreutils
                pkgs.nodejs
                pkgs.npmHooks.npmConfigHook
              ];
              inherit npmDeps;
              JAVA_HOME = pkgs.jdk25.home;
              buildCommand = ''
                export JAVA_HOME="${pkgs.jdk25.home}"
                export JAVA_CMD="${pkgs.jdk25}/bin/java"
                export PATH="${pkgs.nodejs}/bin:$PATH"
                export HOME=$(mktemp -d)

                # start funnel in the background (required by kaocha-cljs2)
                clojure -M:funnel &
                FUNNEL_PID=$!
                sleep 2

                clojure -M:dev:kaocha :cljs

                kill $FUNNEL_PID 2>/dev/null || true
              '';
              installPhase = ''
                mkdir -p $out
                echo "cljs tests passed" > $out/result.txt
              '';
            };
        in
        {
          default = mkLlxPackage;
          cljs = mkCljsCheck;
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
