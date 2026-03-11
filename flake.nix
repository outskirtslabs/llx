{
  description = "dev env";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    devshell.url = "github:numtide/devshell";
    devenv.url = "https://flakehub.com/f/ramblurr/nix-devenv/*";
  };
  outputs =
    {
      self,
      devenv,
      devshell,
      ...
    }:
    devenv.lib.mkFlake ./. {

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
      ];
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

        };
    };
}
