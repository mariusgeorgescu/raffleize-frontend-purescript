{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    purs-nix.url = "github:mariusgeorgescu/purs-nix";
    ps-tools.follows = "purs-nix/ps-tools";
    ctl.url = "github:Plutonomicon/cardano-transaction-lib/v7.0.0";
    utils.url = "github:ursi/flake-utils";
    ctl-nix.url = "github:LovelaceAcademy/ctl-nix";
    # script.url = "github:LovelaceAcademy/nix-templa/tes/change-cache?dir=iogx-plutus";
  };

  outputs = { self, utils, ... }@inputs:
    let
      # TODO add missing arm to match standard systems
      #  right now purs-nix is only compatible with x86_64-linux
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      overlays = with inputs.ctl.overlays; [
        # adds easy-ps for CTL
        purescript
        # adds:
        #  plutip-server
        #  ogmios
        #  kupo
        runtime
      ];
    in utils.apply-systems { inherit inputs systems overlays; }
    ({ system, pkgs, ... }@ctx:
      let
        inherit (pkgs) nodejs;
        # TODO Use a default purs version from CTL
        inherit (ctx.ps-tools.for-0_15)
          purescript purs-tidy purescript-language-server;
        purs-nix = inputs.purs-nix {
          inherit system;
          overlays = [ ctx.ctl-nix ];
        };

        ps = purs-nix.purs {
          inherit purescript nodejs;
          # Project dir (src, test)
          dir = ./.;
          # Dependencies
          dependencies = with purs-nix.ps-pkgs; [
            console
            effect
            halogen
            prelude
            cardano-transaction-lib
            cip30
            cip30-typesafe
          ];
          # FFI dependencies
          # foreign."Scripts".node_modules = scripts;
        };
        ps-command = ps.command { };
        purs-watch = pkgs.writeShellApplication {
          name = "purs-watch";
          runtimeInputs = with pkgs; [ entr ps-command ];
          text = "find src | entr -s 'echo building && purs-nix compile'";
        };
        vite = pkgs.writeShellApplication {
          name = "vite";
          runtimeInputs = with pkgs; [ nodejs ];
          text = "npx vite --open";
        };
        dev = pkgs.writeShellApplication {
          name = "dev";
          runtimeInputs = with pkgs; [ concurrently ];
          text = "concurrently purs-watch vite";
        };
      in {
        packages.default = ps.modules.Main.bundle { };

        # checks.default = checks;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            purescript-language-server
            ps-command
            purs-nix.purescript
            purs-watch
            vite
            dev
          ];
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.tcp4.me?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://hercules-ci.cachix.org"
      "https://klarkc.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.tcp4.me:cmk2Iz81lQuX7FtTUcBgtqgI70E8p6SOamNAIcFDSew="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
    ];
  };
}
