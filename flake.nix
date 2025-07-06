{
  description = "wst flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-23.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in {

      packages.${system} = {
        inputs = [pkgs.lispPackages.quicklisp];
        default = pkgs.sbcl;
      };

      shellHooks = """
    EDITOR=emacs
""";

    };
}
