{
  description = "wst";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        quicklisp = pkgs.fetchurl {
          url = "https://beta.quicklisp.org/quicklisp.lisp";
          sha256 = "4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17";
        };
      in
        {
          devShells.default = pkgs.mkShell {
            packages = [
              pkgs.sbcl
              pkgs.git
            ];

            shellHook = ''
            export QUICKLISP_HOME=$PWD/.quicklisp

            if [ ! -d "$QUICKLISP_HOME" ]; then
              echo "Installing Quicklisp locally..."
              sbcl --non-interactive \
                --load ${quicklisp} \
                --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_HOME\")" \
                --eval "(ql:add-to-init-file)" \
                --quit
            fi

            echo "SBCL + Quicklisp ready"
            echo "Run: sbcl"
          '';
          };
        });
}
