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
            export INIT_FILE="$PWD/.sbclrc"
            export QUICKLISP_HOME=$PWD/.quicklisp

            if [ ! -d ".sbclrc" ]; then
               echo "Creating local sbcl resource file."
               touch $INIT_FILE
            fi


            if [ ! -d "$QUICKLISP_HOME" ]; then
              cat <<EOF > $INIT_FILE
#-quicklisp
(let ((quicklisp-init #P"$QUICKLISP_HOME/setup.lisp"))
  (when (probe-file quicklisp-init)
      (load quicklisp-init)))

#+quicklisp
(push #P"$PWD" ql:*local-project-directories*)
EOF

              echo "Installing Quicklisp..."
              sbcl --non-interactive \
                --load ${quicklisp} \
                --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_HOME\")" \
                --quit

              echo "Installing qlot..."
              sbcl --non-interactive \
                --sysinit "$INIT_FILE" \
                --eval "(ql:quickload :qlot)" \
                --eval "(qlot:init \"$PWD\")" \
                --eval "(qlot:install)"

            fi

            echo "SBCL + Quicklisp + qlot"
          '';
          };
        });
}
