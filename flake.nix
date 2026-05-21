{
    description = "A utility for automating repeated ODOO operations";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        utils.url = "github:numtide/flake-utils";

        sbt = {
          url = "github:zaninime/sbt-derivation";
          inputs.nixpkgs.follows = "nixpkgs";
          inputs.flake-utils.follows = "utils";
        };
    };

    outputs = { self, nixpkgs, sbt, utils }:
      utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        name = "odoo-automation";
        mainClass = "peschke.odoo.cli.Run";

        app = sbt.mkSbtDerivation.${system} {
          #inherit pkgs;

          pname = name;
          src = ./.;

          version = "0.1.0";
          depsSha256 = "sha256-+7ftFT1REsooztIFZVCIzQXOxolQQ7ZucrtJ/5wY0KA=";

          buildPhase = ''
            sbt 'compile;test;stage'
          '';

          startScript = ''
            #!${pkgs.runtimeShell}

            exec ${pkgs.openjdk_headless}/bin/java ''${JAVA_OPTS:-} -cp "${
              placeholder "out"
            }/share/${name}/lib/*" ${nixpkgs.lib.escapeShellArg mainClass} "$@"
          '';

          installPhase = ''
            libs_dir="$out/share/${name}/lib"
            mkdir -p "$libs_dir"
            cp -ar target/universal/stage/lib/. "$libs_dir"

            install -T -D -m755 $startScriptPath $out/bin/${name}
          '';

           passAsFile = ["startScript"];
        };
      in {
        packages.default = app;
      });
}