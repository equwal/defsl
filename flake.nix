{
  description = "Wrapper for the lowest common denominator of Sly and Slime.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAll (pkgs: with pkgs; rec {
        sl = sbcl.buildASDFSystem {
          pname = "sl";
          version = "1.0";
          src = (lib.cleanSourceWith { src = self; filter = p: _: !(lib.hasSuffix ".fasl" p || lib.hasPrefix ".#" (baseNameOf p)); });
          systems = [ "sl" ];
          lispLibs = [ sbclPackages.alexandria ];
          meta = { description = "Wrapper for the lowest common denominator of Sly and Slime."; homepage = "https://github.com/equwal/defsl"; license = lib.licenses.gpl3Only; };
        };
        default = sl;
        # an SBCL with this system (and its dependencies) preloaded: `nix run .#sbcl`
        sbcl-with = sbcl.withPackages (ps: [ sl ]);
      });
      apps = forAll (pkgs: {
        sbcl = { type = "app"; program = "${self.packages.${pkgs.system}.sbcl-with}/bin/sbcl"; };
      });
    };
}
