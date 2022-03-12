with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "omnidict";
  buildInputs = with pkgs; [ghc zlib];
}
