{ mkDerivation, base, fuzzy, lens, optparse-applicative, stdenv
, tagsoup-navigate, wreq
}:
mkDerivation {
  pname = "z";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base fuzzy lens optparse-applicative tagsoup-navigate wreq
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
