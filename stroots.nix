{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "stroots";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/stroots#readme";
  license = stdenv.lib.licenses.bsd3;
}
