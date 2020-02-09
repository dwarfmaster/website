{ mkDerivation, base, containers, hakyll, pandoc, stdenv }:
mkDerivation {
  pname = "dwarfmaster-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers hakyll pandoc ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
  
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(readlink -f $(whereis ghc | cut -d' ' -f 2) | xargs dirname)/../share/doc/hoogle/index.html"
  '';
}
