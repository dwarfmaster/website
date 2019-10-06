{ mkDerivation, base, containers, data-default, shakespeare, split
, stdenv, time, yesod
}:
mkDerivation {
  pname = "Site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-default shakespeare split time yesod
  ];
  homepage = "https://github.com/lucas8/website";
  description = "DwarfMaster website";
  license = stdenv.lib.licenses.gpl3;
  
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(readlink -f $(whereis ghc | cut -d' ' -f 2) | xargs dirname)/../share/doc/hoogle/index.html"
  '';
}
