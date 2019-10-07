{ mkDerivation, base, blaze-html, blaze-markup, containers
, data-default, mtl, pandoc, pandoc-types, shakespeare, split
, stdenv, text, time, yesod
}:
mkDerivation {
  pname = "Site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup containers data-default mtl pandoc
    pandoc-types shakespeare split text time yesod
  ];
  homepage = "https://github.com/lucas8/website";
  description = "DwarfMaster website";
  license = stdenv.lib.licenses.mit;
  
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(readlink -f $(whereis ghc | cut -d' ' -f 2) | xargs dirname)/../share/doc/hoogle/index.html"
  '';
}
