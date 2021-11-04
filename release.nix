let proj = import ./. {};
in {
  dev = proj.shells.ghc;
  js = proj.exe;
}
