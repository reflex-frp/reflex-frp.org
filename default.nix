{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false
}:

with import ./.obelisk/impl { inherit system iosSdkVersion; };

project ./. ({ pkgs, hackGet, ... }: let
  tutorialSrc = hackGet deps/calculator-tutorial;
in {
  inherit withHoogle;
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    obelisk-google-analytics = hackGet deps/obelisk-google-analytics;
    # TODO make overlay or something in that repo
    mmark = hackGet ./deps/mmark;
    modern-uri = hackGet ./deps/modern-uri;
  };

  overrides = with pkgs.haskell.lib; self: super: {
    conduit = dontCheck super.conduit;
    mmark = if self.ghc.isGhcjs or false then dontHaddock super.mmark else super.mmark;
    mono-traversable = dontCheck super.mono-traversable;
    unliftio = dontCheck super.unliftio;
    yaml = dontCheck super.yaml;
    modern-uri = doJailbreak super.modern-uri;
    # TODO: Find a better way to combine obelisk apps:
    tutorial-frontend = self.callCabal2nix "tutorial-frontend" (pkgs.runCommand "tutorial-import" {} ''
      set -eux
      mkdir tutorial-frontend
      cp -rL "${tutorialSrc + "/frontend"}"/* tutorial-frontend/
      mv tutorial-frontend/frontend.cabal tutorial-frontend/tutorial-frontend.cabal
      substituteInPlace tutorial-frontend/tutorial-frontend.cabal \
        --replace "name: frontend" "name: tutorial-frontend" \
        --replace "executable frontend" "executable tutorial-frontend${"\n"}  buildable: false" \
        --replace ", frontend" ", tutorial-frontend" \
        --replace "exposed-modules: Frontend" "exposed-modules: "
      mkdir $out
      cp -rL tutorial-frontend/* $out/
    '') {};
  };
})
