{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }: {
  inherit withHoogle;
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    obelisk-google-analytics = hackGet deps/obelisk-google-analytics;
    tutorial = hackGet deps/calculator-tutorial + "/tutorial";
  };

  overrides = with pkgs.haskell.lib; self: super: {
    conduit = dontCheck super.conduit;
    mmark = dontCheck super.mmark;
    mono-traversable = dontCheck super.mono-traversable;
    unliftio = dontCheck super.unliftio;
    yaml = dontCheck super.yaml;
  };

})
