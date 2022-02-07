{ config, pkgs, ...}:

{
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  system.defaults = {
     dock = {
       autohide = true;
       orientation = "right";
       showhidden = true;
       mineffect = "scale";
       launchanim = false;
       show-process-indicators = true;
       tilesize = 48;
       static-only = true;
       mru-spaces = false;
     };
     finder = {
       AppleShowAllExtensions = true;
       FXEnableExtensionChangeWarning = false;
     };
     trackpad = {
       Clicking = true;
       TrackpadThreeFingerDrag = true;
     };
     NSGlobalDomain = {
       AppleKeyboardUIMode = 3;
       ApplePressAndHoldEnabled = false;
       InitialKeyRepeat = 10;
       KeyRepeat = 1;
       NSAutomaticCapitalizationEnabled = false;
       NSAutomaticDashSubstitutionEnabled = false;
       NSAutomaticPeriodSubstitutionEnabled = false;
       NSAutomaticQuoteSubstitutionEnabled = false;
       NSAutomaticSpellingCorrectionEnabled = false;
       NSNavPanelExpandedStateForSaveMode = true;
       NSNavPanelExpandedStateForSaveMode2 = true;
     };
   };

}
