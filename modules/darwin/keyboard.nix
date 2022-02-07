{ config, pkgs, lib, ...}:

with lib;

let
  cfg = config.peel.keybindings;
  mkMapping = from: to: { inherit from to; };
  mappings = [ (mkMapping "C" "^")
               (mkMapping "M" "~")
               (mkMapping "S" "$")
               (mkMapping ">" "&gt;")
               (mkMapping "<" "&lt;")
               (mkMapping "Bksp" "\U007F")
               (mkMapping "-" "")];
  replaceMappings = strings.replaceChars (builtins.map (x: x.from) mappings) (builtins.map (x: x.to) mappings);
  format = attrsets.mapAttrs' (k: v: nameValuePair (replaceMappings k) (if attrsets.isAttrs v then format v else v));
  keybindings = generators.toPlist {} (format cfg.mappings);
in {
  options.peel.keybindings = {
    mappings = mkOption {
      type = with types; nullOr (attrsOf (either str (either (attrsOf str) (listOf str))));
      default = null;
      example = ''
       {
         "C-/" = "undo:";
         "C-x" = {
            u = "undo:";
         };
       }
      '';
      description = ''
        The text system uses a generalized key-binding mechanism that is completely re-mappable by the user, although defining custom key bindings dynamically (that is, while the application is running) is not supported. The standard key bindings are specified in /System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict. These standard bindings include a large number of Emacs-compatible control key bindings, all the various arrow key bindings, bindings for making field editors and some keyboard UI work, and backstop bindings for many function keys.
        Details: Apple <https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html>
        Additional resources: Customize the Cocoa Text System <http://www.hcs.harvard.edu/~jrus/site/cocoa-text.html>
      '';
    };
  };
  
  config = {
    # system.activationScripts.peel.keybindings.text = mkIf (cfg.mappings != null) ''
    system.activationScripts.postActivation.text = mkIf (cfg.mappings != null) ''
      # Set defaults
      echo "configuring keybindings..." >&2
      mkdir -p ~/Library/KeyBindings || true
      cp ${pkgs.writeText "DefaultKeyBindings.dict" keybindings} ~/Library/KeyBindings/DefaultKeyBinding.dict
     '';
  };
}
