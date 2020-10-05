# {config, pkgs, ...}:

# {
#   users.users.peel =  { uid = 501; home = "/Users/peel"; };
#   home-manager.useUserPackages = true;  
#   home-manager.users.peel = {pkgs, ...}: {
#     programs.direnv = {
#       enable = true;
#       enableBashIntegration = true;
#       enableNixDirenvIntegration = true;
#     };
#     # programs.mbsync.enable = true;
#     # programs.msmtp.enable = true;
#     # programs.notmuch = {
#     #   enable = true;
#     #   hooks = {
#     #     preNew = "mbsync --all";
#     #   };
#     # };
#     # accounts.email = {
#     #   accounts.protonmail = {
#     #     address = "plimanowski@pm.me";
#     #     gpg = {
#     #       key = "A03BCA31003F49A1D839391EF50AA5EE52A54A46";
#     #       signByDefault = true;
#     #     };
#     #     imap = {
#     #       host = "127.0.0.1";
#     #       port = 1143;
#     #     };
#     #     smtp = {
#     #       host = "127.0.0.1";
#     #       port = 1025;
#     #     };
#     #     mbsync = {
#     #       enable = true;
#     #       create = "maildir";
#     #     };
#     #     msmtp.enable = true;
#     #     notmuch.enable = true;
#     #     primary = true;
#     #     realName = "Piotr Limanowski";
#     #     signature = {
#     #       text = ''
#     #       Piotr Peel Limanowski
#     #       https://keybase.io/peel
#     #     '';
#     #       showSignature = "append";
#     #     };
#     #     passwordCommand = "${config.peel.secrets.protonmail-local}";
#     #     userName = "plimanowski@pm.me";
#     #   };      
#     };
#   # };
# }
