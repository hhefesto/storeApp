{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.agenix.nixosModules.default
  ];

  users.groups.admin = { };
  users.users.admin = {
    name = "admin";
    createHome = true;
    isNormalUser = true;
    group = "admin";
    home = "/home/admin";
    description = "tt admin";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    hashedPassword = "$6$Gh97W1iHUsUiGTDq$ZKlQloRJLbGm3hoQJUfLROWO.js1L3c15FQsAQs/Y/7jSwOdFyKLXuWnZWgqJBTy4GBvyp6vl5nkovNT/YdYQ0";
    openssh.authorizedKeys.keys = [ ''ssh-ed25519 _''
                                  ];
  };
  security.sudo.wheelNeedsPassword = false;
}
