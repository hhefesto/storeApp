let
  admin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBAolzCtF1t8rPKSRzvREQPBUjxRAi5medog8Ebi0n/G hhefesto@rdataa.com";
  xty = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5cWilmgGZa24PrEFftyajabwxHvR4jxvkIvqkyCtmG root@nixos";

  users = [ admin ];
  systems = [ xty ];
in
{
  "directo-db-password.age".publicKeys = users ++ systems;
  # DATABASE_URL=…, MERCADOPAGO_ACCESS_TOKEN=…, MERCADOPAGO_WEBHOOK_SECRET=…
  "directo-backend-env.age".publicKeys = users ++ systems;
  "directo-admin-password-hash.age".publicKeys = users ++ systems;
}
