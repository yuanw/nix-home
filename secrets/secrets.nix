let
  user1 =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDEkr1oPi6Yk3iHFI568FNJLr3TAAy9EzjotVxf5vM3UvLCP2K7N3Jax8zamdMVajSH+s07xq8f7yyB2kFZtkPs8bs6ccWgbLakGehZOVo1Q0NZOiqVZ/viodK+maG3jhsp6oEdfuwMzQDLbcKj75kF3ma9LWHZNP47m6gsKvSC7GGZV5VG7JAUkUmWZyfGGXq1ExvkHT3/xf03R8g2CLDcC7rJtwhPTzyCzZBoZsPgOQ/ALPXpVhAhRPgNiReVaZyA5ROAnDm2iq7wJiwjs6A7PogE1N7E/T55SIgyQN3sBBpQwxo8tEVMmybmTeycmOnbTGwJWvLsdlCNXuFKjuzbEyBiJWi2r+q5p6Dvg02Eo/ESt6xmxjXICMPq1xn3qp/Q1tNlbXcV8oAqQQJE0wWD0tNWCV8onGgFY2MHVJT/Cqsx7mWOuSF8cgNkQ6U4/KKkAvUTNrgVFc85NqvlCnwC8evlPZz23K47QhL2VZr++sZ4L/l07eGBmEsynbTFVNnhVm4MGCsXPjabLO9pC9mV3JDoWG/47nCoIOlM8xs1nR82I6REi85DF4Wa9sJqGHGcSSjmKbnkl656RYVh19fuETk3RhuCVdyd88hMVm4V3KQPTTBtTF/6yjd0KFoYjKGZhDUnnV7UYXlo4rT/9VJHyzeJbTV502Vj07Ensapmvw==";
  yuanwang_wv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHM9A47jwAmysQRtmnDdfXtV1Slt8irXG2UsDZUn41rS";
  yuanwang_ca = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFpYgmWwtRG7vlRbtWheYrtHl9E9qx84sdU+YlE8w+CZ";
  users = [ user1 yuanwang_wv yuanwang_ca ];
  ashe =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINLStUG7iJOYDhS3GLuI0LkvSiY9fqsw2bM5p9JfoUqf";
  adguard = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEvgQaTxQmNGhbmkXYXmeTcbHo6vnpC0h0olTIErZxIC";
  systems = [ ashe adguard ];
in
{
  "secret1.age".publicKeys = users ++ systems;
  "adguard.age".publicKeys = users ++ systems;
  "adguard-encryption-key.age".publicKeys = users ++ systems;
  "adguard-encryption-certificate.age".publicKeys = users ++ systems;
  "wireguard-server-private.age".publicKeys = users ++ systems;
}
