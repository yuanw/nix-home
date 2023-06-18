let
  user1 =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDEkr1oPi6Yk3iHFI568FNJLr3TAAy9EzjotVxf5vM3UvLCP2K7N3Jax8zamdMVajSH+s07xq8f7yyB2kFZtkPs8bs6ccWgbLakGehZOVo1Q0NZOiqVZ/viodK+maG3jhsp6oEdfuwMzQDLbcKj75kF3ma9LWHZNP47m6gsKvSC7GGZV5VG7JAUkUmWZyfGGXq1ExvkHT3/xf03R8g2CLDcC7rJtwhPTzyCzZBoZsPgOQ/ALPXpVhAhRPgNiReVaZyA5ROAnDm2iq7wJiwjs6A7PogE1N7E/T55SIgyQN3sBBpQwxo8tEVMmybmTeycmOnbTGwJWvLsdlCNXuFKjuzbEyBiJWi2r+q5p6Dvg02Eo/ESt6xmxjXICMPq1xn3qp/Q1tNlbXcV8oAqQQJE0wWD0tNWCV8onGgFY2MHVJT/Cqsx7mWOuSF8cgNkQ6U4/KKkAvUTNrgVFc85NqvlCnwC8evlPZz23K47QhL2VZr++sZ4L/l07eGBmEsynbTFVNnhVm4MGCsXPjabLO9pC9mV3JDoWG/47nCoIOlM8xs1nR82I6REi85DF4Wa9sJqGHGcSSjmKbnkl656RYVh19fuETk3RhuCVdyd88hMVm4V3KQPTTBtTF/6yjd0KFoYjKGZhDUnnV7UYXlo4rT/9VJHyzeJbTV502Vj07Ensapmvw==";
  yuanwang_ca = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJTPuCx79mCUfwGKWyId92yvRHo2F8PvacTlxIXbLOcr";
  users = [ user1 yuanwang_ca ];
  wf147084 =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDyPDccnmc7LE0E+Wg/+HapicOsTAlsiun7i0cBZtFE8CWpCuGXD2XkOpcvV9/+uygDcyOQMT+8diYz2jdvSKSQIk3FGa2nhE8KzKcjFSoOxqLJgTO8LmtbUJXp9r2OqPB+6dbG8+yTA7AILNb3AJZdEyvk0siFgLuHqVBDFo/YnuT5OhCFQG9oHmkehO8N/j6YUwWkXQkG/ar78BpHq+JNs+FxjNYeuBx31Z/012J0VSgZtFpFLJUnPlg0jjL1uWsTFjI2r2g37NcRRIFotfq492BrAJku4TGMXfRw43nAjH0U0EhPZZKYeXlEZjpy9pyw3F2sipye+GWbYyUxh64wN4onRlN3PrmzEHI+ISrmK39+5qQIf4FO/umI69pbfIZ4HcDP+ogVZn9weSpEMSnJRyLfQiupntu5KGlPoQ6H2/QCfgJbyqEV2Eqqtrfwlz+2zNvwxvOpV0wwmEzLNFFB9xaGYeisTSIp9uZQzRwl3bLbNFC/m1i+kxMkOaz/pi0=";
  ashe =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINLStUG7iJOYDhS3GLuI0LkvSiY9fqsw2bM5p9JfoUqf";
  adguard = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBZpxxUxq2nwPz3g+Iz1k4WFb88x4xAQKUgiw511VWtg";
  systems = [ wf147084 ashe adguard ];
in
{
  "secret1.age".publicKeys = users ++ systems;
  "adguard.age".publicKeys = users ++ systems;
  "adguard-encryption-key.age".publicKeys = users ++ systems;
  "adguard-encryption-certificate.age".publicKeys = users ++ systems;
  "wireguard-server-private.age".publicKeys = users ++ systems;
}
