let mailAddr = name: domain: "${name}@${domain}";
in {
  username = "yuanwang";
  name = "Yuan Wang";
  email = mailAddr "me" "yuanwang.ca";
  hostname = "yuan-mac";
  gpgKey = "BF2ADAA2A98F45E7";
}
