# Shared enterprise search policies for Firefox / LibreWolf.
{
  SearchEngines = {
    PreventInstalls = true;
    Add = [
      {
        Name = "Kagi";
        URLTemplate = "https://kagi.com/search?q={searchTerms}";
        Method = "GET";
        IconURL = "https://kagi.com/asset/405c65f/favicon-32x32.png?v=49886a9a8f55fd41f83a89558e334f673f9e25cf";
        Description = "Kagi Search";
      }
      {
        Name = "Nix Packages";
        Description = "Nix package Search";
        URLTemplate = "https://search.nixos.org/packages?type=packages&query={searchTerms}";
        Method = "GET";
        IconURL = "https://nixos.org/favicon.png";
        Alias = "np";
      }
    ];
    # https://github.com/nix-community/home-manager/blob/master/modules/programs/firefox/profiles/search.nix#L212
    Remove = [
      "Bing"
      "eBay"
      "DuckDuckGo"
    ];
    Default = "Kagi";
  };
  SearchSuggestEnabled = false;
  Preferences = {
    "browser.urlbar.placeholderName" = "Kagi";
  };
}
