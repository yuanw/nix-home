{ config, ... }:
{
  services.declarative-jellyfin = {
    enable = true;
    serverId = "f4655afd22f348e19089c6b474b24a75";
    libraries = {
      Movies = {
        enabled = true;
        contentType = "movies";
        pathInfos = [ "/data/Movies" ];
        typeOptions.Movies = {
          metadataFetchers = [
            "The Open Movie Database"
            "TheMovieDb"
          ];
          imageFetchers = [
            "The Open Movie Database"
            "TheMovieDb"
          ];
        };
      };
      Shows = {
        enabled = true;
        contentType = "tvshows";
        pathInfos = [ "/data/Shows" ];
      };
      "Photos and videos" = {
        enabled = true;
        contentType = "homevideos";
        pathInfos = [
          "/data/Pictures"
          "/data/Videos"
        ];
      };
      Books = {
        enabled = true;
        contentType = "books";
        pathInfos = [ "/data/Books" ];
      };
      Music = {
        enabled = true;
        contentType = "music";
        pathInfos = [ "/data/Music" ];
      };
    };

    users = {
      yuanw = {
        mutable = false;
        hashedPasswordFile = config.age.secrets.jellyfin-admin.age.path;
        permissions = {
          isAdministrator = true;
        };
      };
    };

  };

  # This is just for making sure the library paths exists, you dont need this
  system.activationScripts.setupFolders =
    # bash
    ''
      mkdir -p /data/Movies
      mkdir -p /data/Shows
      mkdir -p /data/Pictures
      mkdir -p /data/Videos
      mkdir -p /data/Books
      mkdir -p /data/Music
    '';
}
