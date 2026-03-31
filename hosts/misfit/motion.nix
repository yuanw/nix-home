{ ... }:
{
  services.motion-camera = {
    enable = true;
    dataDir = "/data/Cameras";
    port = 8080;
    streamPort = 8081;
    retentionDays = 7;
  };
}
