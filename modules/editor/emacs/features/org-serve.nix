{ ... }:

# Export an Org buffer or subtree to HTML and serve it through simple-httpd, so
# local <video>/<audio> sources work (they do not via a file:// export).
{
  epkgs = epkgs: [
    epkgs.simple-httpd
  ];

  elispFile = ./org-serve.el;
}
