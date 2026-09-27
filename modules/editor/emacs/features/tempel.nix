{ ... }:

# All Elisp for this feature lives in tempel.el (loaded + byte-compiled by
# the feature loader), which configures Tempel and contributes the pi session
# openers.  See the Commentary in tempel.el for details.
{
  epkgs = epkgs: [
    epkgs.tempel
  ];

  elispFile = ./tempel.el;
}
