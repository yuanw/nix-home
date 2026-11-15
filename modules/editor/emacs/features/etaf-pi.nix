{ ... }:

{
  # Pi owns the conversation, ETAF owns the view: `etaf-pi-open' starts
  # `pi --mode rpc' in the current project, so Pi has to be on Emacs' PATH.
  #
  # `etaf', `etaf-ui', `ebox' and `ecss' arrive through etaf-pi's own requires,
  # so only the client is listed.  The framework is big, so keep it off the
  # startup path: the library still loads `etaf-pi-autoloads', which is what
  # makes the :commands below autoload.
  requireEpkgs = false;

  epkgs = epkgs: [ epkgs.etaf-pi ];

  # `C-c q' is the existing pi prefix (`s'essions, `t'ree, `o'pen file with
  # pilish, `p'olicy, `d'iagnose with tempel); `E' is free.  Inside the client
  # upstream keeps its own keys: `q' closes, `?' helps.
  elisp = ''
    (use-package etaf-pi
      :commands (etaf-pi-open etaf-pi-close)
      :bind (("C-c q E" . etaf-pi-open)))
  '';
}
