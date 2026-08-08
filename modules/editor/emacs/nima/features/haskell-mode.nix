{
  epkgs = epkgs: [
    epkgs.haskell-mode
  ];

  elisp = ''
    (use-package haskell-mode
      :mode (("\\.hs\\'" . haskell-mode)
             ("\\.hsc\\'" . haskell-mode)
             ("\\.c2hs\\'" . haskell-mode)
             ("\\.cpphs\\'" . haskell-mode)
             ("\\.lhs\\'" . haskell-literate-mode))
      :hook (haskell-mode . subword-mode))
  '';
}
