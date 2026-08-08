{
  epkgs = epkgs: [
    epkgs.cmake-mode
  ];

  elisp = ''
    (use-package cmake-mode
      :mode ("\\.cmake\\'"
             "CMakeLists.txt\\'"))
  '';
}
