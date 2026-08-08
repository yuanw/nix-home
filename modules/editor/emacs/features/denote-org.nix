{
  epkgs = epkgs: [
    epkgs.denote-org
  ];

  elisp = ''
    (use-package denote-org
      :commands (denote-org-link-to-heading
                 denote-org-backlinks-for-heading
                 denote-org-extract-org-subtree
                 denote-org-convert-links-to-file-type
                 denote-org-convert-links-to-denote-type
                 denote-org-dblock-insert-files
                 denote-org-dblock-insert-links
                 denote-org-dblock-insert-backlinks
                 denote-org-dblock-insert-missing-links
                 denote-org-dblock-insert-files-as-headings))
  '';
}
