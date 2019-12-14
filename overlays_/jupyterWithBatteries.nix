self: super: {
  jupyterWithBatteries = super.jupyter.override {
    python3 = super.python3.withPackages(ps: with ps; [ numpy scipy matplotlib ]);
  };
}
