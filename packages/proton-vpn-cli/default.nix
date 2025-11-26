{
  buildPythonApplication,
  fetchFromGitHub,
  setuptools,
  lib,
  # Python dependencies
  proton-core,
  proton-vpn-api-core,
  proton-keyring-linux,
  proton-vpn-network-manager,
  proton-vpn-local-agent,
  click,
  dbus-fast,
  packaging,
# For tests (optional)
}:
buildPythonApplication rec {
  pname = "proton-vpn-cli";
  version = "0.1.2";

  pyproject = true;

  src = fetchFromGitHub {
    owner = "ProtonVPN";
    repo = "proton-vpn-cli";
    rev = "refs/heads/stable"; # No tags available, using stable branch
    hash = "sha256-przQVRGEqJRT+QQIyFwZqPduPeSI9ERoovJxdHx2aos=";
  };

  build-system = [ setuptools ];

  propagatedBuildInputs = [
    proton-core
    proton-vpn-api-core
    proton-keyring-linux
    proton-vpn-network-manager
    proton-vpn-local-agent
    click
    dbus-fast
    packaging
  ];

  # Disable tests for now as they may require additional setup
  doCheck = false;

  # Optional: Enable tests if needed
  # nativeCheckInputs = [
  #   pytestCheckHook
  #   pytest-asyncio
  # ];
  #
  # preCheck = ''
  #   export HOME=$TMPDIR
  # '';

  meta = {
    description = "Official Proton VPN command-line interface for Linux";
    homepage = "https://github.com/ProtonVPN/proton-vpn-cli";
    license = lib.licenses.gpl3Only;
    mainProgram = "protonvpn";
    platforms = lib.platforms.linux;
  };
}
