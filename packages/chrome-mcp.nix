{ lib
, python313Packages
, fetchPypi
,
}:

python313Packages.buildPythonApplication rec {
  pname = "chroma-mcp";
  version = "0.2.6";
  pyproject = true;

  src = fetchPypi {
    pname = "chroma_mcp";
    inherit version;
    hash = "sha256-0fCX0jVo5TI13BDC+o7O+9Vj+C8cGIRpeLpoea+xnhc=";
  };

  nativeBuildInputs = with python313Packages; [
    pythonRelaxDepsHook
  ];

  build-system = with python313Packages; [
    hatchling
  ];

  dependencies = with python313Packages; [
    chromadb
    cohere
    httpx
    mcp
    openai
    pillow
    python-dotenv
    typing-extensions
    # voyageai - optional, not in nixpkgs
  ];

  # Skip tests - they require network access
  doCheck = false;

  # Relax version constraints - mcp 1.15.0 should work despite pin
  pythonRelaxDeps = [ "mcp" ];

  # Skip runtime deps check for voyageai (optional embedding provider)
  pythonRemoveDeps = [ "voyageai" ];

  pythonImportsCheck = [ "chroma_mcp" ];

  meta = {
    description = "MCP server for ChromaDB vector database";
    homepage = "https://github.com/chroma-core/chroma-mcp";
    license = lib.licenses.asl20;
    maintainers = [ ];
    mainProgram = "chroma-mcp";
  };
}
