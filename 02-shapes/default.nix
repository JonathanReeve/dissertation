with import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-21.11";
  url = "https://github.com/nixos/nixpkgs/";
  # Commit hash for nixos-unstable as of 2018-09-12
  # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
  ref = "refs/heads/release-21.11";
  rev = "c254b8c915ac912ae9ee9dc74eac555ccbf33795";
}) {};

(let
    NRCLex = pkgs.python3Packages.buildPythonPackage rec {
      pname = "NRCLex";
      version = "3.0.0";
      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "pK6A4rUKFATW3PMnO0OI16OBHCDGx6+h+RdosrPMj6M=";
      };
      buildInputs = with pkgs.python3Packages; [ textblob nltk ];
      doCheck = false;
    };
    pywsd = pkgs.python3Packages.buildPythonPackage rec {
      pname = "pywsd";
      version = "1.2.4";
      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "wV+oCW0w+pZdOG+ogzmOxL+Hvn+L1f00ysNDOtW6QR0=";
      };
      buildInputs = with pkgs.python3Packages; [ nltk numpy pandas wn requests tomli six ];
      doCheck = false;
    };
    wn = pkgs.python3Packages.buildPythonPackage rec {
      pname = "wn";
      version = "0.0.23";
      # format = "flit";
      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "7uWwmjRmANLjPW5pETpTAoO7tCLwT5r2snfyykrVFL0=";
      };
      buildInputs = with pkgs.python3Packages; [ requests tomli ];
      doCheck = false;
    };
    textblob = pkgs.python3Packages.buildPythonPackage rec {
      pname = "textblob";
      version = "0.17.1";
      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "jcCHXfqx6vDcdyqdvEr6qcqT0ONc1iy3kvOjjgZ6to8=";
      };
      buildInputs = with pkgs.python3Packages; [ nltk ];
      doCheck = false;
    };
in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
      tomli
      matplotlib
      pandas
      jupyter
      nltk
      altair
      parsy
      plotly
      vega_datasets
      jupyterlab # Dev
      NRCLex
      spacy
      spacy_models.en_core_web_lg
      pywsd
      wn
      scikit-learn
    ];
}).env
