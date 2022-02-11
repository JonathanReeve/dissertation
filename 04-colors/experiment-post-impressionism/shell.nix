with import <nixpkgs> {};

( let
    colormath = pkgs.python3Packages.buildPythonPackage rec {
      pname = "colormath";
      version = "3.0.0";

      src = pkgs.python3Packages.fetchPypi{
        inherit version;
        inherit pname;
        sha256 = "05qjycgxp3p2f9n6lmic68sxmsyvgnnlyl4z9w7dl9s56jphaiix";
      };

      buildInputs = [ pkgs.python3Packages.numpy pkgs.python3Packages.networkx ];
    };

    colorgram = pkgs.python3Packages.buildPythonPackage rec {
      pname = "colorgram.py";
      version = "1.2.0";
      src = pkgs.python3Packages.fetchPypi{
        inherit version;
        inherit pname;
        sha256 = "1gzxgcmg3ndra2j4dg73x8q9dw6b0akj474gxyyhfwnyz6jncxz7";
      };
      buildInputs = [ pkgs.python3Packages.pillow pkgs.python3Packages.networkx ];
    };


in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
	    matplotlib
      spacy
      pandas
      spacy_models.en_core_web_lg
      jupyter
      scikitlearn
      nltk
      vega_datasets
      cherrypy
      dominate
      jupyterlab # Dev
      colorgram
      pillow
      scikitlearn
      seaborn
      statsmodels
      beautifulsoup4
      mwclient
      opencv4
    ];
  }).env
