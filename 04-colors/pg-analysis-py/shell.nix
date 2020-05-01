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

    newPlotly = pkgs.python3Packages.buildPythonPackage rec {
      pname = "plotly";
      version = "4.6.0";

      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "0br996lqbyq1prq9hhrzkgpicz5fgvxamzjrrpms20a2y1alkwv1";
      };

      buildInputs = with pkgs.python3Packages; [ decorator nbformat
                                                 pytz requests retrying six];
      doCheck = false;

    };

    newCufflinks = pkgs.python3Packages.buildPythonPackage rec {
      pname = "cufflinks";
      version = "0.17.3";

      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "0i56062k54dlg5iz3qyl1ykww62mpkp8jr4n450h0c60dm0b7ha8";
      };

      buildInputs = with pkgs.python3Packages; [ numpy pandas plotly six colorlover
                                                 setuptools ipython ipywidgets ];
      doCheck = false;

    };
in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
	    matplotlib
      spacy
      pandas
      spacy_models.en_core_web_md
      jupyter
      scikitlearn
      nltk
      altair
      vega_datasets
      cherrypy
      dominate
      # plotly
      newPlotly
      newCufflinks
      # chart-studio
      jupyterlab # Dev
      ipywidgets # Required by Cufflinks
      colorlover
      colormath
      networkx
      retrying
      # flake8  # Dev
      # python-language-server
      # pyls-mypy
      scikitlearn
      seaborn
      statsmodels
    ];
  }).env
