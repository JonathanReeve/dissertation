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

    # CommonsDownloader = pkgs.python3Packages.buildPythonPackage rec {
    #   pname = "CommonsDownloader";
    #   version = "0.5.3";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "JonathanReeve";
    #     repo = "CommonsDownloader";
    #     rev = "0.5.5";
    #     sha256 = "0qi4068q74vj894cwnh6ki9w04293vzkqbd6dz67jllynph3wnw9";
    #   };
    #   postPatch = ''
    #     # Prevent errors that complain about missing argparse
    #     rm requirements.txt '';
    #   buildInputs = [ pkgs.python3Packages.mwclient ];
    #   doCheck = false;
    # };

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

    newAltair = pkgs.python3Packages.buildPythonPackage rec {
      pname = "altair";
      version = "4.1.0";

      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "0c99q5dy6f275yg1f137ird08wmwc1z8wmvjickkf2mvyka31p9y";
      };

      buildInputs = with pkgs.python3Packages; [
        entrypoints
        jinja2
        jsonschema
        numpy
        toolz
        pandas
      ];
      doCheck = false;

    };
in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
	    matplotlib
      spacy
      pandas
      # spacy_models.en_core_web_md
      spacy_models.en_core_web_lg
      jupyter
      scikitlearn
      nltk
      newAltair
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
      colorgram
      networkx
      retrying
      pillow
      # flake8  # Dev
      # python-language-server
      # pyls-mypy
      scikitlearn
      seaborn
      statsmodels
      beautifulsoup4
      #CommonsDownloader
      mwclient
    ];
  }).env
