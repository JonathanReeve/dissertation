with import <nixpkgs> {};

( let

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
      pandas
      # plotly
      newPlotly
      newCufflinks
      retrying # Required by plotly
      # chart-studio
      # flake8  # Dev
      # python-language-server
      # pyls-mypy
    ];
  }).env
