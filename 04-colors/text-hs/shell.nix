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
    spacy_conll = pkgs.python3Packages.buildPythonPackage rec {
      pname = "spacy_conll";
      version = "1.0.1";

      src = pkgs.python3Packages.fetchPypi{
        inherit version;
        inherit pname;
        sha256 = "1wffwp539i3yvqx6dl3ki5fmmbrpqpnf0ymg5806czk0rh7843j7";
      };

      buildInputs = [ pkgs.python3Packages.spacy pkgs.python3Packages.packaging ];
    };
    # pandoc = pkgs.haskellPackages.pandoc.override {
    #   version = "2.9.1.1";
    #   };

  in pkgs.mkShell {
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    buildInputs = with pkgs; [
        (python3.withPackages (ps: with ps; [
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
          plotly
          colormath
          falcon # Spacy server from Haskell Cookbook
          spacy_conll
          ]))
        (haskellPackages.ghcWithPackages (ps: with ps; [ lens pandoc roman-numerals doclayout ] ))
	    ];
	  }
  )
