.PHONY : clean nuke

SOLN=refinements.sln
PKG_DIR=packages
SRC_DIR=refinements

FSLEX=$(PKG_DIR)/FsLexYacc.6.1.0/build/fslex.exe
FSYACC=$(PKG_DIR)/FsLexYacc.6.1.0/build/fsyacc.exe

LEXER=$(SRC_DIR)/Lexer.fs
PARSER=$(SRC_DIR)/Parser.fs

negation.exe : $(PKG_DIR) $(LEXER) $(PARSER)
	xbuild $(SOLN) /p:Configuration=Release

$(LEXER) : $(SRC_DIR)/Lexer.fsl $(PKG_DIR)
	mono $(FSLEX) -o $@ --unicode $<

$(PARSER) : $(SRC_DIR)/Parser.fsy $(LEXER) $(PKG_DIR)
	mono $(FSYACC) -o $@ --module Parser -v $<

$(PKG_DIR) :
	nuget restore refinements.sln

clean :
	xbuild $(SOLN) /t:clean
	rm -rf $(SRC_DIR)/Parser.fsi $(SRC_DIR)/Parser.fsyacc.output

nuke : clean
	rm -rf packages
