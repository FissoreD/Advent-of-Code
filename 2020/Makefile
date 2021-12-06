BUILD = _build
SRC = src
LEX = $(BUILD)/lex.yy.c
YACC = $(BUILD)/y.tab.c
BACKUP = backup
VALID_EXT = {ml,c,py,l,y}

build: 
	ocamlc -I $(SRC) -c $(SRC)/lib.ml
	mv $(SRC)/*.cm*  $(BUILD) 

clean_all:
	rm -f *.out *.o 
	rm $(BUILD)/*

clean:
	rm -f *.out $(SRC)/*.o
	-mv $(SRC)/*.cm* $(BUILD) 

$(BUILD)/%.cmx: $(SRC)/%.ml
	ocamlopt $< -I $(SRC) -o $@

%.out: $(SRC)/%.ml $(SRC)/lib.ml
	ocamlopt str.cmxa $(SRC)/lib.ml $< -I $(SRC) -o $(BUILD)/$@
	rm -f $(SRC)/*.cm* $(SRC)/*.o 


$(BUILD)/%.yy.c: $(SRC)/%.l
	lex -o $(LEX) $<

$(BUILD)/%.tab.c: $(SRC)/%.y
	yacc -d $< -b $(BUILD)/y 

%.o: $(BUILD)/%.yy.c $(BUILD)/%.tab.c
	gcc $(LEX) $(YACC) -o $(BUILD)/$@ -ll 