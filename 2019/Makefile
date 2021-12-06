BUILD = _build
SRC = src
LEX = $(BUILD)/lex.yy.c
YACC = $(BUILD)/y.tab.c
BACKUP = backup
VALID_EXT = {ml,c,py,l,y}
WORD_SPLIT = $(word $2,$(subst $3, ,$1))

build: 
	ocamlc -c lib.ml
	mv *.cm*  $(BUILD) 

clean_all:
	rm -f *.out *.o 
	rm $(BUILD)/*

%.mlx: %.ml
	ocamlopt str.cmxa lib.ml $< -o $(BUILD)/$(call WORD_SPLIT,$@,1,/)_$(call WORD_SPLIT,$@,3,/)
	rm $(call WORD_SPLIT,$@,1,.).cm* $(call WORD_SPLIT,$@,1,.).o *.cm* *.o

%.lx: %.l
	lex -o $(LEX) $<

%.yx: %.y
	yacc -d $< -b $(BUILD)/y 

%.cx: %.yx %.lx
	gcc $(LEX) $(YACC) -o $(BUILD)/$(call WORD_SPLIT,$@,1,/)_$(call WORD_SPLIT,$@,3,/) -ll 