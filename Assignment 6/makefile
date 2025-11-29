CXX      = gcc
LEX_SRC  = ctype.l
YACC_SRC  = ctype.y
TARGET   = a.out
INPUT    = decl.c


compile: $(TARGET)

$(TARGET): $(YACC_SRC) $(LEX_SRC)
	yacc -d -Wno-other $(YACC_SRC)
	lex $(LEX_SRC)
	$(CXX) -o $(TARGET) y.tab.c lex.yy.c -ll


run: $(TARGET)
	./$(TARGET) $(INPUT)

clean:
	rm -f $(TARGET) lex.yy.c y.tab.c y.tab.h
