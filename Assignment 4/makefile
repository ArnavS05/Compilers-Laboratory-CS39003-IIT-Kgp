CXX      = gcc
CXXFLAGS = -w
LEX_SRC  = A4_lex_23CS30009.l
YACC_SRC  = A4_yacc_23CS30009.y
TARGET   = a.out
INPUT    = input.txt


compile: $(TARGET)

$(TARGET): $(YACC_SRC) $(LEX_SRC)
	yacc -d $(YACC_SRC)
	lex $(LEX_SRC)
	$(CXX) $(CXXFLAGS) -o $(TARGET) y.tab.c lex.yy.c -ll


run: $(TARGET)
	./$(TARGET) $(INPUT)

clean:
	rm -f $(TARGET) lex.yy.c y.tab.c y.tab.h
