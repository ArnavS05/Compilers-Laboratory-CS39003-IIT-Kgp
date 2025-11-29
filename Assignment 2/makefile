CXX      = g++
CXXFLAGS = -w
LEX      = lex
LEX_SRC  = A2_lex_23CS30009.l
CPP_SRC  = A2_parser_23CS30009.cpp
TARGET   = a.out
INPUT    = input.txt


all: $(TARGET)

$(TARGET): lex.yy.c $(CPP_SRC)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(CPP_SRC)

lex.yy.c: $(LEX_SRC)
	$(LEX) $(LEX_SRC)

run: $(TARGET)
	./$(TARGET) $(INPUT)

clean:
	rm -f $(TARGET) lex.yy.c
