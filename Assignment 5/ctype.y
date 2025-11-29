%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <ctype.h>
    void yyerror(char *);
    extern int yylineno;

    struct type{
        int element;
        int dimension;
        int category;   // 0: basic, 1: Array, 2: Pointer
        int size;
        char* name;
    };
    struct type TT[4096];    // Type table
    int add_type_ptr(int);
    int add_type_arr(int, int);
    int ttentry = 11;

    struct stentry{
        char* name;
        int type;
        int offset;
    };
    struct stentry ST[4096];    // Symbol table
    int nstentry = 0;

    int ST_size=0;

    int STindex ( char * );
    void STstore ( char *, int);

    extern FILE *yyin;
    extern int yylex();
%}

%union {
    int num;
    char* str;
    int typ;
    struct stentry *var;
}
%token VOID UCHR CHR SRT USRT LNG ULNG UINT INT FLT DBL
%type <typ> BASIC VARLIST M1 M2 M3 M4 DIM PROG DECLIST DECL
%type <var> VAR
%token <num> NUM
%token <str> ID
%start PROG

%%

PROG : DECLIST
    ;
DECLIST : DECLIST DECL
        | DECL
        ;
DECL : BASIC M1 VARLIST ';'
     ;
BASIC : VOID { $$ = 0;}
      | UCHR { $$ = 1;}
      | CHR { $$ = 2;}
      | SRT { $$ = 4;}
      | USRT { $$ = 3; }
      | LNG { $$ = 6; }
      | ULNG { $$ = 5; }
      | UINT { $$ = 7; }
      | INT { $$ = 8; }
      | FLT { $$ = 9; }
      | DBL { $$ = 10; }
      ;
M1 : {$$ = $<typ>0;};
VARLIST : VARLIST ',' M2 VAR {STstore($4->name, $4->type);}
        | VAR {STstore($1->name, $1->type); $$ = $<typ>0;}
        ;
M2 : {$$ = $<typ>-1;};
VAR : '*' M4 VAR { $$ = (struct stentry*)malloc(sizeof(struct stentry)); $$->type = $3->type; $$->name = strdup($3->name); }
    | ID M2 DIM { $$ = (struct stentry*)malloc(sizeof(struct stentry)); $$->name = strdup($1); $$->type = $3;}
    ;
DIM : '[' NUM ']' M3 DIM {$$ = add_type_arr($5, $2);}
    | {$$ = $<typ>0;}
    ;
M3 : {$$ = $<typ>-3;};
M4 : {$$ = add_type_ptr($<typ>-1);};

%%

int STindex ( char *name){
    int i;
    for (i=0; i<nstentry; ++i)
    if (!strcmp(name,ST[i].name)) return i;
    return -1;
}

void STstore ( char *name, int ty)
{
    int i;
    i = STindex(name);
    if (i != -1) {
        printf("*** Redefinition of variable %s in line number %d. Ignoring.\n", name, yylineno);
        return;
    }
    ST[nstentry].name = strdup(name);
    ST[nstentry].type = ty;
    ST[nstentry].offset = ST_size;
    if (TT[ty].size!=0) ST_size += ((TT[ty].size-1)/4)*4+4;  // Align to 4 bytes
    ++nstentry;
}

int add_type_ptr(int base){
    int i;
    for(i=0; i<ttentry; i++){
        if (TT[i].category == 2 && TT[i].element == base) return i;
    }
    TT[ttentry].element = base;
    TT[ttentry].dimension = 1;
    TT[ttentry].category = 2; // Pointer
    TT[ttentry].name = (char*)malloc(strlen(TT[base].name)+10);
    TT[ttentry].name[0] = '\0';
    strcat(TT[ttentry].name, "pointer(");
    strcat(TT[ttentry].name, TT[base].name);
    strcat(TT[ttentry].name, ")");
    TT[ttentry].size = sizeof(void*);
    return ttentry++;
}

int add_type_arr(int base, int dim){
    int i;
    for(i=0; i<ttentry; i++){
        if (TT[i].category == 1 && TT[i].element == base && TT[i].dimension == dim) return i;
    }
    TT[ttentry].element = base;
    TT[ttentry].dimension = dim;
    TT[ttentry].category = 1; // Array
    TT[ttentry].name = (char*)malloc(strlen(TT[base].name)+50);
    TT[ttentry].name[0] = '\0';
    strcat(TT[ttentry].name, "array(");
    char buf[32];
    sprintf(buf, "%d", dim);
    strcat(TT[ttentry].name, buf);
    strcat(TT[ttentry].name, ",");
    strcat(TT[ttentry].name, TT[base].name);
    strcat(TT[ttentry].name, ")");
    TT[ttentry].size = TT[base].size * dim;
    return ttentry++;
}



void yyerror(char * a){
    printf("Parsing error at line: %d\n", yylineno);
    exit(1);
}

int main(int argc, char* argv[]) {
    TT[0].element = 0; TT[0].dimension = 0; TT[0].category = 0; TT[0].name = "void"; TT[0].size = 0;
    TT[1].element = 1; TT[1].dimension = 1; TT[1].category = 0; TT[1].name = "unsigned char"; TT[1].size = sizeof(unsigned char);
    TT[2].element = 2; TT[2].dimension = 1; TT[2].category = 0; TT[2].name = "char"; TT[2].size =  sizeof(char);
    TT[3].element = 3; TT[3].dimension = 1; TT[3].category = 0; TT[3].name = "unsigned short"; TT[3].size = sizeof(unsigned short);
    TT[4].element = 4; TT[4].dimension = 1; TT[4].category = 0; TT[4].name = "short"; TT[4].size = sizeof(short);
    TT[5].element = 5; TT[5].dimension = 1; TT[5].category = 0; TT[5].name = "unsigned long"; TT[5].size = sizeof(unsigned long);
    TT[6].element = 6; TT[6].dimension = 1; TT[6].category = 0; TT[6].name = "long"; TT[6].size = sizeof(long);
    TT[7].element = 7; TT[7].dimension = 1; TT[7].category = 0; TT[7].name = "unsigned int"; TT[7].size = sizeof(unsigned int);
    TT[8].element = 8; TT[8].dimension = 1; TT[8].category = 0; TT[8].name = "int"; TT[8].size = sizeof(int);
    TT[9].element = 9; TT[9].dimension = 1; TT[9].category = 0; TT[9].name = "float"; TT[9].size = sizeof(float);
    TT[10].element = 10; TT[10].dimension = 1; TT[10].category = 0; TT[10].name = "double"; TT[10].size = sizeof(double);

    if (argc>1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            printf("Could not open file: %s\n", argv[1]);
            return 1;
        }
        yyin = file;
    }
    yyparse();

    printf("+++ All declarations read\n\n");

    // Print Type Table
    printf("+++ %d types\n", ttentry);
    int max_size = 0;
    for(int i = 0; i < ttentry; i++) {
        if(TT[i].size > max_size) max_size = TT[i].size;
    }

    int digits = 0;
    int tmp = max_size;
    if(tmp == 0) digits = 1;
    else {
        while(tmp) { digits++; tmp/=10; }
    }
    digits += 2; // 2 extra spaces

    // Print header
    printf("Type Table:\n");
    for(int i = 0; i < ttentry; i++) {
        printf("\tType %4d:    %*d        %s\n", i, digits, TT[i].size, TT[i].name);
    }

    // Print Symbol Table
    printf("\n+++ Symbol Table\n");
    // Find max name length
    int max_name_len = 0;
    for(int i=0; i<nstentry; i++) {
        int len = strlen(ST[i].name);
        if(len > max_name_len) max_name_len = len;
    }
    max_name_len += 4; // 4 extra spaces

    // Find max digits in offset
    int max_offset = ST[nstentry-1].offset;
    int digits_offset = 0;
    tmp = max_offset;
    if(tmp==0) digits_offset = 1;
    else { while(tmp) { digits_offset++; tmp/=10; } }

    // Find max digits in offset+size
    int max_end = ST[nstentry-1].offset + TT[ST[nstentry-1].type].size;
    int digits_end = 0;
    tmp = max_end;
    if(tmp==0) digits_end=1;
    else { while(tmp) { digits_end++; tmp/=10; } }

    for(int i=0; i<nstentry; i++) {
        int end = ST[i].offset + TT[ST[i].type].size-1;
        if (TT[ST[i].type].size == 0){
            printf("\t%-*s%*d - %-*c    type = %4d = %s\n",
                max_name_len, ST[i].name,
                digits_offset, ST[i].offset,
                digits_end, 'x',                  // Printing 'x' for void type as it uses no space
                ST[i].type, TT[ST[i].type].name
            );
            continue;
        };
        printf("\t%-*s%*d - %-*d    type = %4d = %s\n",
            max_name_len, ST[i].name,
            digits_offset, ST[i].offset,
            digits_end, end,
            ST[i].type, TT[ST[i].type].name
        );
    }
    printf("Total width = %d\n", ST_size);
    return 0;
}