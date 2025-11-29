%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <ctype.h>
    void yyerror(char *);
    extern int yylineno;

    struct type{
        int element;    // For struct, its the symbol table number
        int dimension;
        int category;   // 0: basic, 1: Array, 2: Pointer, 3: Structure
        int size;
        char* name;
    };
    struct type TT[4096];    // Type table
    int add_type_ptr(int);
    int add_type_arr(int, int);
    int add_type_stru(char*, int);
    int ttentry = 11;
    
    struct stentry{
        char* name;
        int type;
        int offset;
    };
    struct pair{
        int t;
        int stnum;  // symbol table number
    };

    // Change 64 (int the next 3 lines) to some other number if more symbol tables (i.e., more structures) are needed
    struct stentry ST[64][1024];    // Symbol table
    int nstentry[64] = {0};
    int ST_size[64] = {0};

    int new_ST = 1;  // The index of the new symbol table to be used next

    int STindex ( char * , int);
    void STstore ( char *, int, int);
    int find_type(char *);

    extern FILE *yyin;
    extern int yylex();
%}

%union {
    int num;
    char* str;
    int typ;
    struct stentry *var;
    struct pair *p;
}
%token VOID UCHR CHR SRT USRT LNG ULNG UINT INT FLT DBL STRUCT
%type <typ> BASIC DIM N4
%type <var> VAR
%type <p> M1 M2 M3 M4 N5 N6 VARLIST
%token <num> NUM
%type <num> PROG DECLIST DECL N1 N2 N3
%token <str> ID
%start PROG

%%

PROG : N1 DECLIST
     ;
N1 : {$$ = 0;};
DECLIST : DECLIST N2 DECL
        | DECL
        ;
N2 : {$$ = $<num>-1;};
DECL : BASIC M1 VARLIST ';'
     | STRUCT ID N4 '{' N3 DECLIST '}' N5 ';'
     | STRUCT ID N4 '{' N3 DECLIST '}' N5 VARLIST ';'
     | STRUCT ID N6 VARLIST ';'
     ;
N3 : {$$ = new_ST++;} ;
N4 : {char* temp_name = (char*)malloc(strlen($<str>0)+32);
            temp_name[0] = '\0';
            strcat(temp_name, "struct ");
            strcat(temp_name, $<str>0);
            strcat(temp_name, " with symbol table ");
            char buf[5];
            sprintf(buf, "%d", new_ST);
            strcat(temp_name, buf); 
            $$ = add_type_stru(temp_name, new_ST);} ;
N5 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = $<typ>-4; $$->stnum = $<num>-7; TT[$$->t].size = ST_size[$<num>-2];};
N6 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = find_type($<str>0); $$->stnum = $<num>-2;};
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
M1 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = $<typ>0; $$->stnum = $<num>-1;};
VARLIST : VARLIST ',' M2 VAR {STstore($4->name, $4->type, ($3)->stnum);}
        | VAR {STstore($1->name, $1->type, ($<p>0)->stnum); $$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>0)->t; $$->stnum = ($<p>0)->stnum;}
        ;
M2 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>-1)->t; $$->stnum = ($<p>-1)->stnum;};
VAR : '*' M4 VAR { $$ = (struct stentry*)malloc(sizeof(struct stentry)); $$->type = $3->type; $$->name = strdup($3->name); }
    | ID M2 DIM { $$ = (struct stentry*)malloc(sizeof(struct stentry)); $$->name = strdup($1); $$->type = $3;}
    ;
DIM : '[' NUM ']' M3 DIM {$$ = add_type_arr($5, $2);}
    | {$$ = ($<p>0)->t;}
    ;
M3 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>-3)->t;};
M4 : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = add_type_ptr(($<p>-1)->t);};

%%

int find_type(char* name){
    for(int i=0; i<ttentry; i++){
        if (!strncmp(name,TT[i].name+7, strlen(name))) return i;
    }
    return -1;
}

int STindex(char *name, int st){
    int i;
    for (i=0; i<nstentry[st]; ++i)
    if (!strcmp(name,ST[st][i].name)) return i;
    return -1;
}

void STstore(char *name, int ty, int st)
{
    int i;
    i = STindex(name, st);
    if (i != -1) {
        printf("*** Redefinition of variable %s in line number %d. Ignoring.\n", name, yylineno);
        return;
    }

    if (TT[ty].size == -1){
        printf("*** Recursive definition of variable %s in line number %d. Ignoring.\n", name, yylineno);
        return;
    }
    ST[st][nstentry[st]].name = strdup(name);
    ST[st][nstentry[st]].type = ty;
    ST[st][nstentry[st]].offset = ST_size[st];
    if (TT[ty].size!=0) ST_size[st] += ((TT[ty].size-1)/4)*4+4;  // Align to 4 bytes
    ++nstentry[st];
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

int add_type_stru(char* name, int st){
    int i;
    for(i=0; i<ttentry; i++){
        if (TT[i].category == 3 && TT[i].element == st) return i;
    }
    TT[ttentry].element = st;
    TT[ttentry].dimension = 1;
    TT[ttentry].category = 3; // Structure
    TT[ttentry].name = (char*)malloc(strlen(name)+1);
    TT[ttentry].name[0] = '\0';
    strcat(TT[ttentry].name, name);
    TT[ttentry].size = -1; // Will be updated later, storing -1 to detect recursive definitions
    return ttentry++;
}



void yyerror(char* a){
    printf("Parsing error at line: %d\n", yylineno);
    exit(1);
}

void getFirstTwoWords(const char *str, char *out, size_t maxLen) {
    size_t j = 0;         // output index
    int wordCount = 0;
    int inWord = 0;

    for (size_t i = 0; str[i] != '\0' && j < maxLen - 1; i++) {
        if (!isspace((unsigned char)str[i])) {
            out[j++] = str[i];
            if (!inWord) {
                inWord = 1;
                wordCount++;
            }
        } else {
            if (inWord) {
                inWord = 0;
                if (wordCount >= 2) break;
                if (j < maxLen - 1) out[j++] = ' ';
            }
        }
    }

    // Remove trailing space
    if (j > 0 && out[j-1] == ' ') j--;
    out[j] = '\0';
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
    printf("\n+++ Symbol Table 0 [main]\n");
    // Find max name length
    int max_name_len = 0;
    for(int i=0; i<nstentry[0]; i++) {
        int len = strlen(ST[0][i].name);
        if(len > max_name_len) max_name_len = len;
    }
    max_name_len += 4; // 4 extra spaces

    // Find max digits in offset
    int max_offset = ST[0][nstentry[0]-1].offset;
    int digits_offset = 0;
    tmp = max_offset;
    if(tmp==0) digits_offset = 1;
    else { while(tmp) { digits_offset++; tmp/=10; } }

    // Find max digits in offset+size
    int max_end = ST[0][nstentry[0]-1].offset + TT[ST[0][nstentry[0]-1].type].size;
    int digits_end = 0;
    tmp = max_end;
    if(tmp==0) digits_end=1;
    else { while(tmp) { digits_end++; tmp/=10; } }

    for(int i=0; i<nstentry[0]; i++) {
        int end = ST[0][i].offset + TT[ST[0][i].type].size-1;
        if (TT[ST[0][i].type].size == 0){
            printf("\t%-*s%*d - %-*c    type = %4d = %s\n",
                max_name_len, ST[0][i].name,
                digits_offset, ST[0][i].offset,
                digits_end, 'x',                  // Printing 'x' for void type as it uses no space
                ST[0][i].type, TT[ST[0][i].type].name
            );
            continue;
        };
        printf("\t%-*s%*d - %-*d    type = %4d = %s\n",
            max_name_len, ST[0][i].name,
            digits_offset, ST[0][i].offset,
            digits_end, end,
            ST[0][i].type, TT[ST[0][i].type].name
        );
    }
    printf("\tTotal width = %d\n\n", ST_size[0]);


    for(int w = 0; w < ttentry; w++){
        if(TT[w].category!=3) continue;
        char* temp_name = (char*) malloc(strlen(TT[w].name)-10);
        getFirstTwoWords(TT[w].name, temp_name, strlen(temp_name));
        printf("\n+++ Symbol Table %d [%s]\n", TT[w].element, temp_name);
        // Find max name length
        int max_name_len = 0;
        int st = TT[w].element;
        for(int i=0; i<nstentry[st]; i++) {
            int len = strlen(ST[st][i].name);
            if(len > max_name_len) max_name_len = len;
        }
        max_name_len += 4; // 4 extra spaces

        // Find max digits in offset
        int max_offset = ST[st][nstentry[st]-1].offset;
        int digits_offset = 0;
        tmp = max_offset;
        if(tmp==0) digits_offset = 1;
        else { while(tmp) { digits_offset++; tmp/=10; } }

        // Find max digits in offset+size
        int max_end = ST[st][nstentry[st]-1].offset + TT[ST[st][nstentry[st]-1].type].size;
        int digits_end = 0;
        tmp = max_end;
        if(tmp==0) digits_end=1;
        else { while(tmp) { digits_end++; tmp/=10; } }

        for(int i=0; i<nstentry[st]; i++) {
            int end = ST[st][i].offset + TT[ST[st][i].type].size-1;
            if (TT[ST[st][i].type].size == 0){
                printf("\t%-*s%*d - %-*c    type = %4d = %s\n",
                    max_name_len, ST[st][i].name,
                    digits_offset, ST[st][i].offset,
                    digits_end, 'x',                  // Printing 'x' for void type as it uses no space
                    ST[st][i].type, TT[ST[st][i].type].name
                );
                continue;
            };
            printf("\t%-*s%*d - %-*d    type = %4d = %s\n",
                max_name_len, ST[st][i].name,
                digits_offset, ST[st][i].offset,
                digits_end, end,
                ST[st][i].type, TT[ST[st][i].type].name
            );
        }
        printf("\tTotal width = %d\n\n", ST_size[st]);
    }
    return 0;
}