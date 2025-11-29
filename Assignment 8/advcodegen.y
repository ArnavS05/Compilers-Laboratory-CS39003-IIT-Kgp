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
        int category;   // 0: basic, 1: Array, 3: Structure
        int size;
        char* name;
    };
    struct type TT[4096];    // Type table
    int add_type_arr(int, int);
    int add_type_stru(char*, int);
    int ttentry = 4;
    
    struct stentry{
        char* name;
        int type;
        int offset;
    };
    struct pair{
        int t;
        int stnum;  // symbol table number
    };

    struct node{
        int val;  //address of the instruction
        struct node *next;
    };

    struct addr{
        int category;   //0: intconst, 1: floatconst, 2: temp, 3: offset, 4: toffset
        // If its category 3, we print value as it is, else if it is category 4, we print t<tval> (eg. t7)
        int val_datatype;   //0: int, 1: long, 2: float, 3: double
        int type;    // type index from type table
        long int ival;
        double fval;
        int tval;    // temp reg number
    };

    struct sstmt{
        struct node *nextlist;
    };

    struct sbool{
        struct node *truelist;
        struct node *falselist;
    };

    // Change 64 (int the next 3 lines) to some other number if more symbol tables (i.e., more structures) are needed
    struct stentry ST[64][1024];    // Symbol table
    int nstentry[64] = {0};
    int ST_size[64] = {0};

    int new_ST = 1;  // The index of the new symbol table to be used next
    int new_temp = 1;  // Next temporary variable number

    char instructions[4096][64] = {0};
    int next_instruction = 0;
    int is_leader[4096] = {0};


    int STindex ( char * , int);
    void STstore ( char *, int, int);
    int find_type(char *);
    void print_addr(char*, struct addr*);
    void getFirstTwoWords(const char *str, char *out, size_t maxLen);

    struct node* makelist(int);
    struct node* merge(struct node*, struct node*);
    void backpatch(struct node*, int);

    extern FILE *yyin;
    extern int yylex();
%}

%union {
    int num;
    double ff;
    char* str;
    int typ;
    struct stentry *var;
    struct pair *p;
    struct addr *addrr;
    struct sstmt *stmt;
    struct sbool *bool;
}
%token LNG INT FLT DBL STRUCT EQ NEQ LEQ GEQ NOT AND OR IF ELSE WHILE
%left '+' '-'
%left '*' '/' '%'
%nonassoc NEG
%left OR
%left AND
%nonassoc NOT
%nonassoc ELSE
%token <ff> fltconst
%type <stmt> STMT STMTLIST N
%type <bool> BOOL
%type <typ> BASIC DIM N4
%type <var> VAR
%type <p> M1 M2 M3 N5 N6 VARLIST
%token <num> NUM
%type <num> PROG DECLIST DECL N1 N2 N3 O1 O2 RELOP M
%token <str> ID
%type <addrr> TERM FACTOR EXPR ITEM SMPLITEM AREF ASGN
%start PROG

%%

PROG        : N1 DECLIST {
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
                }} STMTLIST {backpatch($4->nextlist, next_instruction);}
            ;
N1          : {$$ = 0;};
DECLIST     : DECLIST N2 DECL
            | DECL
            ;
N2          : {$$ = $<num>-1;};
DECL        : BASIC M1 VARLIST ';'
            | STRUCT ID N4 '{' N3 DECLIST '}' N5 ';'
            | STRUCT ID N4 '{' N3 DECLIST '}' N5 VARLIST ';'
            | STRUCT ID N6 VARLIST ';'
            ;
N3          : {$$ = new_ST++;} ;
N4          : {
                char* temp_name = (char*)malloc(strlen($<str>0)+32);
                temp_name[0] = '\0';
                strcat(temp_name, "struct ");
                strcat(temp_name, $<str>0);
                strcat(temp_name, " with symbol table ");
                char buf[5];
                sprintf(buf, "%d", new_ST);
                strcat(temp_name, buf); 
                $$ = add_type_stru(temp_name, new_ST);}
            ;
N5          : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = $<typ>-4; $$->stnum = $<num>-7; TT[$$->t].size = ST_size[$<num>-2];};
N6          : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = find_type($<str>0); $$->stnum = $<num>-2;};

BASIC       : LNG { $$ = 1; }
            | INT { $$ = 0; }
            | FLT { $$ = 2; }
            | DBL { $$ = 3; }
            ;
M1          : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = $<typ>0; $$->stnum = $<num>-1;};
VARLIST     : VARLIST ',' M2 VAR {STstore($4->name, $4->type, ($3)->stnum);}
            | VAR {STstore($1->name, $1->type, ($<p>0)->stnum); $$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>0)->t; $$->stnum = ($<p>0)->stnum;}
            ;
M2          : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>-1)->t; $$->stnum = ($<p>-1)->stnum;};
VAR         : ID M2 DIM { $$ = (struct stentry*)malloc(sizeof(struct stentry)); $$->name = strdup($1); $$->type = $3;}
            ;
DIM         : '[' NUM ']' M3 DIM {$$ = add_type_arr($5, $2);}
            | {$$ = ($<p>0)->t;}
            ;
M3          : {$$ = (struct pair*)malloc(sizeof(struct pair)); $$->t = ($<p>-3)->t;};


STMTLIST    : {
                $$ = (struct sstmt*)malloc(sizeof(struct sstmt)); $$->nextlist = NULL;
                backpatch($$->nextlist, next_instruction);}
            | STMTLIST M STMT {
                backpatch($1->nextlist, $2);
                $$ = (struct sstmt*)malloc(sizeof(struct sstmt)); $$->nextlist = $3->nextlist;}
            ;

STMT        : ASGN {$$ = (struct sstmt*)malloc(sizeof(struct sstmt)); $$->nextlist = NULL;}
            | IF '(' BOOL ')' '{' M STMTLIST '}' {
                backpatch($3->truelist, $6);
                $$ = (struct sstmt*)malloc(sizeof(struct sstmt));
                $$->nextlist = merge($3->falselist, $7->nextlist);}
            | IF '(' BOOL ')' '{' M STMTLIST '}' N ELSE '{' M STMTLIST '}' {
                backpatch($3->truelist, $6); 
                backpatch($3->falselist, $12);
                $$ = (struct sstmt*)malloc(sizeof(struct sstmt));
                $$->nextlist = merge(merge($7->nextlist, $9->nextlist), $13->nextlist);}
            | WHILE M '(' BOOL ')' M '{' STMTLIST '}' {
                backpatch($8->nextlist, $2);
                backpatch($4->truelist, $6);
                $$ = (struct sstmt*)malloc(sizeof(struct sstmt));
                $$->nextlist = $4->falselist;
                sprintf(instructions[next_instruction], "     goto %d\n", $2);
                is_leader[$2] = 1;
                is_leader[next_instruction + 1] = 1;
                next_instruction++;
                }
            ;

M           : {$$ = next_instruction;};

N           : {
                struct node* temp = makelist(next_instruction);
                $$->nextlist = temp;
                sprintf(instructions[next_instruction], "     goto");
                next_instruction++;
                }
            ;

BOOL        : BOOL OR M BOOL {
                backpatch($1->falselist, $3);
                $$ = (struct sbool*)malloc(sizeof(struct sbool));
                $$->truelist = merge($1->truelist, $4->truelist);
                $$->falselist = $4->falselist;
                }
            | BOOL AND M BOOL {
                backpatch($1->truelist, $3);
                $$ = (struct sbool*)malloc(sizeof(struct sbool));
                $$->falselist = merge($1->falselist, $4->falselist);
                $$->truelist = $4->truelist;
                }
            | NOT BOOL {
                $$ = (struct sbool*)malloc(sizeof(struct sbool));
                $$->truelist = $2->falselist;
                $$->falselist = $2->truelist;
                }
            | '(' BOOL ')' {
                $$ = (struct sbool*)malloc(sizeof(struct sbool));
                $$->truelist = $2->truelist;
                $$->falselist = $2->falselist;
                }
            | EXPR RELOP EXPR {
                int t1 = -1;   // New temp var number for EXPR1 (if any)
                int t2 = -1;   // New temp var number for EXPR2 (if any)
                if ($1->type == $3->type){
                    // pass
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                }
                $$ = (struct sbool*)malloc(sizeof(struct sbool));
                $$->truelist = makelist(next_instruction);
                $$->falselist = makelist(next_instruction+1);
                sprintf(instructions[next_instruction], "     if ");
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                if ($2==0) strcat(instructions[next_instruction], " == ");
                else if ($2==1) strcat(instructions[next_instruction], " != ");
                else if ($2==2) strcat(instructions[next_instruction], " < ");
                else if ($2==3) strcat(instructions[next_instruction], " <= ");
                else if ($2==4) strcat(instructions[next_instruction], " > ");
                else if ($2==5) strcat(instructions[next_instruction], " >= ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], " goto");
                next_instruction++;
                sprintf(instructions[next_instruction], "     goto");
                next_instruction++;
                }
            ;

RELOP       : EQ {$$ = 0;}
            | NEQ {$$ = 1;}
            | '<' {$$ = 2;}
            | LEQ {$$ = 3;}
            | '>' {$$ = 4;}
            | GEQ {$$ = 5;}
            ;

ASGN        : ITEM '=' EXPR ';' {
                if ($1->type > 3){
                    printf("*** Error: invalid type of l-value in line number %d.\n", yylineno);
                }
                else if ($3->type > 3){
                    printf("*** Error: invalid type of r-value in line number %d.\n", yylineno);
                }
                else{
                    if ($1->type == $3->type){
                        if ($1->type == 0) sprintf(instructions[next_instruction], "[int]"); 
                        else if ($1->type == 1) strcat(instructions[next_instruction], "[lng]");
                        else if ($1->type == 2) strcat(instructions[next_instruction], "[flt]");
                        else if ($1->type == 3) strcat(instructions[next_instruction], "[dbl]");
                        strcat(instructions[next_instruction], "MEM(");
                        print_addr(instructions[next_instruction], $1);
                        char *end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                        sprintf(end, ",%d) = ", TT[$1->type].size);
                        print_addr(instructions[next_instruction], $3);
                        strcat(instructions[next_instruction], "\n");
                        next_instruction++;
                    }
                    else{
                        int t1 = new_temp++;
                        if ($1->type == 0) sprintf(instructions[next_instruction], "[int]"); 
                        else if ($1->type == 1) strcat(instructions[next_instruction], "[lng]");
                        else if ($1->type == 2) strcat(instructions[next_instruction], "[flt]");
                        else if ($1->type == 3) strcat(instructions[next_instruction], "[dbl]");
                        char *end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                        sprintf(end, "t%d = (", t1);
                        if ($3->type == 0) strcat(instructions[next_instruction], "int2"); 
                        else if ($3->type == 1) strcat(instructions[next_instruction], "lng2");
                        else if ($3->type == 2) strcat(instructions[next_instruction], "flt2");
                        else if ($3->type == 3) strcat(instructions[next_instruction], "dbl2");
                        if ($1->type == 0) strcat(instructions[next_instruction], "int)"); 
                        else if ($1->type == 1) strcat(instructions[next_instruction], "lng)");
                        else if ($1->type == 2) strcat(instructions[next_instruction], "flt)");
                        else if ($1->type == 3) strcat(instructions[next_instruction], "dbl)");
                        print_addr(instructions[next_instruction], $3);
                        strcat(instructions[next_instruction], "\n");
                        next_instruction++;
                        if ($1->type == 0) strcat(instructions[next_instruction], "[int]"); 
                        else if ($1->type == 1) strcat(instructions[next_instruction], "[lng]");
                        else if ($1->type == 2) strcat(instructions[next_instruction], "[flt]");
                        else if ($1->type == 3) strcat(instructions[next_instruction], "[dbl]");
                        strcat(instructions[next_instruction], "MEM(");
                        print_addr(instructions[next_instruction], $1);
                        end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                        sprintf(end, ",%d) = t%d\n", TT[$1->type].size, t1);
                        next_instruction++;
                    }
                }
                }
            ;

EXPR        : EXPR '+' TERM {
                $$ = (struct addr*)malloc(sizeof(struct addr));
                int t1 = -1;   // New temp var number for EXPR (if any)
                int t2=-1;   // New temp var number for TERM (if any)
                $$->category = 2; 
                if ($1->type == $3->type){
                    $$->val_datatype = $1->val_datatype; 
                    $$->type = $1->type;
                    $$->tval = new_temp++;
                    if ($$->val_datatype <2) $$->ival = $1->ival + $3->ival;
                    if ($$->val_datatype >1) $$->fval = $1->fval + $3->fval;
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = ", $$->tval);
                else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = ", $$->tval);
                else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = ", $$->tval);
                else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = ", $$->tval);
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " + ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], "\n");
                next_instruction++;
                }
            | EXPR '-' TERM {
                $$ = (struct addr*)malloc(sizeof(struct addr));
                int t1 = -1;   // New temp var number for EXPR (if any)
                int t2=-1;   // New temp var number for TERM (if any)
                $$->category = 2; 
                if ($1->type == $3->type){
                    $$->val_datatype = $1->val_datatype; 
                    $$->type = $1->type;
                    $$->tval = new_temp++;
                    if ($$->val_datatype <2) $$->ival = $1->ival + $3->ival;
                    if ($$->val_datatype >1) $$->fval = $1->fval + $3->fval;
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = ", $$->tval);
                else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = ", $$->tval);
                else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = ", $$->tval);
                else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = ", $$->tval);
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " - ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], "\n");
                next_instruction++;
                }
            | TERM {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = $1->category; $$->val_datatype = $1->val_datatype; $$->type = $1->type; if ($$->val_datatype <2) $$->ival = $1->ival; if ($$->val_datatype >1) $$->fval = $1->fval; $$->tval = $1->tval;}
            ;

TERM        : TERM '*' FACTOR {
                $$ = (struct addr*)malloc(sizeof(struct addr));
                int t1 = -1;   // New temp var number for EXPR (if any)
                int t2=-1;   // New temp var number for TERM (if any)
                $$->category = 2; 
                if ($1->type == $3->type){
                    $$->val_datatype = $1->val_datatype; 
                    $$->type = $1->type;
                    $$->tval = new_temp++;
                    if ($$->val_datatype <2) $$->ival = $1->ival + $3->ival;
                    if ($$->val_datatype >1) $$->fval = $1->fval + $3->fval;
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = ", $$->tval);
                else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = ", $$->tval);
                else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = ", $$->tval);
                else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = ", $$->tval);
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " * ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], "\n");
                next_instruction++;
                }
            | TERM '/' FACTOR {
                $$ = (struct addr*)malloc(sizeof(struct addr));
                int t1 = -1;   // New temp var number for EXPR (if any)
                int t2=-1;   // New temp var number for TERM (if any)
                $$->category = 2; 
                if ($1->type == $3->type){
                    $$->val_datatype = $1->val_datatype; 
                    $$->type = $1->type;
                    $$->tval = new_temp++;
                    if ($$->val_datatype <2) $$->ival = $1->ival + $3->ival;
                    if ($$->val_datatype >1) $$->fval = $1->fval + $3->fval;
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = ", $$->tval);
                else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = ", $$->tval);
                else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = ", $$->tval);
                else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = ", $$->tval);
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " / ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], "\n");
                next_instruction++;
                }
            | TERM '%' FACTOR {
                $$ = (struct addr*)malloc(sizeof(struct addr));
                int t1 = -1;   // New temp var number for EXPR (if any)
                int t2=-1;   // New temp var number for TERM (if any)
                $$->category = 2; 
                if ($1->type == $3->type){
                    $$->val_datatype = $1->val_datatype; 
                    $$->type = $1->type;
                    $$->tval = new_temp++;
                    if ($$->val_datatype <2) $$->ival = $1->ival + $3->ival;
                    if ($$->val_datatype >1) $$->fval = $1->fval + $3->fval;
                }
                else if ($1->type == 0 && $3->type == 1){ // int + long
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 2){ // int + float
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 0 && $3->type == 3){ // int + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 0){ // long + int
                    $$->val_datatype = 1; 
                    $$->type = 1;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[lng]t%d = (int2lng)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->ival = $1->ival + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 2){ // long + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 1 && $3->type == 3){ // long + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->ival + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 0){ // float + int
                    $$->val_datatype = 2; 
                    $$->type = 2;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[flt]t%d = (int2flt)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 1){ // float + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 2 && $3->type == 3){ // float + double
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t1 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t1);
                    print_addr(instructions[next_instruction], $1);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + int
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (int2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 2){ // double + long
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (lng2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->ival;
                    next_instruction++;
                }
                else if ($1->type == 3 && $3->type == 1){ // double + float
                    $$->val_datatype = 3; 
                    $$->type = 3;
                    t2 = new_temp++;
                    sprintf(instructions[next_instruction], "[dbl]t%d = (flt2dbl)", t2);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    $$->tval = new_temp++;
                    $$->fval = $1->fval + $3->fval;
                    next_instruction++;
                }
                if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = ", $$->tval);
                else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = ", $$->tval);
                else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = ", $$->tval);
                else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = ", $$->tval);
                char * end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t1 !=-1) sprintf(end, "t%d", t1);
                else print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " %% ");
                end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                if (t2 !=-1) sprintf(end, "t%d", t2);
                else print_addr(instructions[next_instruction], $3);
                strcat(instructions[next_instruction], "\n");
                next_instruction++;
                }
            | FACTOR {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = $1->category; $$->val_datatype = $1->val_datatype; $$->type = $1->type; if ($$->val_datatype <2) $$->ival = $1->ival; if ($$->val_datatype >1) $$->fval = $1->fval; $$->tval = $1->tval;}
            ;

FACTOR      : NUM {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 0; $$->val_datatype = 0; $$->type = 0; $$->ival = $1;}
            | '-' NUM %prec NEG {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 0; $$->val_datatype = 0; $$->type = 0; $$->ival = -$2;}
            | fltconst {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 1; $$->val_datatype = 3; $$->type = 3; $$->fval = $1;}
            | '-' fltconst %prec NEG {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 1; $$->val_datatype = 3; $$->type = 3; $$->fval = -$2;}
            | ITEM {
                $$ = (struct addr*)malloc(sizeof(struct addr)); 
                if ($$->val_datatype <2) $$->ival = $1->ival; 
                else if ($$->val_datatype >1) $$->fval = $1->fval;
                if ($1->category<=1) $$->category = $1->category; 
                else $$->category = 2;
                $$->type = $1->type; 
                $$->val_datatype = $1->type; 
                if ($1->category > 2){
                    if ($$->type > 3){
                        printf("*** Error: Invalid type for dereferencing in line number %d. Setting value to 0.\n", yylineno);
                        $$->ival = 0;
                        $$->fval = 0.0;
                    }
                    else {
                        $$->tval = new_temp++;
                        if ($$->type == 0) sprintf(instructions[next_instruction], "[int]t%d = MEM(", $$->tval);
                        else if ($$->type == 1) sprintf(instructions[next_instruction], "[lng]t%d = MEM(", $$->tval);
                        else if ($$->type == 2) sprintf(instructions[next_instruction], "[flt]t%d = MEM(", $$->tval);
                        else if ($$->type == 3) sprintf(instructions[next_instruction], "[dbl]t%d = MEM(", $$->tval);
                        print_addr(instructions[next_instruction], $1);
                        char *end = instructions[next_instruction] + strlen(instructions[next_instruction]);
                        sprintf(end, ", %d)\n", TT[$$->type].size);
                        next_instruction++;
                    }
                }
                }
            | '(' EXPR ')' {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = $2->category; $$->val_datatype = $2->val_datatype; $$->type = $2->type; if ($$->val_datatype <2) $$->ival = $2->ival; if ($$->val_datatype >1) $$->fval = $2->fval; $$->tval = $2->tval;}  // intconst is same as NUM
            ;         

ITEM        : O1 SMPLITEM {
                $$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = $2->category; 
                $$->val_datatype = $2->val_datatype; $$->type = $2->type; $$->ival = $2->ival; $$->tval = $2->tval;}
            | ITEM '.' O2 SMPLITEM {
                $$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 4; 
                $$->val_datatype = $4->val_datatype; 
                $$->type = $4->type; 
                $$->ival = $4->ival + $1->ival;
                sprintf(instructions[next_instruction], "[int]t%d = ", new_temp);
                print_addr(instructions[next_instruction], $1);
                strcat(instructions[next_instruction], " + ");
                print_addr(instructions[next_instruction], $4);
                strcat(instructions[next_instruction], "\n");
                $$->tval = new_temp++;
                next_instruction++;
                }
            ;

O1          : {$$ = 0;};
O2          : {$$ = TT[($<addrr>-1)->type].element;};

SMPLITEM    : ID {
                $$ = (struct addr*)malloc(sizeof(struct addr)); 
                int st = $<num>0; 
                int index = STindex($1, st);
                if (index == -1) { 
                    printf("*** Error: Undeclared variable %s in line number %d. Exiting.\n", $1, yylineno); 
                    exit(1); 
                } 
                $$->category = 3; 
                $$->val_datatype = 0; 
                $$->type = ST[st][index].type;
                $$->ival = ST[st][index].offset; }
            | AREF {$$ = (struct addr*)malloc(sizeof(struct addr)); $$->category = 4; $$->val_datatype = $1->val_datatype; $$->type = $1->type; $$->ival = $1->ival; $$->tval = $1->tval;}
            ;
AREF        : AREF '[' EXPR ']' {
                $$ = (struct addr*)malloc(sizeof(struct addr)); 
                int element_type = TT[$1->type].element; 
                $$->category = 4; 
                $$->val_datatype = 0; 
                $$->type = element_type;
                $$->ival = $1->ival + $3->ival * TT[element_type].size;
                if($3->type !=0){
                    sprintf(instructions[next_instruction], "[int]t%d = ", new_temp++);
                    if ($3->type == 1) strcat(instructions[next_instruction], "(lng2int)");
                    else if ($3->type == 2) strcat(instructions[next_instruction], "(flt2int)");
                    else if ($3->type == 3) strcat(instructions[next_instruction], "(dbl2int)");
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = %d * t%d\n", new_temp++, TT[element_type].size, new_temp-1);
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = t%d + t%d\n", new_temp, $1->tval, new_temp-1);
                    $$->tval = new_temp++;
                    next_instruction++;
                }
                else {
                    sprintf(instructions[next_instruction], "[int]t%d = %d * ", new_temp++, TT[element_type].size);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = t%d + t%d\n", new_temp, $1->tval, new_temp-1);
                    $$->tval = new_temp++;
                    next_instruction++;
                }
                }
            | ID '[' EXPR ']' {
                $$ = (struct addr*)malloc(sizeof(struct addr)); 
                int st = $<num>0; 
                int index = STindex($1, st); 
                if (index == -1) { 
                    printf("*** Error: Undeclared variable %s in line number %d. Exiting.\n", $1, yylineno); 
                    exit(1); 
                } 
                int type_index = ST[st][index].type;
                int element_type = TT[type_index].element; 
                $$->category = 4; 
                $$->val_datatype = 0; 
                $$->type = element_type; 
                $$->ival = ST[st][index].offset + $3->ival * TT[element_type].size;
                if($3->type !=0){
                    sprintf(instructions[next_instruction], "[int]t%d = ", new_temp++);
                    if ($3->type == 1) strcat(instructions[next_instruction], "(lng2int)");
                    else if ($3->type == 2) strcat(instructions[next_instruction], "(flt2int)");
                    else if ($3->type == 3) strcat(instructions[next_instruction], "(dbl2int)");
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = %d * t%d\n", new_temp++, TT[element_type].size, new_temp-1);
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = %d + t%d\n", new_temp, ST[st][index].offset, new_temp-1);
                    $$->tval = new_temp++;
                    next_instruction++;
                }
                else {
                    sprintf(instructions[next_instruction], "[int]t%d = %d * ", new_temp++, TT[element_type].size);
                    print_addr(instructions[next_instruction], $3);
                    strcat(instructions[next_instruction], "\n");
                    next_instruction++;
                    sprintf(instructions[next_instruction], "[int]t%d = %d + t%d\n", new_temp, ST[st][index].offset, new_temp-1);
                    $$->tval = new_temp++;
                    next_instruction++;
                }
                }
            ;



%%


void print_addr(char* buff, struct addr* a){
    char *end = buff + strlen(buff);

    if (a->category == 2 || a->category == 4){
        sprintf(end, "t%d", a->tval);
    }
    else if (a->category == 3){
        sprintf(end, "%ld", a->ival);
    }
    else if (a->category == 0){
        sprintf(end, "%ld", a->ival);
    }
    else if (a->category == 1){
        sprintf(end, "%.16lf", a->fval);
    }
    return;
}

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

void STstore(char *name, int ty, int st){
    int i;
    i = STindex(name, st);
    if (i != -1) {
        printf("*** Error: Redefinition of variable %s in line number %d. Ignoring.\n", name, yylineno);
        return;
    }

    if (TT[ty].size == -1){
        printf("*** Error: Recursive definition of variable %s in line number %d. Ignoring.\n", name, yylineno);
        return;
    }
    ST[st][nstentry[st]].name = strdup(name);
    ST[st][nstentry[st]].type = ty;
    ST[st][nstentry[st]].offset = ST_size[st];
    if (TT[ty].size!=0) ST_size[st] += ((TT[ty].size-1)/4)*4+4;  // Align to 4 bytes
    ++nstentry[st];
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

struct node* makelist(int a){
    struct node* temp = (struct node*)malloc(sizeof(struct node));
    temp->val = a;
    temp->next = NULL;
    return temp;
}

struct node* merge(struct node* a, struct node* b){
    if (a == NULL) return b;
    struct node* temp = a;
    while (temp->next != NULL) temp = temp->next;
    temp->next = b;
    return a;
}

void backpatch(struct node* a, int b){
    struct node* temp = a;
    if (temp!=NULL) is_leader[b] = 1;
    while (temp != NULL){
        is_leader[(temp->val)+1] = 1;
        char* end = instructions[temp->val] + strlen(instructions[temp->val]);
        sprintf(end, " %d\n", b);
        temp = temp->next;
    }
    return;
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
    TT[1].element = 1; TT[1].dimension = 1; TT[1].category = 0; TT[1].name = "long"; TT[1].size = sizeof(long);
    TT[0].element = 0; TT[0].dimension = 1; TT[0].category = 0; TT[0].name = "int"; TT[0].size = sizeof(int);
    TT[2].element = 2; TT[2].dimension = 1; TT[2].category = 0; TT[2].name = "float"; TT[2].size = sizeof(float);
    TT[3].element = 3; TT[3].dimension = 1; TT[3].category = 0; TT[3].name = "double"; TT[3].size = sizeof(double);

    is_leader[0] = 1; // First instruction is always a leader    

    if (argc>1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            printf("Could not open file: %s\n", argv[1]);
            return 1;
        }
        yyin = file;
    }
    yyparse();
    for (int i=0; i<=next_instruction; i++){
        if (is_leader[i]==1) printf("\n");
        printf("    %4d  :  %s", i, instructions[i]);
    }
    printf("\n");
    return 0;
}