%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <ctype.h>
    void yyerror(char *);
    extern int yylineno;

    typedef struct {
        char* name;
        char* value;
    } stpair;
    stpair ST[4096];
    int nstentry = 0;
    int STindex ( char * );
    char* STload ( char * );
    void STstore ( char *, char*);

    typedef struct pair_{
        int first;
        int second;
    } pair;

    char* strplus(char* a, char* b);
    char* strsub(char* a, char* b);
    char* strxpnt(char* a, int b);
    char* strrange(char* a, pair* b);

    extern FILE *yyin;
    extern int yylex();
%}

%union {
    int num;
    char* str;
    struct pair* range; // first -> start pos, second -> end pos
}
%token <num> NUM
%type <num> XPNT
%token <str> STR REF ID
%type <str> PROG LINE EXPR TERM BASE
%type <range> RANGE
%start PROG
%left '+' '-'
%left '^' '['

%%

PROG: LINE '\n' {yylineno++;}
    | LINE '\n' PROG {yylineno++;}
    ;
LINE: ID '=' EXPR {STstore($1,$3); printf("\tStored %s = %s\n", $1, $3);}
    ;
EXPR: EXPR '+' TERM {$$ = strplus($1, $3);}
    | EXPR '-' TERM {$$ = strsub($1, $3);}
    | TERM {$$ = $1;}
    ;
TERM: TERM '[' RANGE ']' { $$ = strrange($1, $3); }
    | TERM '^' XPNT { $$ = strxpnt($1, $3); }
    | BASE { $$ = $1;}
    ;
BASE: STR { $$ = $1;}
    | REF { $$ = STload($1); }
    | '(' EXPR ')' { $$ = $2;}
    ;

RANGE: NUM {
    pair* p = (pair*)malloc(sizeof(pair));
    p->first = $1;
    p->second = $1;
    $$ = p;
    }
    | NUM ',' NUM {
    pair* p = (pair*)malloc(sizeof(pair));
    p->first = $1;
    p->second = $3;
    $$ = p;
    }
    | '<' NUM {
    pair* p = (pair*)malloc(sizeof(pair));
    p->first = 0;
    p->second = $2 - 1;
    $$ = p;
    }
    | '>' NUM {
    pair* p = (pair*)malloc(sizeof(pair));
    p->first = -$2;
    p->second = -1;
    $$ = p;
    }
    ;

XPNT: NUM {$$ = $1;}
    ;

%%

int STindex ( char *name){
    int i;
    for (i=0; i<nstentry; ++i)
    if (!strcmp(name,ST[i].name)) return i;
    return -1;
}

char* STload ( char *name){
    int i;
    i = STindex(name);
    if (i == -1) {
        printf("*** Undefined variable %s\n", name);
        return strdup("");
    }
    return strdup(ST[i].value);
}

void STstore ( char *name, char* value )
{
    int i;
    i = STindex(name);
    if (i == -1) {
        i = nstentry;
        ST[i].name = strdup(name);
        ++nstentry;
    }
    ST[i].value = strdup(value);
}

char* strplus(char* a, char* b){
    char* res = (char*)malloc(strlen(a)+strlen(b)+1);
    strcpy(res, a);
    strcat(res, b);
    return res;
}

char* strsub(char* a, char* b) {
    int len_a = strlen(a);
    int len_b = strlen(b);

    int i = 0; // index for a
    int j = 0; // index for b

    // Find how many characters from b can be matched as subsequence in a
    while (i < len_a && j < len_b) {
        if (a[i] == b[j]) j++;
        i++;
    }

    char* res = (char*)malloc(len_a - j + 1);
    int res_idx = 0;
    i = 0;
    j = 0;
    while (i < len_a) {
        if (j < len_b && a[i] == b[j]) j++;
        else res[res_idx++] = a[i];
        i++;
    }
    res[res_idx] = '\0';

    return res;
}


char* strxpnt(char* a, int b){
    if(b==0) return strdup("");
    char* res = (char*)malloc(strlen(a)*b+1);
    res[0]='\0';
    for(int i=0; i<b; ++i) strcat(res, a);
    return strdup(res);
}

char* strrange(char* a, pair* b){
    int len_a = strlen(a);
    int start = b->first;
    int end = b->second;
    if(start==end && start>=len_a) return strdup("");
    if (start > end) return strdup("");
    if (end==-1) end = len_a - 1;
    if (start<0) start = len_a + start;
    if(start<0) start=0;   //Since after the above operation, start can be negative
    if(start>=len_a) return strdup("");
    if (end >= len_a) end = len_a - 1;
    char* res = (char*)malloc(end - start + 2);
    strncpy(res, a + start, end - start + 1);
    res[end - start + 1] = '\0';
    return strdup(res);
}

void yyerror(char *){
    printf("Parsing error at line: %d.\n", yylineno);
    exit(1);
}

int main(int argc, char* argv[]) {
    if (argc>1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            printf("Could not open file: %s\n", argv[1]);
            return 1;
        }
        yyin = file;
    }
    yyparse();
    return 0;
}