%{
    #include "y.tab.h"
%}

alpha_ [A-Za-z_]
dig [0-9]
alphanum_ [a-zA-Z_0-9]
SP [ \t]
ID {alpha_}{alphanum_}*
NUM {dig}+
fltcons_temp ({NUM}\.{NUM})|({NUM}\.)|(\.{NUM})
fltcons {fltcons_temp}([eE][+-]?{NUM})?

%%

"struct" {return STRUCT;}
"long"{SP}+"int" { return LNG;}
"long" { return LNG;}
"int" { return INT;}
"float" { return FLT;}
"double" { return DBL;}
";" { return ';';}
"," { return ',';}
"[" { return '[';}
"]" { return ']';}
"{" { return '{';}
"}" { return '}';}
"." { return '.';}
"+" { return '+';}
"-" { return '-';}
"*" { return '*';}
"/" { return '/';}
"%" { return '%';}
"(" { return '(';}
")" { return ')';}
"==" {return EQ;}
"!=" {return NEQ;}
"<=" {return LEQ;}
">=" {return GEQ;}
"<" {return '<';}
">" {return '>';}
"!"  {return NOT;}
"&&" {return AND;}
"||" {return OR;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"=" { return '=';}


{ID} {
    yylval.str = strdup(yytext);
    return ID;
}
{fltcons} {
    yylval.ff = atof(yytext);
    return fltconst;
}
{NUM} {
    yylval.num = atoi(yytext);
    return NUM;
}

{SP} {}

"\n" {yylineno++;}

. {
    printf("*** Unexpected character: %c at line %d\n", yytext[0], yylineno);
}

%%

int yywrap(){
    return 1;
}