%{
    #include "y.tab.h"
%}

alpha_ [A-Za-z_]
dig [0-9]
alphanum_ [a-zA-Z_0-9]
SP [ \t]
ID {alpha_}{alphanum_}*
NUM {dig}+

%%

"struct" {return STRUCT;}
"void" { return VOID;}
"unsigned"{SP}+"char" { return UCHR;}
"char" { return CHR;}
"unsigned"{SP}+"short"{SP}+"int" { return USRT;}
"unsigned"{SP}+"short" { return USRT;}
"short"{SP}+"int" { return SRT;}
"short" { return SRT;}
"long"{SP}+"int" { return LNG;}
"long" { return LNG;}
"unsigned"{SP}+"long"{SP}+"int" { return ULNG;}
"unsigned"{SP}+"long" { return ULNG;}
"int" { return INT;}
"unsigned"{SP}+"int" { return UINT;}
"unsigned" { return UINT;}
"float" { return FLT;}
"double" { return DBL;}
";" { return ';';}
"," { return ',';}
"*" { return '*';}
"[" { return '[';}
"]" { return ']';}
"{" { return '{';}
"}" { return '}';}

{ID} {
    yylval.str = strdup(yytext);
    return ID;
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