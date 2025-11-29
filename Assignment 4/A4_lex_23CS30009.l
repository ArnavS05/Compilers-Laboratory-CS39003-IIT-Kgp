%{
    #include "y.tab.h"
%}

%x RHS
/* Created RHS exclusive state so that on RHS, an STR is not detected as ID */

alpha_ [A-Za-z_]
alpha [A-Za-z]
dig [0-9]
alphanum_ [a-zA-Z_0-9]

ID {alpha_}{alphanum_}*
NUM {dig}+
STR {alpha}+
REF [$]{ID}
LPN [(]
RPN [)]
CRT [\^]
PLUS [+]
ASG [=]
EOL [\n]
SP [ \t]+
MINUS [\-]
PRE [<]
SUF [>]
RSQ [\[]
LSQ [\]]
COMMA [,]

%%

{ID} {
    yylval.str = strdup(yytext);
    return ID;
}
<RHS>{NUM} {
    yylval.num = atoi(yytext);
    return NUM;
}
<RHS>{STR} {
    yylval.str = strdup(yytext);
    return STR;
}
<RHS>{REF} {
    yylval.str = strdup(yytext+1);
    /* Assuming that if there is a whitespace after $, it is an error. */
    return REF;
}
<RHS>{LPN} {
    return yytext[0];
}
<RHS>{RPN} {
    return yytext[0];
}
<RHS>{CRT} {
    return yytext[0];
}
<RHS>{PLUS} {
    return yytext[0];
}
{ASG} {
    BEGIN RHS;
    return yytext[0];
}
<RHS>{MINUS} {
    return yytext[0];
}
<RHS>{LSQ} {
    return yytext[0];
}
<RHS>{RSQ} {
    return yytext[0];
}
<RHS>{PRE} {
    return yytext[0];
}
<RHS>{SUF} {
    return yytext[0];
}
<RHS>{COMMA} {
    return yytext[0];
}
<RHS>{EOL} {
    BEGIN INITIAL;
    yylineno++;
    return yytext[0];
}
<INITIAL>{EOL} {
    BEGIN INITIAL;
    yylineno++;
    printf("\n");
}
<INITIAL,RHS>{SP} {
}
<INITIAL,RHS>. {
    printf("*** Unexpected character: %c at line %d\n", yytext[0], yylineno);
}

%%

int yywrap(){
    return 1;
}