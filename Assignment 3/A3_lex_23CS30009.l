%{
%}

%x RHS ERROR
/* Created RHS exclusive state so that on RHS, an STR is not detected as ID */

alpha_ [A-Za-z_]
alpha [A-Za-z]
dig [0-9]
alphanum_ [a-zA-Z_0-9]

ID {alpha_}{alphanum_}*
NUM {dig}+
STR {alpha}+
REF [$]{ID}
LPN [ \t]*[(][ \t]*
RPN [ \t]*[)][ \t]*
CRT [ \t]*[\^][ \t]*
PLUS [ \t]*[+][ \t]*
ASG [ \t]*[=][ \t]*
EOL [\n]
SP [ \t]+
line [^\n]*[\n]

%%

<ERROR>{line} {
    BEGIN INITIAL;
    return 11;
    /* 11 is also the return value for spaces. I am returning 11 here as well because I want to ignore this yytext as well. */
}

{ID} {
    return 1;
}
<RHS>{NUM} {
    return 2;
}
<RHS>{STR} {
    return 3;
}
<RHS>{REF} {
    /* Assuming that if there is a whitespace after $, it is an error. */
    return 4;
}
<RHS>{LPN} {
    return 5;
}
<RHS>{RPN} {
    return 6;
}
<RHS>{CRT} {
    return 7;
}
<RHS>{PLUS} {
    return 8;
}
{ASG} {
    BEGIN RHS;
    return 9;
}
<INITIAL,RHS>{EOL} {
    BEGIN INITIAL;
    return 10;
}
<INITIAL,RHS>{SP} {
    return 11;
}
<INITIAL,RHS>. {
    BEGIN ERROR;
    return 12;
}

%%

int yywrap(){
    return 1;
}