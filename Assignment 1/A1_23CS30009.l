%{
    #include <bits/stdc++.h>
    using namespace std;
    int lineno=1;
    map<string, string> var_table;
    string temp_lhs = "";
    string temp_rhs = "";
    string temp = "";
    int l;
%}

%x LHS STR RANGE PREFIX SUFFIX FULLRR EXPONENT ERROR VAR AFTERVAR AFTEREXP AFTERRANGE AFTERAFTERRANGE

alpha_ [A-Za-z_]
alpha [A-Za-z]
num [0-9]
notalpha_ [^A-Za-z_]
alphanum_ {alpha_}|[0-9]
newl [ \t]*\n
sptab [ \t]+
alphastring {alpha}+
eq [ \t]*[=][ \t]*
line [^\n]*[\n]
dot [ \t]*[\.][ \t]*
dollar [ \t]*[$][ \t]*
openbracket [ \t]*[\[][ \t]*
closebracket [ \t]*[\]][ \t]*
caret [ \t]*\^[ \t]*
valid_lhs {alpha_}{alphanum_}*
prefix [ \t]*[<][ \t]*
suffix [ \t]*[>][ \t]*
comma [ \t]*[,][ \t]*
bn \n

%%

<ERROR>{line} {
    lineno++;
    temp="";
    temp_lhs="";
    temp_rhs="";
    BEGIN INITIAL;
}


<INITIAL,RANGE,PREFIX,SUFFIX,EXPONENT,ERROR>{sptab} {/*Skip white spaces*/}  
<INITIAL,RANGE,AFTERRANGE,PREFIX,FULLRR,SUFFIX,EXPONENT,VAR,ERROR>[\n] {
    /*Error (incomplete line)*/
    cout<<"\t***Line "<<lineno<<" incomplete. Skipping line."<<endl;
    lineno++;
    temp="";
    temp_lhs="";
    temp_rhs="";
    BEGIN INITIAL;
} 

{notalpha_} {
    /*If line starts with non-alphabet or _ (after skipping whitespaces)*/
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

{alpha_} {
    char *p = yytext;
    temp_lhs.push_back(*p);
    BEGIN LHS;
}

<LHS>{alphanum_} {
    char *p = yytext;
    temp_lhs.push_back(*p);
}
<LHS>{eq} {
    BEGIN STR;
}
<LHS>{bn} {
    cout<<"\t***Assignment operator missing: Line "<<lineno<<" cannot be processed"<<endl;
    lineno++;
    temp="";
    temp_lhs="";
    temp_rhs="";
    BEGIN INITIAL;
}
<LHS>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<STR>{dollar} {
    BEGIN VAR;
}
<STR>{alpha} {
    char *p = yytext;
    temp.push_back(*p);
}
<STR>{dot} {
    temp_rhs+=temp;
    temp="";
    BEGIN STR;
}
<STR>{newl} {
    temp_rhs+=temp;
    if(temp_rhs.empty()){
        cout<<"\t***No r-value. Line "<<lineno<<" cannot be processed"<<endl;
    }
    else{
        var_table[temp_lhs] = temp_rhs;
        cout<<"+++Line "<<lineno<<" processed: "<<temp_lhs<<" is set to \""<<temp_rhs<<"\""<<endl;
    }
    temp = "";
    temp_lhs="";
    temp_rhs="";
    lineno++;
    BEGIN INITIAL;
}
<STR>{openbracket} {
    if(temp.empty()){
        cout<<"\t***No string argument. Line "<<lineno<<" cannot be processed."<<endl;
        BEGIN ERROR;
    }
    else{
        BEGIN RANGE;
    }
}
<STR>{caret} {
    if(temp.empty()){
        cout<<"\t***No string argument. Line "<<lineno<<" cannot be processed."<<endl;
        BEGIN ERROR;
    }
    else{
        BEGIN EXPONENT;
    }
}
<STR>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<VAR>{valid_lhs} {
    string t(yytext);
    if(var_table.find(t) == var_table.end()){
        temp="";
        cout<<"\t***Reference to undefined variale \""<<t<<"\""<<endl;
    }
    else temp = var_table[t];
    BEGIN AFTERVAR;
}
<VAR>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<AFTERVAR>{dot} {
    temp_rhs+=temp;
    temp="";
    BEGIN STR;
}
<AFTERVAR>{newl} {
    temp_rhs+=temp;
    var_table[temp_lhs] = temp_rhs;
    cout<<"+++Line "<<lineno<<" processed: "<<temp_lhs<<" is set to \""<<temp_rhs<<"\""<<endl;
    temp = "";
    temp_lhs="";
    temp_rhs="";
    lineno++;
    BEGIN INITIAL;
}
<AFTERVAR>{openbracket} {
    BEGIN RANGE;
}
<AFTERVAR>{caret} {
    BEGIN EXPONENT;
}
<AFTERVAR>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<EXPONENT>{num}+ {
    string t(yytext);
    int exp = stoi(t);
    if (exp==0) temp="";
    t=temp;
    for(int i=0; i<exp-1; i++){
        temp+=t;
    }
    BEGIN AFTEREXP;
}
<EXPONENT>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<AFTEREXP>{dot} {
    temp_rhs+=temp;
    temp="";
    BEGIN STR;
}
<AFTEREXP>{newl} {
    temp_rhs+=temp;
    var_table[temp_lhs] = temp_rhs;
    cout<<"+++Line "<<lineno<<" processed: "<<temp_lhs<<" is set to \""<<temp_rhs<<"\""<<endl;
    temp = "";
    temp_lhs="";
    temp_rhs="";
    lineno++;
    BEGIN INITIAL;
}
<AFTEREXP>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<RANGE>{prefix} {
    BEGIN PREFIX;
}
<RANGE>{suffix} {
    BEGIN SUFFIX;
}
<RANGE>{num}+{comma} {
    string t(yytext);
    while (t.back()==' ' || t.back()=='\t' || t.back()==','){
        t = t.substr(0, t.length()-1);
    }
    l = stoi(t);
    BEGIN FULLRR;
}
<RANGE>{num}+ {
    string t(yytext);
    l = stoi(t);
    if (l>=temp.length()) temp = "";
    else temp = temp.substr(l, 1);
    BEGIN AFTERRANGE;
}
<RANGE>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<PREFIX>{num}+ {
    string t(yytext);
    l = stoi(t);
    if(temp.length()>l){
        temp = temp.substr(0, l);
    }
    BEGIN AFTERRANGE;
}
<PREFIX>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}
<SUFFIX>{num}+ {
    string t(yytext);
    l = stoi(t);
    if(temp.length()>l){
        temp = temp.substr(temp.length()-l, l);
    }
    BEGIN AFTERRANGE;
}
<SUFFIX>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}
<FULLRR>{num}* {
    string t(yytext);
    int r = stoi(t);
    if (l>r || l>=temp.length()){
        temp = "";
    }
    else{
        temp = temp.substr(l, r-l+1);
    }
    BEGIN AFTERRANGE;
}
<FULLRR>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<AFTERRANGE>{closebracket} {
    BEGIN AFTERAFTERRANGE;
}
<AFTERRANGE>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

<AFTERAFTERRANGE>{dot} {
    temp_rhs+=temp;
    temp="";
    BEGIN STR;
}
<AFTERAFTERRANGE>{newl} {
    temp_rhs+=temp;
    var_table[temp_lhs] = temp_rhs;
    cout<<"+++Line "<<lineno<<" processed: "<<temp_lhs<<" is set to \""<<temp_rhs<<"\""<<endl;
    temp = "";
    temp_lhs="";
    temp_rhs="";
    lineno++;
    BEGIN INITIAL;
}
<AFTERAFTERRANGE>{caret} {
    BEGIN EXPONENT;
}
<AFTERAFTERRANGE>. {
    cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<lineno<<" cannot be processed"<<endl;
    BEGIN ERROR;
}

%%

int yywrap(){
    return 1;
}

int main(int argc, char* argv[]){
    if(argc!=0){
        yyin = (FILE*)fopen(argv[1], "r");
    }
    yylex();
    exit(0);
    return 0;
}