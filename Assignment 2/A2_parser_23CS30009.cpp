#include <bits/stdc++.h>
#include "lex.yy.c"

using namespace std;

vector<string> NT{    // Non-terminals
    "LINE",
    "EXPR",
    "REST",
    "ARG",
    "XPNT",
    "BASE"
};
vector<string> T{    // Terminals
    "EOF",
    "ID",
    "NUM",
    "STR",
    "REF",
    "LPN",
    "RPN",
    "CRT",
    "DOT",
    "ASG",
    "EOL",
    "SP",     // SP is for space or tab
    "ERROR"
};
// "eps" is epsilon


unordered_map<string, unordered_map<string, bool>> parse_table;  // Will store if the parse table entry is empty or not

map<string, string> var_table;

string lhs="";
string rhs="";
string look;    // Lookahead token


void get_token();   // Returns "-1" in case of error. Also ignores spaces and tabs.
string parseBASE();
string parseXPNT();
string parseARG();
string parseREST();
string parseEXPR();
string parseLINE();


int main(int argc, char const *argv[]){
    if(argc>1){
        yyin = (FILE*)fopen(argv[1], "r");
    }

    parse_table["LINE"]["ID"] = true;
    parse_table["EXPR"]["STR"] = true;
    parse_table["EXPR"]["REF"] = true;
    parse_table["EXPR"]["LPN"] = true;
    parse_table["REST"]["RPN"] = true;
    parse_table["REST"]["DOT"] = true;
    parse_table["REST"]["EOL"] = true;
    parse_table["ARG"]["STR"] = true;
    parse_table["ARG"]["REF"] = true;
    parse_table["ARG"]["LPN"] = true;
    parse_table["XPNT"]["RPN"] = true;
    parse_table["XPNT"]["CRT"] = true;
    parse_table["XPNT"]["DOT"] = true;
    parse_table["XPNT"]["EOL"] = true;
    parse_table["BASE"]["STR"] = true;
    parse_table["BASE"]["REF"] = true;
    parse_table["BASE"]["LPN"] = true;

    BEGIN(INITIAL);
    yylineno=0;
    get_token();
    parseLINE();   // Since the start state is LINE

    return 0;
}


void get_token(){
    int token_ind = yylex();
    string token = T[token_ind];
    string val = yytext;
    while(token=="SP"){   // Skipping spaces and tabs
        token_ind = yylex();
        token = T[token_ind];
        val = yytext;
    }

    if (token=="ERROR"){
        cout<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        lhs="";
        rhs="";
        look = "-1";
    }
    else if (token=="EOF"){
        exit(0);
    }
    else{
        look = token;
    }
    return;
}

string parseBASE(){
    if(look=="-1"){
        return "-1";
    }
    if (!parse_table["BASE"][look]){
        if (look=="EOL"){
            cout<<"\t***Line "<<yylineno<<" incomplete. Skipping line."<<endl;
            return "-1";
        }
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        return "-1";
    }
    
    if (look=="STR"){
        string val = yytext;
        get_token();
        return val;
    }
    else if (look=="REF"){
        string val = yytext;
        val = val.substr(1, val.size()-1);
        if (var_table.find(val)==var_table.end()){
            cout<<"\t***Reference to undefined variable \""<<val<<"\""<<endl;
            get_token();
            return "";
        }
        get_token();
        return var_table[val];
    }
    else if (look=="LPN"){
        get_token();
        string expr = parseEXPR();
        if(look=="-1") return "-1";  // Error in parsing EXPR
        if(look!="RPN"){
            cout<<"\t***Right parenthesis not found. Line "<<yylineno<<" cannot be processed."<<endl;
            if(look=="EOL") return "-1";
            BEGIN(ERROR);
            return "-1";
        }
        get_token();
        return expr;
    }
}

string parseXPNT(){
    if(look=="-1"){
        return "-1";
    }
    if (!parse_table["XPNT"][look]){
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        return "-1";
    }
    if (look=="RPN" || look=="DOT" || look == "EOL") return "";
    else if (look=="CRT"){
        get_token();
        if(look=="-1") return "-1";  // Error in parsing EXPR
        if(look!="NUM"){
            cout<<"\t***Integer exponent not found. Line "<<yylineno<<" cannot be processed."<<endl;
            if(look=="EOL") return "-1";
            BEGIN(ERROR);
            return "-1";
        }
        string val = yytext;
        get_token();
        return val;
    }
}

string parseARG(){
    if(look=="-1"){
        return "-1";
    }
    if (!parse_table["ARG"][look]){
        if (look=="EOL"){
            cout<<"\t***Line "<<yylineno<<" incomplete. Skipping line."<<endl;
            return "-1";
        }
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        return "-1";
    }
    string base = parseBASE();
    if (base=="-1") return "-1";  // Error in parsing BASE
    string xpnt = parseXPNT();
    if (xpnt=="-1") return "-1";  // Error in parsing XPNT
    if (xpnt=="") return base;
    int exp = stoi(xpnt);
    string aa = base;
    for (int i=1; i<exp; i++){
        base += aa;
    }
    return base;
}

string parseREST(){
    if(look=="-1"){
        return "-1";
    }
    if (!parse_table["REST"][look]){
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        return "-1";
    }
    if (look == "RPN") return "";
    else if (look == "EOL") return "";
    else if (look == "DOT"){
        get_token();
        return parseEXPR();
    }
}

string parseEXPR(){
    if(look=="-1"){
        return "-1";
    }
    if (!parse_table["EXPR"][look]){
        if (look=="EOL"){
            cout<<"\t***Line "<<yylineno<<" incomplete. Skipping line."<<endl;
            return "-1";
        }
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        return "-1";
    }
    string arg = parseARG();
    if (arg=="-1") return "-1";  // Error in parsing ARG
    string rest = parseREST();
    if (rest=="-1") return "-1";  // Error in parsing REST
    return arg + rest;  // Concatenating the two strings
}

string parseLINE(){
    yylineno++;
    if(look=="-1"){
        get_token();
        parseLINE();
        return "";
    }
    if (look=="EOL"){
        cout<<"\t***No l-value in line "<<yylineno<<". Skipping line"<<endl;
        lhs="";
        rhs="";
        get_token();
        parseLINE();
        return "";
    }
    if (!parse_table["LINE"][look]){
        cout<<"\t***Invalid character \'"<<yytext[0]<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        lhs="";
        rhs="";
        get_token();
        parseLINE();
        return "";
    }

    // Only case is ID = EXPR
    lhs = yytext;
    get_token();
    if (look=="-1") return "-1";  // Error in parsing EXPR
    if (look=="EOL"){
        cout<<"\t***Assignment operator not found. Line "<<yylineno<<" cannot be processed"<<endl;
        lhs="";
        rhs="";
        BEGIN(INITIAL);
        parseLINE();
        return "";
    }
    else if(look!="ASG"){
        cout<<"\t***Assignment operator not found. Line "<<yylineno<<" cannot be processed"<<endl;
        BEGIN(ERROR);
        lhs="";
        rhs="";
        get_token();
        parseLINE();
        return "";
    }

    get_token();
    rhs = parseEXPR();
    if (rhs!="-1"){
        var_table[lhs]=rhs;
        cout<<"+++Line "<<yylineno<<" processed: "<<lhs<<" is set to \""<<rhs<<"\""<<endl;
    }
    lhs="";
    rhs="";
    get_token();
    parseLINE();
    return "";
}