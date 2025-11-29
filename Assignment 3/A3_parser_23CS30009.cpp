#include <bits/stdc++.h>
#include "lex.yy.c"

using namespace std;

vector<string> NT{    // Non-terminals
    "L",
    "E",
    "R",
    "A",
    "X",
    "B"
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
    "PLUS",
    "ASG",
    "EOL",
    "SP",     // SP is for space or tab
    "ERROR"
};
// "eps" is epsilon


typedef struct box_{  // An entry of the parse table
    char t;   // s for shift, r for reduce, g for goto, a for accept
    int i;
} box;

typedef struct rule_{
    int n;     // The number of symbols on the RHS of the rule
    string NT;    // The non terminal on the LHS of a rule;
} rule;

typedef struct state_{
    int s;  //State number
    string ss;   //Value
} state;

vector<rule> rules;   // Index of the vector is the rule number

vector<unordered_map<string, box>> parse_table(18);  // ith index stores the row of parse table for ith state

map<string, string> var_table;

string lhs="";
string rhs="";
string look="";


void get_token(bool);   // Returns "-1" in case of error. Also ignores spaces and tabs.
void parse();



int main(int argc, char const *argv[]){
    if(argc>1){
        yyin = (FILE*)fopen(argv[1], "r");
    }

    rules.push_back({1, "l"});   //l is for L'
    rules.push_back({3, "L"});
    rules.push_back({2, "E"});
    rules.push_back({0, "R"});   //No pops for epsilon on RHS
    rules.push_back({2, "R"});
    rules.push_back({2, "A"});
    rules.push_back({0, "X"});
    rules.push_back({2, "X"});
    rules.push_back({1, "B"});
    rules.push_back({1, "B"});
    rules.push_back({3, "B"});

    parse_table[0]["ID"]={'s', 2};
    parse_table[0]["L"]={'g', 1};
    parse_table[1]["EOL"]={'a', 0};
    parse_table[2]["ASG"]={'s', 5};
    parse_table[3]["RPN"]={'r', 2};
    parse_table[3]["EOL"]={'r', 2};
    parse_table[4]["EOL"]={'r', 1};
    parse_table[5]["STR"]={'s', 11};
    parse_table[5]["REF"]={'s', 12};
    parse_table[5]["LPN"]={'s', 10};
    parse_table[5]["E"]={'g', 4};
    parse_table[5]["A"]={'g', 6};
    parse_table[5]["B"]={'g', 15};
    parse_table[6]["PLUS"]={'s', 7};
    parse_table[6]["RPN"]={'r', 3};
    parse_table[6]["EOL"]={'r', 3};
    parse_table[6]["R"]={'g', 3};
    parse_table[7]["STR"]={'s', 11};
    parse_table[7]["REF"]={'s', 12};
    parse_table[7]["LPN"]={'s', 10};
    parse_table[7]["E"]={'g', 13};
    parse_table[7]["A"]={'g', 6};
    parse_table[7]["B"]={'g', 15};
    parse_table[8]["RPN"]={'s', 9};
    parse_table[9]["PLUS"]={'r', 10};
    parse_table[9]["CRT"]={'r', 10};
    parse_table[9]["RPN"]={'r', 10};
    parse_table[9]["EOL"]={'r', 10};
    parse_table[10]["STR"]={'s', 11};
    parse_table[10]["REF"]={'s', 12};
    parse_table[10]["LPN"]={'s', 10};
    parse_table[10]["E"]={'g', 8};
    parse_table[10]["A"]={'g', 6};
    parse_table[10]["B"]={'g', 15};
    parse_table[11]["PLUS"]={'r', 8};
    parse_table[11]["CRT"]={'r', 8};
    parse_table[11]["RPN"]={'r', 8};
    parse_table[11]["EOL"]={'r', 8};
    parse_table[12]["PLUS"]={'r', 9};
    parse_table[12]["CRT"]={'r', 9};
    parse_table[12]["RPN"]={'r', 9};
    parse_table[12]["EOL"]={'r', 9};
    parse_table[13]["RPN"]={'r', 4};
    parse_table[13]["EOL"]={'r', 4};
    parse_table[14]["PLUS"]={'r', 5};
    parse_table[14]["RPN"]={'r', 5};
    parse_table[14]["EOL"]={'r', 5};
    parse_table[15]["PLUS"]={'r', 6};
    parse_table[15]["CRT"]={'s', 16};
    parse_table[15]["RPN"]={'r', 6};
    parse_table[15]["EOL"]={'r', 6};
    parse_table[15]["X"]={'g', 14};
    parse_table[16]["NUM"]={'s', 17};
    parse_table[17]["PLUS"]={'r', 7};
    parse_table[17]["RPN"]={'r', 7};
    parse_table[17]["EOL"]={'r', 7};

    BEGIN(INITIAL);
    yylineno=0;
    parse();
    return 0;
}


void get_token(bool first=false){   // first means is this the first token call of the current line
    int token_ind = yylex();
    string token = T[token_ind];
    string val = yytext;
    while(token=="SP"){   // Skipping spaces and tabs
        token_ind = yylex();
        token = T[token_ind];
        val = yytext;
    }

    if (token=="ERROR"){
        if (first) cout<<"+++ Going to parse next statement";
        cout<<endl<<"\t***Invalid character \'"<<yytext<<"\' found. Line "<<yylineno<<" cannot be processed"<<endl<<endl;
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

void parse(){
    while(true){   //Looping over lines
        yylineno++;
        lhs="";
        rhs="";
        stack<state> st;
        st.push({0, ""});   // Start state;
        get_token(true);
        if (look=="-1"){   // Error in lexing
            continue;
        }
        cout<<"+++ Going to parse next statement"<<endl;
        while (true){      // Looping over tokens of a line
            state curr = st.top();
            if ((parse_table[curr.s].find(look) == parse_table[curr.s].end())){    // If the corresponding table entry is empty
                cout<<endl<<"Parsing Error. Exiting ..."<<endl;
                exit(0);
            }
            box b = parse_table[curr.s][look];
            if (b.t=='a'){     // Accept
                var_table[lhs]=rhs;
                cout<<"-> ACCEPT"<<endl;
                cout<<"+++ Stored "<<lhs<<" = "<<rhs<<endl;
                cout<<endl;
                break;
            }
            else if (b.t=='s'){   // Shift
                state nnew;
                nnew.s = b.i;
                nnew.ss = yytext;    // Consuming the input in case of shift
                st.push(nnew);
                if (b.i==2){   // In case of first line of processing a new line
                    cout<<"      0 "<<"[s2] -> 2 ";
                }
                else{
                    cout<<"[s"<<b.i<<"] -> "<<b.i <<" ";
                }
                get_token();    // Reading a new token as we consume a token in case of shift
                if (look=="-1"){   // Error in lexing
                    break;
                }
            }
            else if (b.t=='r'){    // Reduce
                cout<<"[r"<<b.i<<"] ";
                // Processing the stack according to the rule number
                // r0 never happens according to the parse table
                if(b.i==1){
                    //L -> ID = E
                    state temp = st.top();
                    st.pop();
                    rhs = temp.ss;    // Storing the r-value in rhs variable
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();  //Ignoring =
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();
                    lhs = temp.ss;    // Storing the l-value in lhs variable
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = "";
                    st.push(nnew);
                    cout<<"<- "<<temp.s<<" ";
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==2){
                    // E -> AR
                    state temp = st.top();
                    st.pop();
                    string R = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();
                    string A = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = A+R;
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==3){
                    // R -> eps
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = "";
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==4){
                    // R -> +E
                    state temp = st.top();
                    st.pop();
                    string E = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();     //Ignoring +
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = E;
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==5){
                    // A -> B X
                    state temp = st.top();
                    st.pop();
                    string X = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();
                    string B = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    if (X=="") nnew.ss = B;
                    else{
                        X = X.substr(1, X.length()-1);
                        int k = stoi(X);
                        nnew.ss = "";
                        for(int i=0; i<k; i++){
                            nnew.ss +=B;
                        }
                    }
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==6){
                    // X -> eps
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = "";
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==7){
                    // X -> ^ NUM
                    state temp = st.top();
                    st.pop();
                    string N = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = "^"+N;    // I am handling the removal of caret and converting to int in rule number 5
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==8){
                    // B -> STR
                    state temp = st.top();
                    st.pop();
                    string SS = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = SS;
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==9){
                    // B -> $ID
                    state temp = st.top();
                    st.pop();
                    string SS = temp.ss.substr(1, temp.ss.length()-1);    // Removing $
                    cout<<"<- "<<temp.s<<" ";
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    if ((var_table.find(SS) == var_table.end())) nnew.ss = "";
                    else nnew.ss = var_table[SS];
                    st.push(nnew);
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
                else if(b.i==10){
                    // B -> (E)
                    state temp = st.top();
                    st.pop();    //Ignoring )
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();
                    string E = temp.ss;
                    cout<<"<- "<<temp.s<<" ";
                    temp = st.top();
                    st.pop();    //Ignoring (
                    state nnew;
                    nnew.s = parse_table[st.top().s][rules[b.i].NT].i;
                    nnew.ss = E;
                    st.push(nnew);
                    cout<<"<- "<<temp.s<<" ";
                    cout<<"-> "<<nnew.s<<endl<<"\t";
                }
            }
        }
    }
}