#include <bits/stdc++.h>

using namespace std;


string strip(string s){
    int i=0;
    while(i<s.size()){
        if (isspace(s[i])){
            i++;
        }
        else break;
    }
    int j=s.size()-1;
    while(j>=0){
        if (isspace(s[j])){
            j--;
        }
        else break;
    }
    if (i>j) return "";
    string t="";
    for(int k=i; k<=j; k++){
        t.push_back(s[k]);
    }
    return t;
}

bool check_valid_lhs(string &s){
    if (!((s[0]<='z' && s[0]>='a') || (s[0]<='Z' && s[0]>='A') || s[0]=='_')){
        return false;
    }
    for(int i=1; i<s.size(); i++){
        if (!((s[i]<='z' && s[i]>='a') || (s[i]<='Z' && s[i]>='A') || s[i]=='_' || (s[i]<='9' && s[i]>='0'))){
            return false;
        }
    }
    return true;
}

int main(){
    map<string, string> var_table;
    while (true){
        cout<<"line> ";
        string s;
        getline(cin, s);
        if (s=="exit") break;
        string lhs;
        string rhs;
        int i=0;
        while (i<s.size()){
            if (s[i]!='='){
                lhs.push_back(s[i]);
                i++;
            }
            else{
                i++;
                break;
            }
        }
        while (i<s.size()){
            rhs.push_back(s[i]);
            i++;
        }

        lhs = strip(lhs);
        rhs = strip(rhs);

        if (lhs.size()==0 || rhs.size()==0){
            if(lhs.size()==0) cout<<"*** l-value is absent. "<<endl;
            if(rhs.size()==0) cout<<"*** r-value is absent. "<<endl;
            continue;
        }

        if(check_valid_lhs(lhs) == false){
            cout<<"*** Invalid l-value according to C variable conventions."<<endl;
            continue;
        }

        vector<string> concats;
        i = 0;
        while(i<rhs.size()){
            string t="";
            while(rhs[i]!='.' && i<rhs.size()){
                t.push_back(rhs[i]);
                i++;
            }
            t = strip(t);
            concats.push_back(t);
            i++;   // To skip the dot
        }

        string result="";
        vector<string> invalids;
        
        bool valid = true;
        for(auto s:concats){
            if (s.size()==0) continue;
            if (s[0]!='$'){
                string t = "";
                string num = "";
                int i=0;
                while(i<s.size()){
                    if (s[i]!='^'){
                        t.push_back(s[i]);
                        i++;
                    }
                    else{
                        i++;    // To skip carat symbol
                        break;
                    }
                }

                t = strip(t);

                while(i<s.size()){
                    num.push_back(s[i]);
                    i++;
                }
                int n = 1;

                num = strip(num);
                if (num.size()!=0) n = stoi(num);

                for(int i=1; i<=n; i++){
                    result = result + t;
                }
            }
            else{
                string var = "";
                string num = "";
                int i=1;
                while(i<s.size()){
                    if (s[i]!='^'){
                        var.push_back(s[i]);
                        i++;
                    }
                    else{
                        i++;    // To skip carat symbol
                        break;
                    }
                }

                var = strip(var);
                if (var_table.find(var) == var_table.end()){
                    valid = false;
                    invalids.push_back(var);
                    continue;
                }

                var = var_table[var];

                while(i<s.size()){
                    num.push_back(s[i]);
                    i++;
                }
                int n = 1;

                num = strip(num);
                if (num.size()!=0) n = stoi(num);

                for(int i=1; i<=n; i++){
                    result = result + var;
                }
            }
        }

        var_table[lhs] = result;
        cout<<lhs<<" is set to \""<<result<<"\""<<endl;

        if (valid == false){
            if (invalids.size()==1){
                cout<<"*** Undefined variable \""<<invalids[0]<<"\""<<endl;
            }
            else{
                cout<<"*** Undefined variables ";
                for(int i=0; i<invalids.size()-1; i++){
                    cout<<"\""<<invalids[i]<<"\", ";
                }
                cout<<"\""<<invalids[invalids.size()-1]<<"\""<<endl;
            }
            continue;
        }
    }
    return 0;
}