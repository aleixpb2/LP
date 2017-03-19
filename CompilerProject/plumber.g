#header
<<
#include <string>
#include <iostream>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>
#include <vector>
#include <map>

typedef struct Tube{
  int length;
  int diameter;
  Tube(){}
  Tube(int len, int diam): length(len), diameter(diam) {}
} Tube;

map<string, char> m; // variable types. Key: ID, Value: type. IDs are unique, even if the variables are of different types
// Types: T, C, V
map<string, Tube> tubes;
map<string, int> connectors;
map<string, pair<vector<Tube>,int> > tubeVectors; // Key: ID, Value: pair < TubeVector, maxSize >

// Errors strings
string incorrectVar = "Error. The variable does not exist or it is not of the type expected."; // TODO: use the functions
string incorrectLenDiam = "Error. Length (of tubes and vectors) and diameter must be postive integers";
string badSyntax = "Error. Bad syntax";
string vecFull = "Error. Cannot push: the vector is full";
string vecEmpty = "Error. Cannot pop: the vector is empty";
string mergeError = "Error. Cannot merge: Unmatched diameters";


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == NUM) {
    attr->kind = "intconst";
    attr->text = text;
  }
  else if(type == ID){
    attr->kind = "var";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
 AST *c=a->down;
 for (int i=0; c!=NULL && i<n; i++) c=c->right;
 return c;
} 

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  if (a->text!="") cout<<a->text; // NUM or ID
  else cout<<a->kind;
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

/***** AUXILIARY FUNCTIONS ********/

int evalNumExpr(AST *a) {
    if (a == NULL) return 0;
    if(a->kind == "intconst")
        return atoi(a->text.c_str());

    if(a->kind == "LENGTH" || a->kind == "DIAMETER"){
        string var = child(a,0)->text;
        if(m.find(var) != m.end()){ // var exists
            char varT = m[var];
            if(a->kind == "LENGTH"){
                // check type
                if(varT == 'T') return tubes[var].length;
                else cout << incorrectVar << endl; return -1; // TODO: return -1 is enough?
            }else{ // DIAMETER
                // check type
                if(varT == 'T') return tubes[var].diameter;
                else if(varT == 'C') return connectors[var];
                else cout << incorrectVar << endl; return -1;
            }
        }else cout << incorrectVar << endl; return -1;
    }
    if(a->kind == "+")
        return evalNumExpr(child(a,0)) + evalNumExpr(child(a,1));
    if(a->kind == "-")
        return evalNumExpr(child(a,0)) - evalNumExpr(child(a,1));
    if(a->kind == "*")
        return evalNumExpr(child(a,0)) * evalNumExpr(child(a,1));
    if(a->kind == "/")
        return evalNumExpr(child(a,0)) / evalNumExpr(child(a,1));
}

bool evalBoolExpr(AST* a){
    if (a == NULL) return true;
    if(a->kind == "OR")
        return evalBoolExpr(child(a,0)) || evalBoolExpr(child(a,1));
    if(a->kind == "AND")
        return evalBoolExpr(child(a,0)) && evalBoolExpr(child(a,1));
    if(a->kind == "NOT")
        return !evalBoolExpr(child(a,0));
    if(a->kind == "<")
        return evalNumExpr(child(a,0)) < evalNumExpr(child(a,1));
    if(a->kind == ">")
        return evalNumExpr(child(a,0)) > evalNumExpr(child(a,1));
    if(a->kind == "==")
        return evalNumExpr(child(a,0)) == evalNumExpr(child(a,1));
    if(a->kind == "FULL" || a->kind == "EMPTY"){
        string var = child(a,0)->text;
        if(m.find(var) == m.end() || (m.find(var) != m.end() && m[var] != 'V')){
            cout << incorrectVar << endl;
            return false; // don't iterate
        }
        if(a->kind == "FULL"){
            int maxSize = tubeVectors[var].second;
            return tubeVectors[var].first.size() == maxSize;
        }
        return tubeVectors[var].first.size() == 0; // EMPTY
    }
}

void deleteVar(string var){
    if(m.find(var) != m.end()){ // exists
        char t = m[var];
        switch(t){
        case 'T':
            tubes.erase(var);
            break;
        case 'C':
            connectors.erase(var);
            break;
        default: // 'V'
            tubeVectors.erase(var);
        }
        m.erase(var);
    }
}

void printVars(){
    cout << endl << "=== Variables ===" << endl;
    map<string, char>::iterator it = m.begin();
    while(it != m.end()){
        cout << it->first << ": \t";
        char t = it->second;
        switch(t){
        case 'T':
            cout << "TUBE " << tubes[it->first].length << " " << tubes[it->first].diameter << endl;
            break;
        case 'C':
            cout << "CONNECTOR " << connectors[it->first] << endl;
            break;
        default: // 'V'
            cout << "TUBEVECTOR OF " << tubeVectors[it->first].second << ":" << endl;
            int size = tubeVectors[it->first].first.size();
            for(int i = 0; i < size; ++i){
                cout << "\t  TUBE " << tubeVectors[it->first].first[i].length << " " << tubeVectors[it->first].first[i].diameter << endl;
            }
        }
        ++it;
    }
}

bool existsTube(AST* a){
    string var = a->text;
    if(a->kind != "var" || m.find(var) == m.end()){
        cout << "Variable " << var << " does not exist." << endl;
        return false;
    }
    if(m.find(var) != m.end() && m[var] != 'T'){
        cout << "Variable " << var << " is not a tube." << endl;
        return false;
    }
    return true;
}


bool existsConnector(AST* a){
    string var = a->text;
    if(a->kind != "var" || m.find(var) == m.end()){
        cout << "Variable " << var << " does not exist." << endl;
        return false;
    }
    if(m.find(var) != m.end() && m[var] != 'C'){
        cout << "Variable " << var << " is not a connector." << endl;
        return false;
    }
    return true;
}


bool existsTubeVector(AST* a){
    string var = a->text;
    if(a->kind != "var" || m.find(var) == m.end()){
        cout << "Variable " << var << " does not exist." << endl;
        return false;
    }
    if(m.find(var) != m.end() && m[var] != 'V'){
        cout << "Variable " << var << " is not a tube vector." << endl;
        return false;
    }
    return true;
}

Tube merge(AST* mer, bool modify){
    AST* lTubeAST = child(mer, 0);
    AST* conAST = child(mer, 1);
    AST* rTubeAST = child(mer, 2);
    Tube lTube, rTube;
    if(!existsConnector(conAST))return Tube(-1, -1);
    // Connector exists
    if(lTubeAST->kind == "MERGE") lTube = merge(lTubeAST, modify);
    else if(!existsTube(lTubeAST)) return Tube(-1, -1);
    // lTube exists
    else lTube = tubes[lTubeAST->text];

    if(rTubeAST->kind == "MERGE") rTube = merge(rTubeAST, modify);
    else if(!existsTube(rTubeAST)) return Tube(-1, -1);
    // rTube exists
    else rTube = tubes[rTubeAST->text];

    // Check if we can merge
    if(lTube.length != -1 && rTube.length != -1 && (lTube.diameter == rTube.diameter == connectors[conAST->text])){
        if(modify){
            deleteVar(lTubeAST->text);
            deleteVar(rTubeAST->text);
            deleteVar(conAST->text);
        }
        return Tube(lTube.length+rTube.length, lTube.diameter);
    }else return Tube(-1, -1);

}

void execute(AST *a){
    if (a == NULL)
        return;

    // ASSIGNATION
    else if (a->kind == "="){
        cerr << "=" << endl;
        string lvar = child(a,0)->text;
        AST* right = child(a,1);
        if(child(a,0)->kind != "var")cout << incorrectVar << endl;
        else{
            // lvar is a variable now
            deleteVar(lvar); // we delete it if it already exists
            AST* splitAST = child(a, 2);
            if(splitAST != NULL && splitAST->kind == "SPLIT"){ // splitasig
                cerr << "SPLIT" << endl;
                // we check if right is a var and varToSplit is a Tube
                string varToSplit = child(splitAST, 0)->text;
                if(right->kind != "var" || child(splitAST, 0)->kind != "var" || m[varToSplit] != 'T')cout << incorrectVar << endl;
                else{
                    string rvar = right->text;
                    deleteVar(rvar);
                    int len = tubes[varToSplit].length;
                    int diam= tubes[varToSplit].diameter;
                    deleteVar(varToSplit);
                    int len1 = len/2;
                    int len2 = len-len1;
                    m[lvar] = 'T';
                    m[rvar] = 'T';
                    if(len1 > 0 && len2 > 0 && diam > 0){
                        tubes[lvar] = Tube(len1, diam);
                        tubes[rvar] = Tube(len2, diam);
                    }
                    else cout << incorrectLenDiam << endl;
                }
            }else if(right->kind == "TUBE"){
                cerr << "TUBE" << endl;
                m[lvar] = 'T';
                int len = evalNumExpr(child(right, 0));
                int diam = evalNumExpr(child(right, 1));
                if(len > 0 && diam > 0) tubes[lvar] = Tube(len, diam);
                else cout << incorrectLenDiam << endl;
            }else if(right->kind == "CONNECTOR"){
                cerr << "CONNECTOR" << endl;
                m[lvar] = 'C';
                int diam = evalNumExpr(child(right, 0));
                if(diam > 0) connectors[lvar] = diam;
                else cout << incorrectLenDiam << endl;
            }else if(right->kind == "MERGE"){
                Tube t = merge(right, false); // modify = false, just a check if it is possible
                if(t.length > 0){ // merge successful. Change state
                    Tube t = merge(right, true);
                    m[lvar] = 'T';
                    tubes[lvar] = Tube(t.length, t.diameter);
                }else cout << mergeError << endl;

            }else if(right->kind == "TUBEVECTOR"){
                cerr << "TUBEVECTOR" << endl;
                m[lvar] = 'V';
                int maxSize = evalNumExpr(child(right, 0));
                if(maxSize > 0){
                    tubeVectors[lvar].first = vector<Tube>();
                    tubeVectors[lvar].second = maxSize;
                }
                else cout << incorrectLenDiam << endl;
            }else if (right->kind == "var"){ // copy assignment
                cerr << "Copy" << endl;
                string rvar = right->text;
                char t = m[rvar];
                m[lvar] = t;
                switch(t){
                case 'T':
                    tubes[lvar] = tubes[rvar];
                    break;
                case 'C':
                    connectors[lvar] = connectors[rvar];
                    break;
                default: // 'V'
                    tubeVectors[lvar] = tubeVectors[rvar];
                }
            }else cout << badSyntax << endl;
        }
    }

    // numfunct
    else if (a->kind == "LENGTH" || a->kind == "DIAMETER"){
        int res = evalNumExpr(a);
        if(res != -1) cout << res << endl;
    }
    // pushpop
    else if(a->kind == "PUSH" || a->kind == "POP"){
        string vecVar = child(a, 0)->text;
        string tubeVar = child(a, 1)->text;
        if(m.find(vecVar) == m.end() || (m.find(vecVar) != m.end() && m[vecVar] != 'V')) cout << incorrectVar << endl; // not a vector
        else{
            if(a->kind == "PUSH"){
                if(m.find(tubeVar) == m.end() || (m.find(tubeVar) != m.end() && m[tubeVar] != 'T')) cout << incorrectVar << endl; // not a tube
                else{
                    int maxSize = tubeVectors[vecVar].second;
                    if(tubeVectors[vecVar].first.size() < maxSize){
                        Tube t = tubes[tubeVar];
                        tubeVectors[vecVar].first.push_back(t);
                        deleteVar(tubeVar);
                    }
                    else cout << vecFull << endl;
                }
            }else{ // POP
                if(tubeVectors[vecVar].first.size() > 0){
                    deleteVar(tubeVar);
                    Tube t = tubeVectors[vecVar].first.back();
                    tubeVectors[vecVar].first.pop_back();
                    m[tubeVar] = 'T';
                    tubes[tubeVar] = t;
                }else cout << vecEmpty << endl;
            }
        }
    }
    // whilexpr
    else if(a->kind == "WHILE"){
        AST* boolexpr = child(a, 0);
        AST* list = child(a, 1);
        bool iterate = evalBoolExpr(boolexpr);
        while(iterate){
            execute(child(list, 0));
            iterate = evalBoolExpr(boolexpr);
        }
    }
    else cout << badSyntax << endl;
    execute(a->right);
}
/************/

int main() {
  AST *root = NULL;
  ANTLR(plumber(&root), stdin);
  execute(child(root, 0)); // because the root is "list"
  ASTPrint(root);
  printVars();
}
>>

#lexclass START

#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token TIMES "\*"
#token LPAREN "\("
#token RPAREN "\)"

#token ASIG "="
#token TUBE "TUBE"
#token SPLIT "SPLIT"
#token CONNECTOR "CONNECTOR"
#token MERGE "MERGE"
#token LENGTH "LENGTH"
#token DIAMETER "DIAMETER"
#token TUBEVECTOR "TUBEVECTOR"
#token OF "OF"
#token PUSH "PUSH"
#token POP "POP"
#token FULL "FULL"
#token EMPTY "EMPTY"

#token WHILE "WHILE"
#token ENDWHILE "ENDWHILE"

#token LT "<"
#token GT ">"
#token EQ "=="
#token AND "AND"
#token OR "OR"
#token NOT "NOT"

#token COMMA ","
#token ID "[a-zA-Z]+[a-zA-Z0-9]*"


#token SPACE "[\ \n]" << zzskip();>>

plumber: (ops)* <<#0=createASTlist(_sibling);>>;
ops: ID ASIG^ expr | splitasig | numfunct | pushpop | whilexpr;
splitasig: LPAREN! ID COMMA! ID RPAREN! ASIG^ split;
split: SPLIT^ ID;
numfunct:  (LENGTH^ | DIAMETER^) LPAREN! ID RPAREN!;
pushpop: PUSH^ ID ID | POP^ ID ID;
expr: TUBE^ numexpr numexpr | CONNECTOR^ numexpr | merge | TUBEVECTOR^ OF! numexpr |  ID ;

merge: MERGE^ mergexp;
mergexp: (merge | ID) ID (merge | ID);

whilexpr: WHILE^ boolexpr plumber ENDWHILE!;

numexpr: term ((PLUS^|MINUS^) term)* ;
term: atom (TIMES^ atom)* ;
atom: (LPAREN! numexpr RPAREN!) | NUM | numfunct;

boolexpr: andexpr (OR^ andexpr)* ;
andexpr: notexpr (AND^ notexpr)* ;
notexpr: (NOT^ | ) boolatom;
boolatom: (LPAREN! boolexpr RPAREN!) | (NUM | numfunct) (LT^| GT^| EQ^) (NUM | numfunct) | fullempty;
fullempty: (FULL^ | EMPTY^) LPAREN! ID RPAREN!;
