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
string incorrectVar = "Error. The variable does not exist or it is not of the type expected.";
string incorrectLenDiam = "Error. Length (of tubes and vectors) and diameter must be postive integers";
string badSyntax = "Error. Bad syntax";


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
    cerr << "evalN" << endl;
    if (a == NULL) return 0;
    else if(a->kind == "intconst")
        return atoi(a->text.c_str());

    else if(a->kind == "LENGTH" || a->kind == "DIAMETER"){
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

    else if(a->kind == "+")
        return evalNumExpr(child(a,0)) + evalNumExpr(child(a,1));

    else if(a->kind == "-")
        return evalNumExpr(child(a,0)) - evalNumExpr(child(a,1));

    else if(a->kind == "*")
        return evalNumExpr(child(a,0)) * evalNumExpr(child(a,1));

    else if(a->kind == "/")
        return evalNumExpr(child(a,0)) / evalNumExpr(child(a,1));
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
    map<string, char>::iterator it = m.begin();
    while(it != m.end()){
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
                cout << "TUBE " << tubeVectors[it->first].first[i].length << " " << tubeVectors[it->first].first[i].diameter << endl;
            }
        }
        ++it;
    }
}

void execute(AST *a){
    cerr << "execute" << endl;
    if (a == NULL)
        return;

    // ASSIGNATION
    else if (a->kind == "="){
        cerr << "=" << endl;
        string lvar = child(a,0)->text;
        AST* right = child(a,1);

        if(child(a,0)->kind == "var"){ // lvar is a variable
            deleteVar(lvar); // we delete it if it already exists
            AST* splitAST = child(a, 2);
            if(splitAST != NULL && splitAST->kind == "SPLIT"){ // splitasig
                // TODO
                string varToSplit = child(splitAST, 0)->text;
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
                // TODO
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
            }
            else cout << badSyntax << endl;
        }else cout << incorrectVar << endl;
    }

    // numfunct
    else if (a->kind == "LENGTH" || a->kind == "DIAMETER"){
        int res = evalNumExpr(a);
        if(res != -1) cout << res << endl;
    }
    // pushpop
    //
    // whilexpr
    //


    else cerr << a->kind << endl;
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
