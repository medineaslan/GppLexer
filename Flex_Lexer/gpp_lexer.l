%{
	int flag = 0; 
	FILE *fp_w;
%}
  
%%
"and" {fprintf(fp_w, "KW_AND\n"); printf("KW_AND\n");}
"or" {fprintf(fp_w, "KW_OR\n"); printf("KW_OR\n");}
"not" {fprintf(fp_w, "KW_NOT\n"); printf("KW_NOT\n");}
"equal" {fprintf(fp_w, "KW_EQUAL\n"); printf("KW_EQUAL\n");}
"less" {fprintf(fp_w, "KW_LESS\n"); printf("KW_LESS\n");}
"nil" {fprintf(fp_w, "KW_NIL\n"); printf("KW_NIL\n");}
"list" {fprintf(fp_w, "KW_LIST\n"); printf("KW_LIST\n");}
"append" {fprintf(fp_w, "KW_APPEND\n"); printf("KW_APPEND\n");}
"concat" {fprintf(fp_w, "KW_CONCAT\n"); printf("KW_CONCAT\n");}
"set" {fprintf(fp_w, "KW_SET\n"); printf("KW_SET\n");}
"deffun" {fprintf(fp_w, "KW_DEFFUN\n"); printf("KW_DEFFUN\n");}
"for" {fprintf(fp_w, "KW_FOR\n"); printf("KW_FOR\n");}
"if" {fprintf(fp_w, "KW_IF\n"); printf("KW_IF\n");}
"exit" {fprintf(fp_w, "KW_EXIT\n"); printf("KW_EXIT\n");}
"load" {fprintf(fp_w, "KW_LOAD\n"); printf("KW_LOAD\n");}
"disp" {fprintf(fp_w, "KW_DISP\n"); printf("KW_DISP\n");}
"true" {fprintf(fp_w, "KW_TRUE\n"); printf("KW_TRUE\n");}
"false" {fprintf(fp_w, "KW_FALSE\n"); printf("KW_FALSE\n");}
"+" {fprintf(fp_w, "OP_PLUS\n"); printf("OP_PLUS\n");}
"-" {fprintf(fp_w, "OP_MINUS\n"); printf("OP_MINUS\n");}
"/" {fprintf(fp_w, "OP_DIV\n"); printf("OP_DIV\n");}
"*" {fprintf(fp_w, "OP_MULT\n"); printf("OP_MULT\n");}
"(" {fprintf(fp_w, "OP_OP\n"); printf("OP_OP\n");}
")" {fprintf(fp_w, "OP_CP\n"); printf("OP_CP\n");}
"**" {fprintf(fp_w, "OP_DBLMULT\n"); printf("OP_DBLMULT\n");}
"," {fprintf(fp_w, "OP_COMMA\n"); printf("OP_COMMA\n");}

"\"" {    
        if(flag==0) 
        {
            fprintf(fp_w, "OP_OC\n");
            printf("OP_OC\n");
            flag=1;
        }             
        else{ 
            fprintf(fp_w, "OP_CC\n");
            printf("OP_CC\n");
            flag=0;
        }  
	}

";;".* {fprintf(fp_w, "COMMENT\n"); printf("COMMENT\n");}

[1-9][0-9]* {fprintf(fp_w, "VALUE\n"); printf("VALUE\n");}

0 {fprintf(fp_w, "VALUE\n"); printf("VALUE\n");}

[0][0-9]* {fprintf(fp_w, "SYNTAX ERROR %s can not be tokenized.\n", yytext); printf("SYNTAX ERROR %s can not be tokenized.\n", yytext); }

[a-zA-Z_][a-zA-Z0-9_]* {fprintf(fp_w, "IDENTIFIER\n"); printf("IDENTIFIER\n");}

[0-9][a-zA-Z0-9]* {fprintf(fp_w, "SYNTAX ERROR %s can not be tokenized.\n", yytext); printf("SYNTAX ERROR %s can not be tokenized.\n", yytext); }

"\n"|"\t"|" " { } 

. {fprintf(fp_w, "SYNTAX ERROR %s can not be tokenized.\n", yytext); printf("SYNTAX ERROR %s can not be tokenized.\n", yytext); }

"\n\n" {return 0; } 
  
%%

int yywrap(){} 

int main(int argc, char *argv[]){ 

	fp_w = fopen ("parsed_cpp.txt","w");

    if(argc > 1) {
    	FILE *fp_r = fopen(argv[1], "r");
    	yyin = fp_r;
		yylex();
    }
    else {
    	yyin=stdin;
    	yylex();
    }
    
	return 0; 
}
