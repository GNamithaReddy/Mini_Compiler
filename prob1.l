%{
#include <stdio.h>
#include <string.h>
#include "y.tab.h"

//void yyerror(char *);
extern YYSTYPE yylval;
int lineNum = 0;

%}

start \/\* 
end   \*\/
digit  [0-9]
letter [a-zA-Z]
mulop	[*/%]
addop	[+-]


%%
\/\/(.)*                ;
{start}.*{end}          ;  
"if"                                    {strcpy(yylval.STRING,yytext);return IF;}
"else"                                  {strcpy(yylval.STRING,yytext);return ELSE;}
"while"                                 {strcpy(yylval.STRING,yytext);return WHILE;}
"int"                       		{strcpy(yylval.STRING,yytext); return INT;   }
"float"                     		{strcpy(yylval.STRING,yytext); return FLOAT; }
"char"                      		{strcpy(yylval.STRING,yytext); return CHAR;  }
"long"                      		{strcpy(yylval.STRING,yytext); return LONG;  }
"double"                      		{strcpy(yylval.STRING,yytext); return DOUBLE;}
"record"                                {strcpy(yylval.STRING, yytext);return RECORD;}

{letter}({letter}|{digit})*             {strcpy(yylval.STRING,yytext); return ID;}
{digit}+|({digit}+("."){digit}+)   	{yylval.VALUE = atoi(yytext); return NUM;} 
"=="|">="|"<="|">"|"<"|"!="	        {strcpy(yylval.STRING, yytext);return RELOP;}
{addop} 				{strcpy(yylval.STRING, yytext);	return ADDOP;}
{mulop} 				{strcpy(yylval.STRING, yytext); return MULOP;}
"="					{strcpy(yylval.STRING, yytext);	return ASSIGN;}
"||"           			        {strcpy(yylval.STRING, yytext);	return OR;}
"&&"					{strcpy(yylval.STRING, yytext);	return AND;}
";"					{strcpy(yylval.STRING, yytext);	return END;}
"!"					{strcpy(yylval.STRING, yytext);	return NOT;}
"["					{strcpy(yylval.STRING, yytext);	return SQRBO;}
"]"					{strcpy(yylval.STRING, yytext);	return SQRBC;}
"("					{strcpy(yylval.STRING, yytext);	return STRBO;}
")"					{strcpy(yylval.STRING, yytext);	return STRBC;}
"{"					{strcpy(yylval.STRING, yytext);	return CURBO;}
"}"					{strcpy(yylval.STRING, yytext);	return CURBC;}
"++"                                    {strcpy(yylval.STRING, yytext);return UPLUS;}
","					{strcpy(yylval.STRING, yytext);	return COMMA;}
"--"                                    {strcpy(yylval.STRING, yytext);return UMINUS;}
"\n"				        {lineNum++;}
[._]+				        {yyerror("Invalid character");}
"EOF"                                   {return 0;}
[ \t]+                                  ;

%%

