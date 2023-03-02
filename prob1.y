%{
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>
#include<stdbool.h>

extern int lineNum;
int count=0,q;
char type[10];
int offset= 0;
int saveoffset=0,cand=1,cor=1;
int temp=0,size=0,c=0;
bool array=false,error=false,single=false;
int arraybytes=0,start=0,end=0;
int block=1,p=0;
int c1=1,r=0,c2=1;
char strings[10][1000];                                 // To store all error message strings
char tempstr[100],tempstr1[100],elabel[100],str1[100];            // To store labels to use in other ifFalse statements
char vartype[10];                                       // To store type of LHS variable and to compare it with RHS variables


struct dataType                                                       
{
  char variable_name[20];
  char data_type[20];
  char type[20];
  int address;
}symbol_table[40];


struct OPER{
	char operation[10];
	char arg1[20];
	char arg2[20];
	char result[10];
}oper[100];

struct ST{
	char symbol[100];
	int value;
}SYMBOL_TABLE[100];

struct T{
char type[100];
int size;
struct T *nextst;
};


int Q = 0, T = 0, L = 0,ts = 0,ErrorRecovered = 0,STACK_TOP = 0;


char SS[100][100],message[100];   // statement stack

void SS_push(char str[100])
{
	strcpy(SS[STACK_TOP++], str);
}


int GET_SYMBOL_VALUE( char symbol[100]){
	int i;
	for(i=0;i<ts;i++){
		if(strcmp(symbol,SYMBOL_TABLE[i].symbol)==0){			
			return SYMBOL_TABLE[i].value;
		}
	}
	strcpy(SYMBOL_TABLE[ts].symbol, symbol);
	SYMBOL_TABLE[ts].value = 0;
	ts++;
    return 0;
}

void SET_SYMBOL_VALUE(char symbol[100], int value){

	int i;
	for(i=0;i<ts;i++){
		if(strcmp(symbol,SYMBOL_TABLE[i].symbol)==0){
			SYMBOL_TABLE[i].value = value;
			return;
		}
	}
	
 }

int  CALCULATE(int x, int y, char operation[5] ){


	if(strcmp(operation,"*")==0) return x*y;
	if(strcmp(operation,"/")==0) return x/y;
	if(strcmp(operation,"%")==0) return ((int)x) % ((int)y);
	if(strcmp(operation,"&&")==0) return x&&y;
	
	
	if(strcmp(operation,"+")==0) return x+y;
	if(strcmp(operation,"-")==0) return x-y;
	if(strcmp(operation,"||")==0) return x||y;
	if(strcmp(operation,"++")==0) return x+1;
	if(strcmp(operation,"--")==0) return x-1;

	if(strcmp(operation,"==")==0) return  x ==y;		
	if(strcmp(operation,"!=")==0) return x != y;		
	if(strcmp(operation,">=")==0) return x >= y;		
	if(strcmp(operation,"<=")==0) return x <= y;		
	if(strcmp(operation,">")==0)  return x > y;		
	if(strcmp(operation,"<")==0)  return x < y;	
	
}


int IF_NO=1;

%}


%union{
  int      	VALUE;
  char        	STRING[100];
  struct T *t;
}
  
%start Start
%token ID NUM RELOP ADDOP MULOP ASSIGN  IF ELSE WHILE UPLUS UMINUS INT FLOAT CHAR LONG DOUBLE NOT SQRBO SQRBC STRBO STRBC END NEWLINE CURBO CURBC RECORD COMMA OR AND

%type <VALUE> NUM 
%type <STRING> ID RELOP ADDOP MULOP ASSIGN F T SE E V INT FLOAT CHAR LONG DOUBLE NOT SQRBO SQRBC STRBO STRBC END NEWLINE CURBO CURBC RECORD COMMA OR AND M
%type <t> DT B C 

%% 				
		
Start			: CURBO SL { if(!error)
					{int i;
					for(i=0;i<STACK_TOP;i++)
					{
						printf("\r%s\n", SS[i]);
					}
					}
					
				    } CURBC{if(block==1){check(start,count);}} ;
				
SL 		        :    S  | SL S  ;

B                       :                    INT       {     $$=(struct T *)malloc(sizeof(struct T));
							     strcpy($$->type,"int");
							     $$->size=4;
				     
						       } | 
					     FLOAT     {     $$=(struct T *)malloc(sizeof(struct T));
							     strcpy($$->type,"float");
							     $$->size=4;
						       } | 
					     CHAR      {     $$=(struct T *)malloc(sizeof(struct T));
							     strcpy($$->type,"char");
							     $$->size=1;
						       } | 
					     LONG      {     $$=(struct T *)malloc(sizeof(struct T));
							     strcpy($$->type,"long");
							     $$->size=8;
						       } | 
					     DOUBLE      {     $$=(struct T *)malloc(sizeof(struct T));
							     strcpy($$->type,"double");
							     $$->size=8;
						       } ;

P                      :                         B ID{size=$1->size;
						 strcpy(symbol_table[count].variable_name,$2);
						 strcpy(symbol_table[count].data_type,$1->type);
						 symbol_table[count].address=offset;
						 offset=offset+size;
						 p=count;
						 count++;} R|
						 B ID{size=$1->size;
					         strcpy(symbol_table[count].variable_name,$2);
						 strcpy(symbol_table[count].data_type,strcat($1->type,"array"));
						 symbol_table[count].address=offset;
						 offset=offset+size;
						 count++;} SQRBO NUM SQRBC {array=true;arraybytes=$5;}; 

Q                     :                     END S| ASSIGN E END S;	

R                     :                      COMMA ID{
					     strcpy(symbol_table[count].variable_name,$2);
					     strcpy(symbol_table[count].data_type,symbol_table[p].data_type);
					     symbol_table[count].address=offset;
					     offset=offset+size;
					     count++;} R| ;				 

S	  	      :   P Q | {check(start,count);}CURBO{start=count;saveoffset=offset;offset=0;block++;} P END S CURBC{offset=offset+saveoffset;check(start,count);}S|  
                              ID {for(int i=0;i<count;i++){if(strcmp(symbol_table[i].variable_name,$1)==0){r=i;break;}}strcpy(vartype,symbol_table[r].data_type);} ASSIGN E END 
				{               
						int t=0;
						for(int i=0;i<count;i++)
                         		        { if(strcmp(symbol_table[i].variable_name,$1)==0){t++;}}
	                                           
	                                        if(t==0)
			                        { error=true;
			                          strcpy(strings[c],"Var ");
						  strcat(strings[c],$1);
					          strcat(strings[c]," not declared in this scope");
						  c++;
						}
						   
						GET_SYMBOL_VALUE($1);
						char str[100], result[100], num[100]; 
							
						strcpy(num, $4);
						strcpy(result, $1);				
						
						strcpy(oper[Q].operation, $3);
						strcpy(oper[Q].arg1, "");
						strcpy(oper[Q].arg2, num);
						strcpy(oper[Q].result, result);
						Q++;
						
						
						int Num = GET_SYMBOL_VALUE(num);
										
						
						SET_SYMBOL_VALUE(result, Num);					
						
											
						
						sprintf(str, "%s = %s", result, num);
						SS_push(str);
		
						
						
						strcpy(message,"Variable or expression missing. Cannot assign anything!");
				} SL
				| IF STRBO E STRBC  CURBO {char str[100];
					                   sprintf(str, "\n%s:", tempstr);			
					                   SS_push(str);
					                   block++;
				                          }SL CURBC
									{
							
										char str[100],label[100]; 
										strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
										sprintf(str, "goto %s", label);	
										strcpy(elabel,label);				
										SS_push(str);
										
										strcpy(label, tempstr1); 
										sprintf(str, "\n%s:", label);	
													
										SS_push(str);
										
								
									} 
				
				ELSE CURBO SL CURBC 
				{
					char str[100],label[100];
					sprintf(str, "goto %s", elabel);					
					SS_push(str);
					strcpy(label, "\nL"); sprintf(str, "%d", L); strcat(label,str);
					sprintf(str, "%s:", label);
										
					SS_push(str);
					IF_NO+=2;
					c1=1;				                          // We change c1 to 1 again to avoid repeatition of ifFalse statements	
					cor=1;
					cand=1;
				
				} SL
				 
				| WHILE STRBO E STRBC CURBO{  char str[100];
					        sprintf(str, "\n%s:", tempstr);			
					        SS_push(str);
					        block++;
				             }SL CURBC 
				{
						char str[100],label[100]; 
						strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
						sprintf(str, "goto %s", tempstr1);	
						strcpy(elabel,label);				
						SS_push(str);
										
						strcpy(label, tempstr1); 
						sprintf(str, "\n%s:", label);	
													
						SS_push(str);
						c1=1;
						cor=1;
						cand=1;
								
				} SL
				|E| ; 

E		     :          SE 
				{
													
																	
						strcpy($$,  $1);			
										
						
						strcpy(message,"Missing expression!");
				}
				| SE RELOP SE  
				{
						char str[100], result[100], num1[100], num2[100],str1[100],label[100],num[100]; 
					
						strcpy(num1, $1);
						strcpy(num2, $3);
	  
						
						sprintf(str, "%s %s %s", num1, $2, num2);

						strcpy($$,  str);
						int Num1 = GET_SYMBOL_VALUE(num1);
						int Num2 = GET_SYMBOL_VALUE(num2);
						
						GET_SYMBOL_VALUE(str);
						SET_SYMBOL_VALUE(str, CALCULATE(Num1, Num2, $2));
						
	                                        strcpy(message,"Conditional operation cannot be done");
	                                        
											
						
				                                
				} 
				| E  {  
				        char str[100], num[100], label[100]; 
					IF_NO = 1;
													
					strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
					strcpy(tempstr1,label);		
					strcpy(num, $1);
								
					sprintf(str, "ifFalse %s goto %s", num,label);
					SS_push(str);
					
					if(cor==1)
					{
					          strcpy(label, "L"); sprintf(str1, "%d", ++L); strcat(label,str1);
						  sprintf(str1, "goto %s", label);
						  strcpy(tempstr,label);
						  SS_push(str1);
					}
					else
					{
					 SS_push(str1);
					}
					cor++;			
							
				      } OR {  char str[100];
					      sprintf(str, "\n%s:", tempstr1);				
					      SS_push(str);
					   } E { char str[100], num[100], label[100]; 							
						 if(c1==1)				
						 {strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);	
						  strcpy(num, $5);
						  sprintf(str, "ifFalse %s goto %s", num,label);
						  strcpy(tempstr1,label);	
						  SS_push(str);	
						  sprintf(str, "goto %s", tempstr);					
						  SS_push(str);
						 }
						  c1++;
				               } 
				| 
				E {char str[100], num[100], label[100]; 
								IF_NO = 1;
								if(cand==1)					
								{strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
								strcpy(tempstr1,label);	
								}	
								strcpy(num, $1);
								
								sprintf(str, "ifFalse %s goto %s", num,tempstr1);
								SS_push(str);
								
						                 strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
						                 strcpy(tempstr,label);
								 sprintf(str, "goto %s", label);					
								 SS_push(str);
								 cand++;		} AND {char str[100];
										               sprintf(str, "\n%s:", tempstr);				
										               SS_push(str);
										              } E {    char str[100], num[100], label[100]; 
														
													if(c1==1)				
													{
													  strcpy(num, $5);
													  sprintf(str, "ifFalse %s goto %s", num,tempstr1);
														
													  SS_push(str);	
													
													  strcpy(label, "L"); sprintf(str, "%d", ++L); strcat(label,str);
													  sprintf(str, "goto %s", label);
													  strcpy(tempstr,label);	
													  SS_push(str);
													 }
													 c1++;
												  } 
				 |NOT SE{char str[100], result[100], num1[100], num2[100],str1[100],label[100],num[100]; 
					
						single=true;					
						strcpy(num1, $2);
	
						
						sprintf(str, "%s%s", $1, num1);

						strcpy($$,  str);
					};


SE: T 
				{
						strcpy($$, $1);
						
						
				}
				| SE ADDOP T 
				{               char str[100], result[100], num1[100], num2[100], str1[100]; 
				                bool s1=false,s2=false;
				                int c1=-1,c2=-1;
				                // c1 and c2 positive implies variables are present in symbol table
				                // If they remain negative implies either it is a number or the variable is not present in symbol table which outputs error message.
				                for(int i=0;i<count;i++)
				                {
				                  if(strcmp(symbol_table[i].variable_name,$1)==0)
				                  {c1=i;break;}
				                }
				                for(int i=0;i<count;i++)
				                {
				                  if(strcmp(symbol_table[i].variable_name,$3)==0)
				                  {c2=i;break;}
				                }
				             
						if(c1>=0)
						{
						//First Do Type Checking
						if(max(preference(symbol_table[c1].data_type),preference(vartype))==1){ error=true;
													     strcpy(strings[c],symbol_table[c1].data_type);
												             strcat(strings[c]," not assignable to ");
													     strcat(strings[c],vartype);
													     strcat(strings[c]," at line ");
													     sprintf(str, "%d", lineNum+1);
													     strcat(strings[c],str);
													     c++;}
			                        else{
						if(strcmp(symbol_table[c1].data_type,vartype)!=0)
						{ s1=true;
						 strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); strcpy(num1,result); 
				                  strcat(result,"=");strcat(result,"(");strcat(result,vartype);strcat(result,")");strcat(result,symbol_table[c1].variable_name);
				                  SS_push(result);
						}
						}
						}
						if(c2>=0)
						{
						if(max(preference(symbol_table[c2].data_type),preference(vartype))==1){ error=true;
													     strcpy(strings[c],symbol_table[c2].data_type);
												             strcat(strings[c]," not assignable to ");
													     strcat(strings[c],vartype);
													     strcat(strings[c]," at line ");
													     sprintf(str, "%d", lineNum+1);
													     strcat(strings[c],str);
													     c++;}
						else{
						if(strcmp(symbol_table[c2].data_type,vartype)!=0)
						{ s2=true;
						  strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); strcpy(num2,result); 
				                  strcat(result,"=");strcat(result,"(");strcat(result,vartype);strcat(result,")");strcat(result,symbol_table[c2].variable_name);
				                  SS_push(result);
						}
						}
						}
						
						
						
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
						if(s1==false && s2==false)
						{					
						strcpy(num1, $1);
						strcpy(num2, $3);
						
						
						strcpy(oper[Q].operation, $2);
						strcpy(oper[Q].arg1, num1);
						strcpy(oper[Q].arg2, num2);
						strcpy(oper[Q].result, result);
						Q++;
						
					        int Num1 = GET_SYMBOL_VALUE(num1);
						int Num2 = GET_SYMBOL_VALUE(num2);
						int Result = CALCULATE(Num1, Num2, $2);
					
						
						GET_SYMBOL_VALUE(result);
						SET_SYMBOL_VALUE(result, Result);
						sprintf(str, "%s = %s %s %s", result, num1, $2, num2);
						SS_push(str);
						}
						else if(s1==true && s2==true)
						{
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						else if(s1==true &&  s2 == false)
						{
						if(c2>=0){
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,symbol_table[c2].variable_name);
						SS_push(str1);
						}
						else
						{
						strcpy(num2, $3);
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						}
						else if(s1==false && s2==true)
						{
						if(c1>=0){
						strcpy(str1,result);strcat(str1,"=");strcat(str1,symbol_table[c1].variable_name);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						else
						{
						strcpy(num1, $1);
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						}
						

						strcpy($$, result);
											
						strcpy(message,"Additive operation cannot be done");
				}
				;

T			: F 
				{

						strcpy($$, $1);
						
						
				}
				| T MULOP F
				{
				
						 char str[100], result[100], num1[100], num2[100], str1[100]; 
				                bool s1=false,s2=false;
				                int c1=-1,c2=-1;
				                // c1 and c2 positive implies variables are present in symbol table
				                // If they remain negative implies either it is a number or the variable is not present in symbol table which outputs error message.
				                for(int i=0;i<count;i++)
				                {
				                  if(strcmp(symbol_table[i].variable_name,$1)==0)
				                  {c1=i;break;}
				                }
				                for(int i=0;i<count;i++)
				                {
				                  if(strcmp(symbol_table[i].variable_name,$3)==0)
				                  {c2=i;break;}
				                }
				           
						if(c1>=0)
						{
						
						if(strcmp(symbol_table[c1].data_type,vartype)!=0)
						{ s1=true;
						 strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); strcpy(num1,result); 
				                  strcat(result,"=");strcat(result,"(");strcat(result,vartype);strcat(result,")");strcat(result,symbol_table[c1].variable_name);
				                  SS_push(result);
						}
						}
						if(c2>=0)
						{
						
						if(strcmp(symbol_table[c2].data_type,vartype)!=0)
						{ s2=true;
						  strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); strcpy(num2,result); 
				                  strcat(result,"=");strcat(result,"(");strcat(result,vartype);strcat(result,")");strcat(result,symbol_table[c2].variable_name);
				                  SS_push(result);
						}
						}
						
						
						
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
						if(s1==false && s2==false)
						{					
						strcpy(num1, $1);
						strcpy(num2, $3);
						
						
						strcpy(oper[Q].operation, $2);
						strcpy(oper[Q].arg1, num1);
						strcpy(oper[Q].arg2, num2);
						strcpy(oper[Q].result, result);
						Q++;
						
					        int Num1 = GET_SYMBOL_VALUE(num1);
						int Num2 = GET_SYMBOL_VALUE(num2);
						int Result = CALCULATE(Num1, Num2, $2);
					
						
						GET_SYMBOL_VALUE(result);
						SET_SYMBOL_VALUE(result, Result);
						sprintf(str, "%s = %s %s %s", result, num1, $2, num2);
						SS_push(str);
						}
						else if(s1==true && s2==true)
						{
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						else if(s1==true &&  s2 == false)
						{
						if(c2>=0){
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,symbol_table[c2].variable_name);
						SS_push(str1);
						}
						else
						{
						strcpy(num2, $3);
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						}
						else if(s1==false && s2==true)
						{
						if(c1>=0){
						strcpy(str1,result);strcat(str1,"=");strcat(str1,symbol_table[c1].variable_name);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						else
						{
						strcpy(num1, $1);
						strcpy(str1,result);strcat(str1,"=");strcat(str1,num1);strcat(str1,$2);strcat(str1,num2);
						SS_push(str1);
						}
						}
						

						strcpy($$, result);
						
												
						strcpy(message,"Multiplicative operation cannot be done!");
				}
				| F UPLUS
				{
				                char str[100], result[100], num[100],result2[100]; 
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
		
						sprintf(num, "%s", $1);
						
						
						strcpy(oper[Q].operation, "++");
						strcpy(oper[Q].arg1, "");
						strcpy(oper[Q].arg2, num);
						strcpy(oper[Q].result, result);
						Q++;
						
						
						sprintf(str, "%s = %s", result, num);
						SS_push(str);
						strcpy(result2, "T"); sprintf(str, "%d", ++T); strcat(result2,str); 
						sprintf(str, "%s = %s + 1", result2, result);
						SS_push(str);
						//++T;
						//sprintf(str, "%s = %s", num, result2);
						//SS_push(str);

						strcpy($$,result2);
						
											
						strcpy(message,"Unrecognized number/ID format!");
				}
				| F UMINUS
				{
				                char str[100], result[100], num[100],result2[100]; 
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
		
						sprintf(num, "%s", $1);
						
						
						strcpy(oper[Q].operation, "--");
						strcpy(oper[Q].arg1, "");
						strcpy(oper[Q].arg2, num);
						strcpy(oper[Q].result, result);
						Q++;
						
						
						sprintf(str, "%s = %s", result, num);
						SS_push(str);
						strcpy(result2, "T"); sprintf(str, "%d", ++T); strcat(result2,str); 
						sprintf(str, "%s = %s - 1", result2, result);
						SS_push(str);
						strcpy($$,result2);
						
											
						strcpy(message,"Unrecognized number/ID format!");
				}

				; 

F			: ID 
				{      
						strcpy($$,$1);
						
						GET_SYMBOL_VALUE($1);
						int t=0;
						                                    for(int i=0;i<count;i++)
                         				                            { if(strcmp(symbol_table[i].variable_name,$1)==0){t++;}}
											    if(t==0)
											   {error=true; 
											    strcpy(strings[c],"Var ");
											    strcat(strings[c],$1);
											    strcat(strings[c]," not declared in this scope");
											    c++;}
									            
                         
                         
                         										     
						
						
												
						strcpy(message,"Expecting something else!");
				}
				
				| NUM 
				
				{
						char str[100], result[100], num[100]; 
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
											
						sprintf(num, "%d", $1);
						
						
						strcpy(oper[Q].operation, "");
						strcpy(oper[Q].arg1, "");
						strcpy(oper[Q].arg2, num);
						strcpy(oper[Q].result, result);
						Q++;
						
						GET_SYMBOL_VALUE(result);
						SET_SYMBOL_VALUE(result, $1);
						
						
						
						sprintf(str, "%s = %s", result, num);
						SS_push(str);
						
						strcpy($$,result);				
													
						
						strcpy(message,"Unrecognized number format!");
				
				}
				| STRBO E STRBC 
				{
						strcpy(message,"'(' or ')' missing OR expression not found!");
				}
				| NOT F 
				{
						char str[100], result[100], num[100]; 
						strcpy(result, "T"); sprintf(str, "%d", ++T); strcat(result,str); 
						
		
						sprintf(num, "%s", $2);
						
						
						strcpy(oper[Q].operation, "!");
						strcpy(oper[Q].arg1, "");
						strcpy(oper[Q].arg2, num);
						strcpy(oper[Q].result, result);
						Q++;
						
						
						sprintf(str, "%s = !%s", result, num);
						SS_push(str);

						strcpy($$,result);
						
											
						strcpy(message,"Unrecognized number/ID format!");
				
				}; 
								
%%





int yywrap()
{
	
        return 1;
} 
extern FILE *yyin;
int main(int argc, char* argv[])
{
    if(argc > 1)
	{
		FILE *fp = fopen(argv[1], "r");
		if(fp)
			yyin = fp;
	}
	yyparse();
	                                          
	if(error)
	{
	for(int i=0;i<c;i++)
	{
	 printf("%s\n",strings[i]);
	}
	exit(0);
	}
	else{
	printf("\nSYMBOL      DATATYPE         OFFSET\n");
	printf("______________________________________\n\n");
	int i=0;
	
	for(i=0; i<count; i++) 
	{
	        if(strcmp(symbol_table[i].data_type,"intarray")==0)
		{printf("%s\t\t%s\t\t0x%04x\t\t%d\n", symbol_table[i].variable_name, symbol_table[i].data_type, symbol_table[i].address,arraybytes*4);}
		else if(strcmp(symbol_table[i].data_type,"chararray")==0)
		{printf("%s\t\t%s\t\t0x%04x\t\t%d\n", symbol_table[i].variable_name, symbol_table[i].data_type, symbol_table[i].address,arraybytes*1);}
		else if(strcmp(symbol_table[i].data_type,"floatarray")==0)
		{printf("%s\t\t%s\t\t0x%04x\t\t%d\n", symbol_table[i].variable_name, symbol_table[i].data_type, symbol_table[i].address,arraybytes*4);}
		else
		{printf("%s\t\t%s\t\t0x%04x\t\n", symbol_table[i].variable_name, symbol_table[i].data_type, symbol_table[i].address);}
	}
	}
	printf("\n\n");
	return 0;
}

void check(int start,int end)
	                               {       for(int i=start;i<end-1;i++)
                                                  {for(int j=i+1;j<end;j++)
                                                    {if(strcmp(symbol_table[i].variable_name,symbol_table[j].variable_name)==0)
                                                      {if(strcmp(symbol_table[i].data_type,symbol_table[j].data_type)==0)
                                                        {error=true;
                                                        strcpy(strings[c],"error: Redeclaration of ");
                                                        strcat(strings[c],symbol_table[i].variable_name);c++;break;
                                                        }
                                                        else
                                                        {error=true;
                                                        strcpy(strings[c],"error: conflicting types for ");
                                                        strcat(strings[c],symbol_table[i].variable_name);c++;break;
                                                        }
                                                      }
                                                    }
                                                  }
                                       }


// Widening Conversion
int preference(char str1[100])
{
  if(strcmp(str1,"char")==0)
  { return 1;}
  else if(strcmp(str1,"int")==0)
  { return 2;}
  else if(strcmp(str1,"long")==0)
  { return 3;}
  else if(strcmp(str1,"float")==0)
  { return 4;}
  else if(strcmp(str1,"double")==0)
  { return 5;}
}

int max(int t1,int t2)
{
  if(t1==t2)
  {return 0;}
  else if(t1>t2)
  {return 1;}
  else
  {return 2;}
}


int yyerror(char str[100])
{
				if(ErrorRecovered==0)
				{
					{
					
					
						printf("Error Found @ line #%d: ", lineNum+1);
						if(strcmp(str,"Invalid character")==0 || strcmp(str,"Identifier greater than 5 characters")==0)						
							printf("%s!", str);
						else if(strlen(message))
							printf("%s\n",message);
						else printf("%s\n", str);
					}
					printf("\n");
					ErrorRecovered = 1;

				}
		        
}
