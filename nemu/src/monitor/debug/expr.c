#include "nemu.h"

/* We use the POSIX regex functions to process regular expressions.
 * Type 'man regex' for more information about POSIX regex functions.
 */
#include <sys/types.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
enum {
  TK_NOTYPE = 256, TK_EQ,NUM,ADD,MINUS,MUL,DIV,LBRACKET,RBRACKET,REG,HEX.AND,OR,NEQ

  /* TODO: Add more token types */

};

static struct rule {
  char *regex;
  int token_type;
} rules[] = {

  /* TODO: Add more rules.
   * Pay attention to the precedence level of different rules.
   */

  {" +", TK_NOTYPE},    // spaces
  {"\\+", '+'},         // plus
  {"==", TK_EQ},         // equal
  {"0[xX][0-9a-fA-F]+",HEX},		//hex number
  {"[0-9]+",NUM},
  {"\\-",MINUS},
  {"\\*",MUL},
  {"\\/",DIV},
  {"\\(",LBRACKET},
  {"\\)",RBRACKET},
  {"\\$e[abc]x",REG},
  {"\\$e[bs]p",REG},
  {"\\$e[sd]i",REG},
  {"\\$eip",REG},
  {"&&",AND},
  {"\\|\\|",OR},
  {"!=",NEQ}
};

#define NR_REGEX (sizeof(rules) / sizeof(rules[0]) )

static regex_t re[NR_REGEX];

/* Rules are used for many times.
 * Therefore we compile them only once before any usage.
 */
void init_regex() {
  int i;
  char error_msg[128];
  int ret;

  for (i = 0; i < NR_REGEX; i ++) {
    ret = regcomp(&re[i], rules[i].regex, REG_EXTENDED);
    if (ret != 0) {
      regerror(ret, &re[i], error_msg, 128);
      panic("regex compilation failed: %s\n%s", error_msg, rules[i].regex);
    }
  }
}

typedef struct token {
  int type;
  char str[32];
} Token;

Token tokens[32];
int nr_token;

static bool make_token(char *e) {
  int position = 0;
  int i;
  regmatch_t pmatch;

  nr_token = 0;

  while (e[position] != '\0') {
    /* Try all rules one by one. */
    for (i = 0; i < NR_REGEX; i ++) {
      if (regexec(&re[i], e + position, 1, &pmatch, 0) == 0 && pmatch.rm_so == 0) {
        char *substr_start = e + position;
        int substr_len = pmatch.rm_eo;

      //  Log("match rules[%d] = \"%s\" at position %d with len %d: %.*s",
        //    i, rules[i].regex, position, substr_len, substr_len, substr_start);
        position += substr_len;

        /* TODO: Now a new token is recognized with rules[i]. Add codes
         * to record the token in the array `tokens'. For certain types
         * of tokens, some extra actions should be performed.
         */
		tokens[nr_token].type=rules[i].token_type;

        switch (rules[i].token_type) {
				case TK_NOTYPE:
						break;
				case NUM:
				case REG:
				case HEX:
						for(int j=0;j<substr_len;j++){
							tokens[nr_token].str[j]=substr_start[j];
						}
						tokens[nr_token].str[substr_len]='\0';
						nr_token++;
						break;
				case ADD:
				case MINUS:
				case MUL:
				case DIV:
				case LBRACKET:
				case RBRACKET:
						tokens[nr_token].str[0]=substr_start[0];
						tokens[nr_token++].str[1]='\0';
						break;
				case AND:
				case OR:
				case TK_EQ:
				case NEQ:
						tokens[nr_token].str[0]=substr_start[0];
						tokens[nr_token].str[1]=substr_start[1];
						tokens[nr_token++].str[2]='\0';
						break;


          default: TODO();
        }

        break;
      }
    }

    if (i == NR_REGEX) {
      printf("no match at position %d\n%s\n%*.s^\n", position, e, position, "");
      return false;
    }
  }

  return true;
}

bool judge_exp();
uint32_t eval(int p,int q);
uint32_t getnum(char str);
bool check_parentheses(int p,int q);
int find_dominant_operator(int p,int q);
int priority(int i);



uint32_t expr(char *e, bool *success) {
  if (!make_token(e)) {
    *success = false;
    return 0;
  }

  /* TODO: Insert codes to evaluate the expression. */
  //TODO();
  if(!judge_exp())
		  *success=false;
  else
		  return eval(0,nr_token-1);
  return 0;
}
bool judge_exp(){
	int cnt=0;
	for(int i=0;i<=nr_token;i++){
		if(token[i].type==LBRACKET){
			cnt++;
		}
		else if(token[i].type==RBRACKET)
				cnt--;
		if(cnt<0)
				return false;
	}
	return true;
}


uint32_t eval(int p.int q){
	if(p>q)
			return -1;
	else if(p==q){
		if(tokens[p].type==NUM)
				return atoi(tokens[p].str);
		else if(tokens[p].type==REG){
			if(strcmp(tokens[p].str,"$eax")==0) return cpu.eax;
			else if(strcmp(tokens[p].str,"$ebx")==0) return cpu.ebx;
			else if(strcmp(tokens[p].str,"$ecx")==0) return cpu.ecx;
			else if(strcmp(tokens[p].str,"$edx")==0) return cpu.edx;
			else if(strcmp(tokens[p].str,"$ebp")==0) return cpu.ebp;
			else if(strcmp(tokens[p].str,"$esp")==0) return cpu.esp;
			else if(strcmp(tokens[p].str,"$esi")==0) return cpu.esi;
			else if(strcmp(tokens[p].str,"$edi")==0) return cpu.edi;
			else if(strcmp(tokens[p].str,"$eip")==0) return cpu.eip;
		}
		else if(tokens[p].type==HEX){
			int len=strlen(tokens[p].str);
			int cnt=1;
			int sum=0;
			for(int i=len-1;i>=0;i--){
				sum+=cnt*getnum(tokens[p].str[i]);
				cnt*=16;
			}
			return sum;
		}
	}
	else if(check_parentheses(p,q))
			return eval(p+1,q-1);
	else{
		int op=find_dominant_operator(p,q);
		uint32_t leftval=eval(p,op-1);
		uint32_t rightval=eval(op+1,q);
		switch (token[op].type){
			case ADD:
					return leftval+rightval;
			case MINUS:
					return leftval-rightval;
			case MUL:
					return leftval*rightval;
			case DIV:
					return leftval/rightval;
			case AND:
					return leftval&&rightval;
			case OR:
					return leftval||rightval;
			case TK_EQ:
					return leftval==rightval;
			case NEQ:
					return leftval!=rightval;	
			default:
				assert(0);	
		}
	}
	return 1;
}

bool check_parentheses(int p,int q){
	int branch=0;
	for(int i=p;i<=q;i++){
		if(tokens[i].type==LBRACKET)
				branch++;
		if(tokens[i].type==RBRACKET)
				branch--;
		if(branch==0&&i<q)
				return false;
	}
	return true;
}
int find_dominant_operator(int p,int q){
	int cnt;
	int op=0,opp,pos=-1;
	for(int i=p;i<=q;i++){
		if(tokens[i].type==NUM||tokens[i].type==REG||tokens[i].type==HEX){
			continue;
		}
		else if(tokens[i].type==LBRACKET){
			cnt=0;
			for(int j=i+1;j<=q;j++){
				if(tokens[j].type==RBRACKET){
					cnt++;
					i+=cnt;
					break;
				}
				else
						cnt++;
			}
		}
		else{
			opp=priority(i);
			if(opp>=op){
				pos=i;
				op=opp;
			}
		}
	}
	return pos;
}
int priority(int i){
	if(tokens[i].type==ADD||tokens[i].type==MINUS)	return 4;
	else if(tokens[i].type==MUL||tokens[i].type==DIV)  return 3;
	else if(tokens[i].type==OR) return 12;
	else if(tokens[i].type==AND) return 11;
	else if(tokens[i].type==NEQ||tokens[i].type==TK_EQ)  return 7;
	return 0;
}
uint32_t getnum(char str){
	if(str>='0'&&str<='9'){
		return str-'0';
	}
	else if(str>='a'&&str<='f')
			return str-'a'+10;
	else if(str>='A'&&str<='F')
			return str-'A'+10;
	return 0;
}




















