/* tomtorfs@village.uunet.be (Tom Torfs)
   PCalc - programmer's pocket calculator
   Written by Tom Torfs (tomtorfs@mail.dma.be, 2:292/516)
   Donated to the public domain, 1997-09-14
   Last update 1998-09-11, added interactive mode */

/* Execute PCalc with parameter ? for usage info */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* set this to the precision (# of digits) of a double on your system
   15 is the default for a 64-bit IEEE double as used on the 80x87 */
#ifndef DOUBLE_PREC
#define DOUBLE_PREC 15
#endif

/* set this to 1 or 2 depending on whether you want negative numbers
   in other bases to be displayed in 1- or 2-complement representation */

#ifndef COMPLEMENT
#define COMPLEMENT 2
#endif

/* goniometric mode settings */
enum {RADIANS,DEGREES};

/* special codes used in encoded expression */
enum {CODE_START='A',
      CODE_ABS=CODE_START,CODE_ROUND,
      CODE_COS,CODE_SIN,CODE_TAN,
      CODE_ACOS,CODE_ASIN,CODE_ATAN,
      CODE_COSH,CODE_SINH,CODE_TANH,
      CODE_EXP,CODE_LOG,CODE_LOG10,
      CODE_END,
      CODE_NUM=128};

/* predefined constants */
#define CONST_PI 3.14159265358979323846
#define CONST_K  1024.0
#define CONST_M  1048576.0
#define CONST_G  1073741824.0

/* table where numbers are stored while processing */
int numbers;
double numtable[127];

/* goniometric mode & output base, cmdline will override defaults */
int goniomode = RADIANS;
int outbase = 10;

/* set to 1 if precision is lost during an operation */
int preclost;

/* check for internal overflow (e.g. expression buffer or number table)
*/
void cassert(int condition)
{
   if (!condition)
   {
      printf("Internal overflow error; please reduce expression complexity\n");
      exit(EXIT_FAILURE);
   }
}

/* check whether the parentheses match, i.o.w. for each ( there is a ) */
int checkparen(char *expr)
{
  int parlevel = 0;
  int i;

  for (i=0; expr[i]!='\0'; i++)
  {
     if (expr[i]=='(')
        parlevel++;
     else if (expr[i]==')')
        parlevel--;
  }

  return (parlevel==0);
}

/* returns the deepest level of nested parentheses, 0 is top level */
int deepestlevel(char *expr)
{
  int parlevel = 0, maxlevel = 0;
  int i;

  for (i=0; expr[i]!='\0'; i++)
  {
     if (expr[i]=='(')
     {
        parlevel++;
        if (parlevel>maxlevel)
           maxlevel = parlevel;
     }
     else if (expr[i]==')')
        parlevel--;
  }

  return maxlevel;
}

/* check a token: case-insensitive, length must match */
int tokencmp(char *expr, const char *token, int tokenlen)
{
   if (tokenlen!=strlen(token))
      return 1;
   return memicmp(expr,token,tokenlen);
}

/* displays integral part of number with base numbase (2..16)
   negative numbers are displayed in 1- or 2-complement
   representation depending on the setting COMPLEMENT
   max. 64 bits, decimals ignored */
void displaybasenum(double num, int numbase)
{
   static char digit[16] = "0123456789ABCDEF";
   static char numbuf[65];
   int numpos;
   int value;
   int sign;

   if (num<0.0)
   {
      sign = 1;
      num = -(num+COMPLEMENT-1);
   }
   else
      sign = 0;

   numpos = 64;
   numbuf[numpos] = '\0';
   while (numpos>0)
   {
      value = (int)fmod(num,numbase);
      if (sign)
         value = numbase - (value+1);
      numbuf[--numpos] = digit[value];
      if (num==0)
         break;
      num = floor(num/numbase);
      if (num==0 && numpos!=64)
         break;
   }

   /* complete first group of 4 digits */
   while (numpos%4>0)
      numbuf[--numpos] = digit[sign?(numbase-1):0];

   for (; numpos<64; numpos+=4)
      printf(" %.4s",numbuf+numpos);
}

/* scans integral number with base numbase (2..16)
   returns 0 on success, 1-based column number on syntax error */
int scanbasenum(char *expr, int tokenlen, int numbase, double *num)
{
   int i;
   int digval;

   *num = 0.0;
   for (i=0; i<tokenlen; i++)
   {
      *num *= numbase;
      if (isdigit(expr[i]))
         digval = expr[i] - '0';
      else if (isxdigit(expr[i]))
         digval = toupper(expr[i]) - 'A' + 10;
      else
         digval = -1;
      if (digval<0 || digval>=numbase)
         return 1+i;
      *num += digval;
   }

   return 0;
}

/* scans a number token
   returns 0 on success, 1-based column number on syntax error */
int scannum(char *expr, int tokenlen, double *num)
{
   int numbase;
   double value;
   char *endptr;
   int badcol;

   /* empty token -> error */
   if (tokenlen==0)
      return 1;

   /* check constants */
   *num = 1.0;
   if (tokenlen>=2 && memicmp(expr+tokenlen-2,"pi",2)==0)
   {
      *num = CONST_PI;
      tokenlen -= 2;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"k",1)==0)
   {
      *num = CONST_K;
      tokenlen--;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"m",1)==0)
   {
      *num = CONST_M;
      tokenlen--;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"g",1)==0)
   {
      *num = CONST_G;
      tokenlen--;
   }

   /* constant only, no preceding number -> ok */
   if (tokenlen==0)
      return 0;

   /* check base specifiers */
   numbase = 10;
   if (tokenlen>=1 && memicmp(expr+tokenlen-1,"d",1)==0)
   {
      numbase = 10;
      tokenlen--;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"h",1)==0)
   {
      numbase = 16;
      tokenlen--;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"o",1)==0)
   {
      numbase = 8;
      tokenlen--;
   }
   else if (tokenlen>=1 && memicmp(expr+tokenlen-1,"b",1)==0)
   {
      numbase = 2;
      tokenlen--;
   }

   /* base specifier only -> error */
   if (tokenlen==0)
      return 1;

   if (numbase==10)
   {
      value = strtod(expr,&endptr);
      if (value==HUGE_VAL)
         return 1;
      badcol = endptr - expr;
      if (badcol!=tokenlen)
         return 1+badcol;
   }
   else
   {
      if ((badcol=scanbasenum(expr,tokenlen,numbase,&value))!=0)
         return badcol;
   }

   *num *= value;

   return 0;
}

/* encodes expression for processing:
   replaces numbers & constants by CODE_NUM + index into number table
   replaces function names by codes according to code table
   returns 0 success, 1-based column number on syntax error */
int encodeexpr(char *expr, char *codeexp)
{
   int i,codelen;
   int tokenlen;
   int code;
   int badcol;

   numbers = 0;
   codelen = 0;
   for (i=0; expr[i]!='\0';)
   {
      if (expr[i]=='+' || expr[i]=='-' || expr[i]=='*' || expr[i]=='/'
          || expr[i]=='%' || expr[i]=='^' || expr[i]=='!'
          || expr[i]=='(' || expr[i]==')')
      {
         /* operator -> just copy */
         cassert(codelen<255);
         if (expr[i]=='*' && expr[i+1]=='*')
         {
            /* ** is alternative for ^ because some command shells
               assign a special meaning to ^ */
            codeexp[codelen++] = '^';
            i+=2;
         }
         else
         {
            codeexp[codelen++] = expr[i];
            i++;
         }
      }
      else if (isalnum(expr[i]) || expr[i]=='.')
      {
         tokenlen = 1;
         while (isalnum(expr[i+tokenlen]) || expr[i+tokenlen]=='.'
                || ((expr[i+tokenlen]=='+' || expr[i+tokenlen]=='-')
                    && tolower(expr[i+tokenlen-1])=='e'))
            tokenlen++;
         if (tokencmp(expr+i,"abs",tokenlen)==0)
            code = CODE_ABS;
         else if (tokencmp(expr+i,"round",tokenlen)==0)
            code = CODE_ROUND;
         else if (tokencmp(expr+i,"cos",tokenlen)==0)
            code = CODE_COS;
         else if (tokencmp(expr+i,"sin",tokenlen)==0)
            code = CODE_SIN;
         else if (tokencmp(expr+i,"tan",tokenlen)==0)
            code = CODE_TAN;
         else if (tokencmp(expr+i,"acos",tokenlen)==0)
            code = CODE_ACOS;
         else if (tokencmp(expr+i,"asin",tokenlen)==0)
            code = CODE_ASIN;
         else if (tokencmp(expr+i,"atan",tokenlen)==0)
            code = CODE_ATAN;
         else if (tokencmp(expr+i,"cosh",tokenlen)==0)
            code = CODE_COSH;
         else if (tokencmp(expr+i,"sinh",tokenlen)==0)
            code = CODE_SINH;
         else if (tokencmp(expr+i,"tanh",tokenlen)==0)
            code = CODE_TANH;
         else if (tokencmp(expr+i,"exp",tokenlen)==0)
            code = CODE_EXP;
         else if (tokencmp(expr+i,"log",tokenlen)==0)
            code = CODE_LOG;
         else if (tokencmp(expr+i,"log10",tokenlen)==0)
            code = CODE_LOG10;
         else
         {
            cassert(numbers<127);
            code = CODE_NUM + numbers;
            if ((badcol=scannum(expr+i,tokenlen,numtable+numbers))!=0)
               return i+badcol;
            numbers++;
         }
         cassert(codelen<255);
         codeexp[codelen++] = (char)code;
         i += tokenlen;
      }
      else
         return 1+i;
   }
   codeexp[codelen] = '\0';

   return 0;
}

/* marks sub-expressions with any of the specified operators
   should be called in the right order of precedence
   unary: 1=unary operator,right-to-left; 0=binary operator,left-to-
right
   binary: 1=don't allow unary form of operator
   leftop: 1=process only operators on the right of high-level operator
   returns 0 success, 1-based column number on syntax error */
int marksubexpr(char *expr, const char *operators,
                int unary, int binary, int leftop)
{
   int i;
   int parlevel;
   char *left,*right;

   for (i=unary?(strlen(expr)-1):0; i>=0 && expr[i]!='\0';i+=unary?(-1):
(+1))
   {
      /* check for operator */
      if (strchr(operators,expr[i])==NULL)
         continue;
      if (leftop)
      {
         /* check for high-level operator on the left */
         if (i==0 || (expr[i-1]!='^' && expr[i-1]!='!'))
            continue;
      }
      /* check for valid left operand or subexpression */
      if (i==0 || (expr[i-1]!=')' && (unsigned char)expr[i-1]<CODE_NUM))

      {
         if (binary)
            return i+1;
         else if (!unary)
            continue;
      }
      else if (unary)
         continue;   /* we only deal with the unary operators now */
      /* check for valid right operand or sub-expression */
      if (expr[i+1]!='(' && (unsigned char)expr[i+1]<CODE_NUM
          && (expr[i+1]<CODE_START || expr[i+1]>=CODE_END))
         return i+1;
      if (i>0 && (unsigned char)expr[i-1]>=CODE_NUM)
         left = expr+i-1;
      else if (i>0 && expr[i-1]==')')
      {
         parlevel = 1;
         left = expr+i-1;
         while (parlevel!=0)
         {
            if (left==expr)
               return i+1;
            left--;
            if (*left==')')
               parlevel++;
            else if (*left=='(')
               parlevel--;
         }
      }
      else
         left = expr+i;
      /* check for function */
      if (left>expr && left[-1]>=CODE_START && left[-1]<CODE_END)
         left--;
      right = expr+i;
      /* check for function */
      if (right[1]>=CODE_START && right[1]<CODE_END)
         right++;
      if ((unsigned char)right[1]>=CODE_NUM)
         right++;
      else if (right[1]=='(')
      {
         parlevel = 1;
         right++;
         while (parlevel!=0)
         {
            right++;
            if (*right=='\0')
               return i+1;
            if (*right=='(')
               parlevel++;
            else if (*right==')')
               parlevel--;
         }
      }
      cassert(strlen(expr)<254);
      memmove(right+3,right+1,strlen(right+1)+1);
      *(right+2) = ')';
      memmove(left+1,left,right-left+1);
      *left = '(';
      if (unary)
         i--;
      else
         i+=2;
   }

   return 0;
}

/* process function code on number stored in entry num
   returns 0 on success or 1 on error (unknown function) */
int processfunc(int num, char code)
{
   /* math library must deal with divide by zero, domain error etc. */

   switch(code)
   {
   case CODE_ABS:
      numtable[num] = fabs(numtable[num]);
      break;
   case CODE_ROUND:
      if (numtable[num]<0.0)
         numtable[num] = ceil(numtable[num]-0.5);
      else
         numtable[num] = floor(numtable[num]+0.5);
      break;
   case CODE_COS:
      if (goniomode==DEGREES)
         numtable[num] *= CONST_PI / 180;
      numtable[num] = cos(numtable[num]);
      break;
   case CODE_SIN:
      if (goniomode==DEGREES)
         numtable[num] *= CONST_PI / 180;
      numtable[num] = sin(numtable[num]);
      break;
   case CODE_TAN:
      if (goniomode==DEGREES)
         numtable[num] *= CONST_PI / 180;
      numtable[num] = tan(numtable[num]);
      break;
   case CODE_ACOS:
      numtable[num] = acos(numtable[num]);
      if (goniomode==DEGREES)
         numtable[num] *= 180 / CONST_PI;
      break;
   case CODE_ASIN:
      numtable[num] = asin(numtable[num]);
      if (goniomode==DEGREES)
         numtable[num] *= 180 / CONST_PI;
      break;
   case CODE_ATAN:
      numtable[num] = atan(numtable[num]);
      if (goniomode==DEGREES)
         numtable[num] *= 180 / CONST_PI;
      break;
   case CODE_COSH:
      numtable[num] = cosh(numtable[num]);
      break;
   case CODE_SINH:
      numtable[num] = sinh(numtable[num]);
      break;
   case CODE_TANH:
      numtable[num] = tanh(numtable[num]);
      break;
   case CODE_EXP:
      numtable[num] = exp(numtable[num]);
      break;
   case CODE_LOG:
      numtable[num] = log(numtable[num]);
      break;
   case CODE_LOG10:
      numtable[num] = log10(numtable[num]);
      break;
   default:
      return 1;
   }

   /* check for precision underflow */
   if (fabs(numtable[num])>0.0 && fabs(numtable[num])<pow(10.0,-DOUBLE_PREC))
      preclost = 1;

   return 0;
}

/* process [<num2><operator>]<num> and store result entry num
   returns 0 on success or 1 on error (unknown function) */
int processop(int num, int num2, char opcode)
{
   /* math library must deal with divide by zero, domain error etc. */

   switch(opcode)
   {
   case '+':
      if (num2>=0)
         numtable[num] = numtable[num2] + numtable[num];
      break;
   case '-':
      if (num2>=0)
         numtable[num] = numtable[num2] - numtable[num];
      else
         numtable[num] = -numtable[num];
      break;
   case '*':
      if (num2>=0)
         numtable[num] = numtable[num2] * numtable[num];
      break;
   case '/':
      if (num2>=0)
         numtable[num] = numtable[num2] / numtable[num];
      break;
   case '%':
      if (num2>=0)
         numtable[num] = fmod(numtable[num2],numtable[num]);
      break;
   case '^':
      if (num2>=0)
         numtable[num] = pow(numtable[num2],numtable[num]);
      break;
   case '!':
      if (num2>=0)
         numtable[num] = pow(numtable[num2],1/numtable[num]);
      break;
   }

   /* check for precision underflow */
   if (fabs(numtable[num])>0.0 && fabs(numtable[num])<pow(10.0,-DOUBLE_PREC))
      preclost = 1;

   return 0;
}

/* process a single sub-expression of the form
      [<func>]([[<number>]<operator>]<number>)
   the result is stored in the latter number's entry
   returns 0 on success or 1 on error */
int processsub(char *expr, int explen)
{
   int expos;
   int numl,numr;
   int opcode;

   if (explen<2)
      return 1;
   expos = explen-2;

   if ((unsigned char)expr[expos]<CODE_NUM)
      return 1;
   numr = (unsigned char)expr[expos] - CODE_NUM;
   expos--;

   if (expr[expos]!='(')
   {
      opcode = expr[expos];
      expos--;
   }
   else
      opcode = 0;

   if (expr[expos]!='(')
   {
      if ((unsigned char)expr[expos]<CODE_NUM)
         return 1;
      numl = (unsigned char)expr[expos] - CODE_NUM;
      expos--;
   }
   else
      numl = -1;

   if (processop(numr,numl,opcode)!=0)
      return 1;

   if (expr[expos]!='(')
      return 1;

   if (expos!=0
       && (expos!=1 || processfunc(numr,expr[expos-1])!=0))
      return 1;

   expr[0] = (char)(CODE_NUM + numr);
   memmove(expr+1,expr+explen,strlen(expr+explen)+1);

   return 0;
}

/* process all sub-expressions of the specified nesting level
   (should be the deepest level)
   returns 0 on success or 1 on error */
int processlevel(char *expr, int level)
{
   int parlevel = 0;
   int expstart;
   int i;

   for (i=0; expr[i]!='\0'; i++)
   {
      if (expr[i]=='(')
      {
         parlevel++;
         if (parlevel==level)
            expstart = i;
      }
      else if (expr[i]==')')
      {
         if (parlevel==level)
         {
            if (expstart>0
                && expr[expstart-1]>=CODE_START && expr[expstart-1]<CODE_END)
               expstart--;
            if (processsub(expr+expstart,i-expstart+1)!=0)
               return 1;
            i = expstart;
         }
         parlevel--;
      }
   }

   return 0;
}

/* calculate expression */
int calc(char *expr)
{
   static char codeexp[256];
   int badcol;
   int level;
   int result;

   /* check for matching parentheses */
   if (!checkparen(expr))
   {
      printf("Syntax error: parentheses mismatch\n");
      return EXIT_FAILURE;
   }

   /* translate numbers, constants and functions into codes */
   if ((badcol=encodeexpr(expr,codeexp))!=0)
   {
      printf("Syntax error: %s\n",expr);
      printf("             %*c^\n",badcol,' ');
      return EXIT_FAILURE;
   }

   /* enclose all subexpressions in parentheses in order of precedence */
   /* unary operators on right of high-level operators */
   badcol = marksubexpr(codeexp,"+-",1,0,1);
   if (badcol==0) /* power & root */
      badcol = marksubexpr(codeexp,"^!",0,1,0);
   if (badcol==0) /* unary operators */
      badcol = marksubexpr(codeexp,"+-",1,0,0);
   if (badcol==0) /* multiplication & divide */
      badcol = marksubexpr(codeexp,"*/%",0,1,0);
   if (badcol==0) /* addition & substract */
      badcol = marksubexpr(codeexp,"+-",0,0,0);
   if (badcol!=0)
   {
      printf("Syntax error in sub-expression near \'%c\'\n",codeexp[badcol-1]);
      return EXIT_FAILURE;
   }

   preclost = 0;

   for (level=deepestlevel(codeexp); level>=0; level--)
   {
      if (processlevel(codeexp,level)!=0)
      {
         printf("Syntax error: sub-expression consistency mismatch\n");
         return EXIT_FAILURE;
      }
   }

   if ((unsigned char)codeexp[0]<CODE_NUM || codeexp[1]!='\0')
   {
      printf("Syntax error: top-level consistency mismatch\n");
      return EXIT_FAILURE;
   }
   result = (unsigned char)codeexp[0]-CODE_NUM;

   if (preclost)
      printf("warning: precision lost; results probably unreliable\n");

   printf("%s = ",expr);
   printf("%.*lg",(int)DOUBLE_PREC,numtable[result]);
   if (outbase!=10)
   {
      printf(" (base %d:",outbase);
      if (fabs(numtable[result])>=pow(outbase,64.0))
         printf(" out of range");
      else
         displaybasenum(numtable[result],outbase);
      printf(")");
   }
   printf("\n");

   return EXIT_SUCCESS;
}

int usage(void)
{
   printf("Usage     : PCALC <expression> [<options>]\n");
   printf("Options   : rad = use radians (default), deg = use degrees\n");
   printf("            dec = output decimal (default), hex, oct, bin (integer part)\n");
   printf("<expression> may consist of a combination of the following:\n");
   printf("Numbers   : double precision floating points, suffixes allowed are:\n");
   printf("            d = decimal (default)\n");
   printf("            h = hexadecimal, o = octal, b = binary (integers only)\n");
   printf("Constants : pi (3.1415...), k (2^10), m (2^20), g (2^30)\n");
   printf("Operators : + add        - substract    * multiply     / divide\n");
   printf("            %% modulo     ^ or ** power  ! n-root       () precedence\n");
   printf("Functions : abs() = absolute value, round() = round off to integer\n");
   printf("            cos() = cosine, sin() = sine, tan() = tangent\n");
   printf("            acos() = arccos, asin() = arcsin, atan() = arctan\n");
   printf("            cosh() = cos hyp, sinh() = sin hyp, tanh() = tan hyp\n");
   printf("            exp() = e^x, log() = natural log, log10() = base 10 log\n");
   printf("Examples  : sin(.75)/2pi, log(8k)/log(2), 64!2 bin, round(10/3)\n");

   return EXIT_FAILURE;
}

int main(int argc, char *argv[])
{
   static char expbuf[128];
   int explen;
   int i,j;

   printf("PCalc - programmer's pocket calculator, "
          "public domain by Tom Torfs\n\n");

   /* process options, combine expression & remove whitespace */
   explen = 0;
   for (i=1; i<argc; i++)
   {
      if (stricmp(argv[i],"-h")==0)
         return usage();
      else if (stricmp(argv[i],"rad")==0)
         goniomode = RADIANS;
      else if (stricmp(argv[i],"deg")==0)
         goniomode = DEGREES;
      else if (stricmp(argv[i],"dec")==0)
         outbase = 10;
      else if (stricmp(argv[i],"hex")==0)
         outbase = 16;
      else if (stricmp(argv[i],"oct")==0)
         outbase = 8;
      else if (stricmp(argv[i],"bin")==0)
         outbase = 2;
      else
      {
         for (j=0; argv[i][j]!='\0'; j++)
         {
            if (!isspace(argv[i][j]) && explen<127)
               expbuf[explen++] = argv[i][j];
         }
      }
   }
   expbuf[explen] = '\0';

   if (goniomode==DEGREES)
      printf("Auto-translation to/from degrees for goniometric functions is enabled.\n\n");

   /* expression -> non-interactive mode */
   if (explen!=0)
      return calc(expbuf);

   /* no expression -> interactive mode */
   for (;;)
   {
      printf("Enter expression to calculate: ");
      fflush(stdout);
      fgets(expbuf,sizeof expbuf,stdin);
      explen = strlen(expbuf);
      if (expbuf[explen-1]=='\n')
         expbuf[--explen] = '\0';
      if (explen==NULL)
         break;
      calc(expbuf);
      printf("\n");
   }

   return 0;
}
