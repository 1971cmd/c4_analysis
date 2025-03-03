// c4_annotated.c - Annotated version of the C4 compiler
// Original code by Robert Swierczek
// Annotations by Rashed AlKhajeh

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long  // Define 'int' as 'long long' for larger integer support

// Global variables
char *p, *lp,       // p: current position in source code, lp: start of current line
     *data;         // Pointer to data/bss section (for storing strings and global variables)

int *e, *le,        // e: current position in emitted code, le: end of emitted code
    *id,            // Pointer to the currently parsed identifier in the symbol table
    *sym,           // Symbol table (a simple list of identifiers)
    tk,             // Current token (e.g., Num, Id, '+', etc.)
    ival,           // Current token value (e.g., value of a number)
    ty,             // Current expression type (e.g., CHAR, INT, PTR)
    loc,            // Local variable offset in the stack
    line,           // Current line number in the source code
    src,            // Flag to print source and assembly code
    debug;          // Flag to print executed instructions (for debugging)

// Tokens and classes (operators are ordered by precedence)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,  // Basic tokens and identifier classes
  Char, Else, Enum, If, Int, Return, Sizeof, While,  // Keywords
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak  // Operators
};

// Opcodes for the virtual machine
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// Types supported by the compiler
enum { CHAR, INT, PTR };

// Identifier offsets (since we can't create a struct for identifiers)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

// Function to read the next token from the source code
void next() {
  char *pp;

  while (tk = *p) {  // Loop through the source code
    ++p;
    if (tk == '\n') {  // Handle newlines
      if (src) {  // If source printing is enabled
        printf("%d: %.*s", line, p - lp, lp);  // Print the current line
        lp = p;
        while (le < e) {  // Print emitted code
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;  // Increment line number
    }
    else if (tk == '#') {  // Skip preprocessor directives
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {  // Handle identifiers
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;  // Hash the identifier
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {  // Check if the identifier is already in the symbol table
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      id[Name] = (int)pp;  // Add new identifier to the symbol table
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk >= '0' && tk <= '9') {  // Handle numbers
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }  // Decimal
      else if (*p == 'x' || *p == 'X') {  // Hexadecimal
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }  // Octal
      tk = Num;
      return;
    }
    else if (tk == '/') {  // Handle comments or division operator
      if (*p == '/') {  // Single-line comment
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {  // Division operator
        tk = Div;
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {  // Handle character and string literals
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {  // Handle escape sequences
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;  // Store string literals in the data section
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;  // Return pointer to string or character value
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }  // Handle '==' and '='
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }  // Handle '++' and '+'
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }  // Handle '--' and '-'
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }  // Handle '!=' and '!'
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }  // Handle '<=', '<<', and '<'
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }  // Handle '>=', '>>', and '>'
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }  // Handle '||' and '|'
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }  // Handle '&&' and '&'
    else if (tk == '^') { tk = Xor; return; }  // Handle '^'
    else if (tk == '%') { tk = Mod; return; }  // Handle '%'
    else if (tk == '*') { tk = Mul; return; }  // Handle '*'
    else if (tk == '[') { tk = Brak; return; }  // Handle '['
    else if (tk == '?') { tk = Cond; return; }  // Handle '?'
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;  // Handle single-character tokens
  }
}

// Function to parse expressions
void expr(int lev) {
  int t, *d;

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }  // Handle unexpected end of file
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }  // Handle numeric literals
  else if (tk == '"') {  // Handle string literals
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();  // Concatenate adjacent string literals
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;  // Align data and set type to pointer
  }
  else if (tk == Sizeof) {  // Handle sizeof operator
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }  // Handle pointer types
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);  // Emit size
    ty = INT;
  }
  else if (tk == Id) {  // Handle identifiers
    d = id; next();
    if (tk == '(') {  // Function call
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }  // Push arguments onto the stack
      next();
      if (d[Class] == Sys) *++e = d[Val];  // System function call
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }  // User function call
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }  // Adjust stack after function call
      ty = d[Type];
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }  // Numeric constant
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }  // Local variable
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }  // Global variable
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;  // Load value based on type
    }
  }
  else if (tk == '(') {  // Handle parentheses and type casting
    next();
    if (tk == Int || tk == Char) {  // Type casting
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }  // Handle pointer types
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {  // Normal parentheses
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {  // Handle pointer dereference
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {  // Handle address-of operator
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }  // Handle logical NOT
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }  // Handle bitwise NOT
  else if (tk == Add) { next(); expr(Inc); ty = INT; }  // Handle unary plus
  else if (tk == Sub) {  // Handle unary minus
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {  // Handle increment/decrement operators
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) {  // Handle operator precedence using "precedence climbing"
    t = ty;
    if (tk == Assign) {  // Handle assignment
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {  // Handle ternary operator
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }  // Handle logical OR
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }  // Handle logical AND
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }  // Handle bitwise OR
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }  // Handle bitwise XOR
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }  // Handle bitwise AND
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }  // Handle equality
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }  // Handle inequality
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }  // Handle less than
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }  // Handle greater than
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }  // Handle less than or equal
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }  // Handle greater than or equal
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }  // Handle left shift
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }  // Handle right shift
    else if (tk == Add) {  // Handle addition
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }  // Adjust for pointer arithmetic
      *++e = ADD;
    }
    else if (tk == Sub) {  // Handle subtraction
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }  // Adjust for pointer arithmetic
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }  // Handle multiplication
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }  // Handle division
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }  // Handle modulus
    else if (tk == Inc || tk == Dec) {  // Handle post-increment/decrement
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {  // Handle array subscript
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }  // Adjust for pointer arithmetic
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }  // Handle unknown tokens
  }
}

// Function to parse statements
void stmt() {
  int *a, *b;

  if (tk == If) {  // Handle if statements
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);  // Parse condition
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;  // Emit branch-if-zero instruction
    stmt();  // Parse if block
    if (tk == Else) {  // Handle else clause
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);  // Set branch target
  }
  else if (tk == While) {  // Handle while loops
    next();
    a = e + 1;  // Save start of loop
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);  // Parse condition
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;  // Emit branch-if-zero instruction
    stmt();  // Parse loop body
    *++e = JMP; *++e = (int)a;  // Jump back to start of loop
    *b = (int)(e + 1);  // Set branch target
  }
  else if (tk == Return) {  // Handle return statements
    next();
    if (tk != ';') expr(Assign);  // Parse return value
    *++e = LEV;  // Emit leave function instruction
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') {  // Handle blocks
    next();
    while (tk != '}') stmt();  // Parse statements in the block
    next();
  }
  else if (tk == ';') {  // Handle empty statements
    next();
  }
  else {  // Handle expression statements
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

// Main function
int main(int argc, char **argv) {
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle;  // VM registers
  int i, *t;  // Temporary variables

  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }  // Enable source printing
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }  // Enable debug mode
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }  // Check for valid arguments

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }  // Open source file

  poolsz = 256 * 1024;  // Allocate memory pools
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym, 0, poolsz);  // Initialize memory pools
  memset(e, 0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; }  // Add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; }  // Add system functions
  next(); id[Tk] = Char;  // Handle void type
  next(); idmain = id;  // Track main function

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz - 1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // Parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT;  // Base type
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {  // Handle enums
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {  // Parse global declarations
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }  // Handle pointers
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') {  // Handle function declarations
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {  // Parse parameters
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType] = id[Type]; id[Type] = ty;
          id[HVal] = id[Val]; id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char) {  // Parse local variables
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType] = id[Type]; id[Type] = ty;
            id[HVal] = id[Val]; id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;  // Emit enter function instruction
        while (tk != '}') stmt();  // Parse function body
        *++e = LEV;  // Emit leave function instruction
        id = sym;  // Unwind symbol table
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {  // Handle global variables
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }  // Ensure main() is defined
  if (src) return 0;  // Exit if only printing source

  // Setup stack
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT;  // Call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // Run the virtual machine
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {  // Print debug information
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);  // Load local address
    else if (i == IMM) a = *pc++;  // Load global address or immediate
    else if (i == JMP) pc = (int *)*pc;  // Jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }  // Jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;  // Branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;  // Branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }  // Enter subroutine
    else if (i == ADJ) sp = sp + *pc++;  // Adjust stack
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; }  // Leave subroutine
    else if (i == LI)  a = *(int *)a;  // Load int
    else if (i == LC)  a = *(char *)a;  // Load char
    else if (i == SI)  *(int *)*sp++ = a;  // Store int
    else if (i == SC)  a = *(char *)*sp++ = a;  // Store char
    else if (i == PSH) *--sp = a;  // Push

    else if (i == OR)  a = *sp++ |  a;  // Bitwise OR
    else if (i == XOR) a = *sp++ ^  a;  // Bitwise XOR
    else if (i == AND) a = *sp++ &  a;  // Bitwise AND
    else if (i == EQ)  a = *sp++ == a;  // Equality
    else if (i == NE)  a = *sp++ != a;  // Inequality
    else if (i == LT)  a = *sp++ <  a;  // Less than
    else if (i == GT)  a = *sp++ >  a;  // Greater than
    else if (i == LE)  a = *sp++ <= a;  // Less than or equal
    else if (i == GE)  a = *sp++ >= a;  // Greater than or equal
    else if (i == SHL) a = *sp++ << a;  // Left shift
    else if (i == SHR) a = *sp++ >> a;  // Right shift
    else if (i == ADD) a = *sp++ +  a;  // Addition
    else if (i == SUB) a = *sp++ -  a;  // Subtraction
    else if (i == MUL) a = *sp++ *  a;  // Multiplication
    else if (i == DIV) a = *sp++ /  a;  // Division
    else if (i == MOD) a = *sp++ %  a;  // Modulus

    else if (i == OPEN) a = open((char *)sp[1], *sp);  // Open file
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);  // Read file
    else if (i == CLOS) a = close(*sp);  // Close file
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }  // Print formatted string
    else if (i == MALC) a = (int)malloc(*sp);  // Allocate memory
    else if (i == FREE) free((void *)*sp);  // Free memory
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);  // Set memory
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);  // Compare memory
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }  // Exit program
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }  // Handle unknown instructions
  }
}