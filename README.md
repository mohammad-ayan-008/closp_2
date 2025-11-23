

---

# CLOSP — A Small C-Like Language

CLOSP is a small, experimental C-style programming language implemented with a custom lexer, parser, semantic analyzer, and LLVM-based code generator.
The language is designed to look familiar to C programmers while remaining intentionally minimal.

This README covers:

* ✨ Language Features
* 📜 Syntax Overview
* 🧮 Example Program (Fibonacci)
* ⚠️ Current Limitations
* 🛠️ Build & Run
* 🚧 Roadmap

---

## ✨ **Language Features**

CLOSP currently supports:

* **Functions** with `fn` keyword
* **Static typing** (`int`, `char`, `void`)
* **Arithmetic** `+ - * / %`
* **Comparison operators** `== != <= >= < >`
* **Logical operators** `&& || !`
* **Control flow**:

  * `if`, `else`
  * `while` loops
* **Return statements**
* **`printf` and `scanf` passthrough** (mapped to C stdlib)
* **Character literals** `'A'`
* **Basic integer variables**

---

## 📜 **Syntax Overview**

### **Function Declaration**

```c
fn int functionName(int a, int b) {
    // ...
}
```

### **Variables**

```c
int a = 10;
char c = 'X';
```

### **Conditionals**

```c
if (x == 0) {
    return 1;
} else {
    return 2;
}
```

### **Loops**

```c
while (i < 10) {
    i = i + 1;
}
```

### **Function Calls**

```c
int result = fibo(10);
```

### **I/O**

(Directly forwarded to C `printf`/`scanf`)

```c
printf("Enter a number:");
scanf("%d", &num);
```

---

## 🧮 **Example Program (Fibonacci)**

### **Iterative Fibonacci**

```c
fn int fibo(int n){
  if (n == 0){
    return 0;
  }
  if (n == 1){
    return 1;
  }

  int a = 0;
  int b = 1;
  int next = 0;
  int i = 2;

  while (i <= n){
    next = a + b;
    a = b;
    b = next;
    i++;
  }

  return b;
}
```

### **Recursive Fibonacci**

```c
fn int fibo2(int a){
  if (a <= 1){
    return a;
  }
  return fibo(a-1) + fibo(a-2);
}
```

### **Main Function**

```c
fn int main(){
  char userinp = 'Y';
  while (userinp != 'N'){
    int number = 0;
    printf("Enter a number :");
    scanf("%d",&number);
    printf("%d th Fibonnaci is %d\n", number, fibo2(number));

    printf("Enter a choice \n");
    printf("Enter Y to Continue \n");
    printf("Enter N to exit \n");
    scanf(" %c", &userinp);
  }
  return 0;
}
```

---

## ⚠️ **Current Limitations**

CLOSP is still in development. The following features are **NOT supported yet**:

* ❌ **Arrays**

  * `Char* a = "ayan";` → **Works** (pointer-like)
  * `Char[100] a = "ayan";` → **Not supported**
* ❌ No dynamic memory
* ❌ No structs
* ❌ No operator overloading
* ❌ No modules / headers

---

## 🛠️ **Build & Run**

### **1. Compile your CLOSP source file**

```bash
closp input.closp -o output.ll
```

### **2. Use LLVM to build executable**

```bash
llc output.ll -filetype=obj -o out.o
clang out.o -o a.out
```

### **3. Run**

```bash
./a.out
```

---

## 🚧 **Roadmap**

Planned future features:

* [ ] Proper Array Support
* [ ] String literals as real array values
* [ ] For-loops
* [ ] Function overloading
* [ ] Type inference
* [ ] Standard library additions
* [ ] Error reporting improvements

---

## 📄 License

MIT License (optional — customize if needed)

---

