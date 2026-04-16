# Analysis Classification Exercise

## Instructions
For each code snippet in `code-samples.md`, fill in the table below.

**Objective categories:** Correctness, Security, Performance
**Detection method:** Static, Dynamic, Both

---

| Snippet | Issue Description | Objective | Detection Method | Explanation |
|---------|-------------------|-----------|-----------------|-------------|
| 1 | SQL injection from concatenating user input into query | Security | Both | Unsanitized user input; static analyzers look for string building, dynamic tests can exercise injection |
| 2 | Unreachable console.log after return | Correctness | Static | Code after return never executes; lint tools flag unreachable code |
| 3 | Division by zero when list empty | Correctness | Dynamic | Runtime error occurs if numbers list is empty, static tools rarely know list length |
| 4 | Buffer overflow / missing null terminator copying string | Security | Both | C code writes past bounds; static analysis detects unsafe loops, dynamic testing can overflow memory |
| 5 | Off-by-one index error (`<= items.length`) causing undefined access | Correctness | Both | Linter can warn about loop bounds; running with sample data causes crash |
| 6 | Exponential recursion leads to poor performance | Performance | Dynamic | Algorithmic inefficiency exposed by running with large n; static tools typically don't evaluate complexity |
| 7 | Resource leak: FileInputStream never closed | Performance | Static | Leak can be caught by static analyzers; also affects runtime but harder to detect dynamically without long runs |
| 8 | Shell command built from user input (command injection) | Security | Both | Analyzer flags use of `os.system` with concatenated input; dynamic execution can exploit injection |
| 9 | Unbounded cache growth (memory leak) | Performance | Dynamic | Static can't easily determine unbounded growth; dynamic profiling reveals memory spike |
| 10 | Unreachable `result.clear()` after return | Correctness | Static | Dead code following return, static checkers catch unreachable statements |
| 11 | Inconsistent state if `to.deposit` throws (no rollback) | Correctness | Dynamic | Static analysis may not reason about exceptions; dynamic testing with thrown error reveals issue |
| 12 | Inefficient O(n^2) search loop | Performance | Dynamic | Complexity emerges only during execution on large data; static tools rarely flag big-O
| 13 | DOM insertion of unescaped `userInput` (XSS) | Security | Both | Static linters detect unsanitized DOM insertion; dynamic tests reveal XSS payload execution |
| 14 | Division by zero when `divisor` is 0 | Correctness | Dynamic | Runtime exception occurs; static analysis can't know value of divisor
| 15 | Returning pointer to local stack variable | Correctness | Both | Static analyzers warn about returning address of local; dynamic use leads to undefined behavior |

---

## Summary Questions

### How many snippets had Correctness issues? 10
### How many had Security issues? 4
### How many had Performance issues? 5

### Which issues are best caught by static analysis? Why?
Issues such as unreachable code, off‑by‑one loops, constant conditions, unused variables, and resource leaks are structural and don’t depend on runtime values. Static analyzers can inspect the source directly and detect these patterns without executing the program, making them ideal for correctness bugs that stem from coding mistakes and for certain security patterns like string concatenation in queries.

### Which issues require dynamic analysis? Why?
Problems that depend on runtime data or complex behavior—division by zero with unknown inputs, performance bottlenecks, unbounded memory growth, and failures due to exceptions—only surface when the code runs with representative inputs. Dynamic testing exercises these paths and reveals actual failures or inefficiencies that static tools cannot predict accurately.

