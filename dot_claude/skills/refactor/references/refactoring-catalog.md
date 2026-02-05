# Refactoring Catalog

Common refactoring patterns organized by category.

## Extract / Inline

### Extract Function
**When**: A block of code does a distinct subtask within a larger function.
```
Before:
    def process(data):
        # validate
        if not data: raise ValueError()
        if data.type not in VALID: raise ValueError()
        # transform
        result = transform(data)
        return result

After:
    def validate(data):
        if not data: raise ValueError()
        if data.type not in VALID: raise ValueError()

    def process(data):
        validate(data)
        return transform(data)
```

### Extract Variable
**When**: A complex expression is hard to read inline.
```
Before: if user.age >= 18 and user.country in ALLOWED and not user.banned:
After:
    is_eligible = user.age >= 18 and user.country in ALLOWED and not user.banned
    if is_eligible:
```

### Inline Function
**When**: A function's body is as clear as its name, and it's called once.
```
Before:
    def is_valid(x): return x > 0
    if is_valid(count): ...

After:
    if count > 0: ...
```

## Simplify Conditionals

### Replace Nested Conditionals with Guard Clauses
**When**: Deep nesting makes the happy path hard to find.
```
Before:
    def process(request):
        if request:
            if request.user:
                if request.user.is_active:
                    return do_work(request)
        return error()

After:
    def process(request):
        if not request: return error()
        if not request.user: return error()
        if not request.user.is_active: return error()
        return do_work(request)
```

### Consolidate Duplicate Conditional Fragments
**When**: The same code appears in every branch of a conditional.
```
Before:
    if condition:
        setup()
        do_a()
    else:
        setup()
        do_b()

After:
    setup()
    if condition:
        do_a()
    else:
        do_b()
```

### Replace Conditional with Lookup
**When**: A chain of if/elif maps inputs to outputs.
```
Before:
    if status == "active": color = "green"
    elif status == "pending": color = "yellow"
    elif status == "error": color = "red"

After:
    STATUS_COLORS = {"active": "green", "pending": "yellow", "error": "red"}
    color = STATUS_COLORS.get(status, "gray")
```

## Remove Dead Code

### Remove Unused Variables
**When**: A variable is assigned but never read. Verify with grep/search.

### Remove Unused Functions
**When**: A function exists but has no callers. Verify across the entire codebase.

### Remove Commented-Out Code
**When**: Code is commented out with no explanation. Version control preserves history.

### Remove Unreachable Branches
**When**: A conditional branch can never be true given the surrounding logic.

## Rename for Clarity

### Rename Variable
**When**: A name is too vague (`data`, `temp`, `x`) or misleading.
- Use nouns for values: `user_count`, `config_path`
- Use verbs for functions: `validate_input`, `fetch_users`
- Use `is_`/`has_`/`can_` for booleans: `is_valid`, `has_permission`

### Rename Function
**When**: The function name doesn't describe what it does.
- Name should describe the result, not the implementation
- `processData` → `validate_and_transform_order`

## Shell-Specific Refactoring

### Replace Subshell with Variable
```
Before: if [ "$(echo "$var" | grep pattern)" ]; then
After:  if [[ "$var" == *pattern* ]]; then
```

### Replace Command Chain with Function
**When**: The same sequence of commands appears in multiple places.
```
Before:
    cd "$dir" && make clean && make && make install
    # ... later ...
    cd "$other_dir" && make clean && make && make install

After:
    rebuild() { cd "$1" && make clean && make && make install; }
    rebuild "$dir"
    rebuild "$other_dir"
```

### Replace `grep | awk | sed` Pipeline with Single Tool
**When**: A multi-tool pipeline does what one tool can do alone.

## Python-Specific Refactoring

### Replace Loop with Comprehension
**When**: A simple loop builds a list/dict/set.
```
Before:
    result = []
    for item in items:
        if item.active:
            result.append(item.name)

After:
    result = [item.name for item in items if item.active]
```

### Replace os.path with pathlib
```
Before: path = os.path.join(base, "subdir", "file.txt")
After:  path = Path(base) / "subdir" / "file.txt"
```

### Use Context Manager
```
Before:
    f = open("file.txt")
    data = f.read()
    f.close()

After:
    with open("file.txt") as f:
        data = f.read()
```
