# Object Domain with Cache
## Some definitions
- **Concrete object (memory object)**: an object is allocated by given an allocator.
- **Abstract object (aka. DSA node)**: an object represents a number of concrete objects with the same pattern.
- **Singleton**: an abstract object that represents exactly one concrete object. That is, an abstract object is a concrete if and only if there is only one allocation.
- **Most Recently Used object (MRU object)**: a memory object that only records the most recently used by load/store instructions.

## Current object Domain state specification:
- The current abstract state (parameterized by a numerical domain DOM) includes:
  - `m_base_dom: (DOM)`: domain for registers, references and Booleans.
  - `m_odi_map: (obj_id -> DOM)`: a seperate domain where key is an id of an abstract object and value is a domain for object's fields.
  - `m_flds_id_map: (rgn var -> obj_id)`: a map from each region (fields) to corresponding Id
  - `m_obj_info_env: (obj_id -> count num)`: a map from each object id to the number of reference counting

## New state for cache (cache size = 1):
### Addresses
- Q: how do we represent addresses?
  - Goal: at least when the abstract transformer evaluates the load / store operations, we need to determine whether the given reference refers the MRU object or not.
  - Invariants:
    - For reference variables in CrabIR, any references referring the same region may refer the different memory (concrete) object.
    - DSA analysis determines wether two references belong in the same abstract object. In crab, this information is given by intrinsic calls on `regions_from_memory_object`.
  - In general, we cannot determine for every reference if it refers to the MRU object.
    - We only keep track equalities of local references and the base address o
- An address domain keeps equalities between the base addresses of memory objects, as well as the base address of each object's cache.
  - Formally, given two reference variables `ref1, ref2` and their corresponding region variables `rgn1, rgn2` through `ref2, rgn2 := ref_gep(ref1, rgn1, offset)`:
    - If `rgn1` and `rgn2` belong to the same abstract object, then the address domain will keep equality between `ref1_base` and `ref2_base` as:
     - $\{ ref1_{base} == ref2_{base} \}$, where `==` operator represents equal operator.
     - `ref1_base == ref2_base` in the address domain means the base address of `ref1` and `ref2` are the same.
     - In implementation, we will keep a map to track all references to one-to-one corresponding base address variables.
  - Special base address for the MRU object of an abstract object:
    - Using a special variable with name `<obj_id>_mru_base`.
    - E.g. if an abstract object has three fields `V1, V2, V3`. The object id is `V1`, then a variable to refer the cache object as `V1_mru_base`.
  - TODO: if we use the EUF domain, then we can determine the shape of references. For example, with a list struct we could have constraints of the following form:
    ```c=
    struct list { int v; struct list * l;};

    addrs = {
      ref = list(_, ref2)
      ref = list(_, list(_,ref3))
    }
    ```

### Abstract state including cache
- `m_odi_map` becomes `m_odi_map: (obj_id -> <DOM1, DOM2, EUF_DOM>)` 
  - where `DOM1` is the domain for the summary object; and `DOM2` is the domain for the most recently used object. For now `DOM1` and `DOM2` must be the same abstract domain.
  - The `EUF_DOM` keeps what what uninterpreted symbols assign to fields if those equal to some registers.
- `m_addrs_dom`: an EUF domain to keep equalities over base addresses of objects.
- `m_obj_info_env` becomes `m_obj_info_env: (obj_id -> <count num, Boolean, Boolean>)`
  - where the first value of the tuple is still a number of references referring the abstract object; the second value indicates whether the cache has been updated or not; and the third one represents whether the cache has been used or not.
- `m_regs_dom`: an EUF domain to keep what uninterpreted symbols assign to registers.
  - `m_regs_dom` combining with EUF domain in each object of the odi map represents equalities between fields of objects and registers.
  - The fresh symbol is created for a field in an object. We will keep a global mapping to track symbol of each field we already created.
  - The register is assigned by given a symbol created by some field. Thus, assigning for a new field will update it.
  - Overall, the fresh symbol guarantees each field has a distinct symbol to represent it.
- `m_flds_symb_map`: a map from a field to a uninterpreted symbol created for keeping equalities between fields and regsiters.
#### Sample abstract state:
```haskell=
{
  base = { reg1 = 4; reg2 <= reg1; ...; 
           ref1 > 0; ref2 > 0; ref3 > 0; ref4 > 0 }
  object (V1, V2) = (
    non-cache: { V1 <= V2; V2 <= 10 },
    cache: { V1 = 3; V2 = 5; }
    regs: { V2 = $symb1 } // V2 is referring cache object
    )
  addrs = { ref1_base = $addr1; ref2_base = $addr1; V1_mru_base == $addr1; 
            ref3_base = $addr2; ref4_base = $addr2 }
  regs = { reg3 = $symb1 }
  obj_info_env = {
    (V1, V2) -> <OneOrMore, True, True>
  }
  flds_symb_map = { V2 -> $symb1 }
  flds_id_map = { V1 -> V1, V2 -> V1 }
}
```

## Transformer
- Let S be an abstract state. The signature of our transformer is as follows:
  - $Expr \rightarrow S \rightarrow S$

### APIs

```
 JN: I assume that all parameters of gepref,load_ref, etc are passed by value
   
 All domains (included all sub-domains and the mru domain) implement this API:
    
      eval: Expr -> Interval const
      assign: Var x Expr  ('this' is modified)
      assign_interval: Var x Interval ('this' is modified)
      assume: Constraint  ('this' is modified)
      is_unsat: Constraint -> Bool const // or is_valid 
      ...
      is_bottom(): bool const
      is_top(): bool const
      make_bottom(): AbsDom // return a bottom domain value
      make_top(): AbsDom // return a top domain value
      leq:  AbsDom -> bool const // inclusion
      join: AbsDom -> AbsDom  const 
      meet: AbsDom -> AbsDom  const 
      widen: AbsDom -> AbsDom const 
      ...
      forget: list(Var) -> AbsDom const 
      project: list(Var) -> AbsDom const
      expand: list(Var) -> AbsDom const
    
The "const" attribute has the same C/C++ semantics. 
We could also make assign and assume "const" methods 
by returning AbsDom. I guess it depend's on what makes the presentation shorter.  

The types Var, Expr, and Interval are self-explanatory
Constraint is of the form "Expr RelOp 0" where RelOp = <=,<,=,!=,>,>=

Special APIs for uf domain:
      make_fresh_symbol(): term // make a fresh term symbol
      set(): Var x term // set a variable maps to a term
      get(): Var -> term * // get a term by a variable
```

### Make
```c=
[|ref := make_ref(rgn, size)|](S) ->
  match S with (m_base_dom, m_odi_map, 
                m_flds_id_map, m_obj_info_env, m_addrs_dom, m_regs_dom) ->
    obj_id = get_obj_id(rgn)
    old_obj_info = m_obj_info_env.find(obj_id)
    num_refs = old_obj_info.refcount_val()
    if (num_refs.is_one()) {
      move_singleton_to_odi_map(obj_id)
    }
    // create a fresh symbol to represent a base address
    m_addrs_dom.set(get_base_addr_by_reference(ref), 
                   m_addrs_dom.make_fresh_symbol())
    // update object info
    m_obj_info_env.set(obj_id, 
                       {num_refs.increment(), 
                        old_obj_info.cacheused_val(), 
                        old_obj_info.cachedirty_val()})
  return S
```

### Gep
```c=
[|ref2, rgn2 := ref_gep(ref1, rgn1, offset)|](S) ->
  match S with (m_base_dom, m_odi_map, 
                m_flds_id_map, m_obj_info_env, m_addrs_dom, m_regs_dom) ->
    obj_id1 = get_obj_id(rgn1)
    obj_id2 = get_obj_id(rgn2)
    if (obj_id1 == obj_id2) { // both regions refer the same object
      // assign equality: ref2 == ref1
      m_addrs_dom.assign(get_base_addr_by_reference(ref2), 
                         get_base_addr_by_reference(ref1))
    }
    m_base_dom.assign(ref2, ref1 + offset)
  return S
```

### Load
```c=
[|lhs := ref_load(rgn, ref)|](S) ->
  match S with (m_base_dom, m_odi_map, 
                m_flds_id_map, m_obj_info_env, m_addrs_dom, m_regs_dom) ->
  /*
   * Precondition:
   *  lhs must be a register / scalar variable in crab IR;
   *  rgn is a region variable and has been initialized;
   *  ref is a reference variable;
   *  m_obj_info_env must contain rgn 
   *    corresponding abstract object info.
   */
    obj_id = get_obj_id(rgn)
    // retrieve an abstract object info
    obj_info = m_obj_info_env.find(obj_id)
    num_refs = obj_info.refcount_val()
    assert(!num_refs.is_zero()) // number of reference could not be zero
    if (num_refs.is_one()) { // singleton object
      m_base_dom.assign(lhs, rgn)
    } else { // num_refs > 1, use ODI map
      let s = m_base_dom, m_odi_map, m_obj_info_env, m_addrs_dom, m_regs_dom
      s' = invalidate_cache_if_miss(s, obj_id, ref, rgn)
      m_base_dom, m_odi_map, m_obj_info_env, m_addrs_dom, m_regs_dom, symbol = s'
      if (lhs.is_reference()) { // lhs is a reference, assign a fresh symbol into address dom
        m_addrs_dom.set(get_base_addr_by_reference(lhs), 
                   m_addrs_dom.make_fresh_symbol())
      } else { // lhs is a register
        // use cache
        // assigning register with symbol
        m_regs_dom.set(lhs, symbol)
      }
      m_base_dom.forget(lhs)
    }
  let S' = 
  return S
```

### Store
```c=
[|ref_store(rgn, ref, val)|](S) ->
  match S with (m_base_dom, m_odi_map,
                m_flds_id_map, m_obj_info_env, m_addrs_dom, m_regs_dom) ->
  /*
   * Precondition:
   *  val could be a variable or constant;
   *  rgn is a region variable and has been initialized;
   *  ref is a reference variable;
   *  m_obj_info_env must contain rgn 
   *    corresponding abstract object info.
   */
    obj_id = get_obj_id(rgn)
    // retrieve an abstract object info
    obj_info = m_obj_info_env.find(obj_id)
    num_refs = obj_info.refcount_val()
    assert(!num_refs.is_zero()) // number of reference could not be zero
    if (num_refs.is_one()) { // singleton object
      m_base_dom.assign(rgn, val)
    } else { // num_refs > 1, use ODI map
      let s = m_base_dom, m_odi_map, m_obj_info_env, m_addrs_dom, m_regs_dom
      s' = invalidate_cache_if_miss(s, obj_id, ref, rgn)
      m_base_dom, m_odi_map, m_obj_info_env, m_addrs_dom, m_regs_dom, symbol_ptr = s'
      obj_non_cache_dom, obj_cache_dom, obj_cache_reg_dom = m_odi_map.find(obj_id)
      if (val.is_constant()) {
        obj_cache_dom.assign(rgn, val)
      }
      else { // val is a variable (i.e. register)
        // use cache
        // assigning register with symbol
        m_regs_dom.set(val, symbol)
      }
      // update object info
      obj_info = obj_info_env.find(obj_id)
      obj_info_env.set(obj_id,
                    obj_info.refcount_val(),
                    true /* cache is dirty*/,
                    true /* cache is used */)
      // update odi map
      odi_map.set(obj_id, {obj_non_cache_dom, obj_cache_dom, obj_cache_reg_dom})
    }

  return S
```

### Helper functions

#### `invalidate_cache_if_miss`: check whether cache is missed. If missed, then commit cache if it is dirty and update cache for new MRU object.
```c=
invalidate_cache_if_miss(s, obj_id, ref, rgn) ->
  match s with (base_dom, odi_map, obj_info_env, addrs_dom, regs_dom) ->
    mru_obj_ref = get_cache_obj_reference(obj_id)
    obj_info = obj_info_env.find(obj_id)
    cache_used = obj_info.cacheused_val()
    obj_non_cache_dom, obj_cache_dom, obj_cache_reg_dom = odi_map.find(obj_id)
    if (cache_used.is_false()
         or addrs_dom.is_unsat(mru_obj_ref == ref)) {
      /* Cache missed condition:
         cache is empty or current ref does not refer to the mru object */

      // Step1: commit cache if the cache is dirty
      let odi = base_dom, obj_non_cache_dom, obj_cache_dom, 
                obj_cache_reg_dom, regs_dom, obj_info, obj_id
      odi' = commit_cache_if_dirty(odi)

      let base_dom, obj_non_cache_dom, obj_cache_dom, 
          obj_cache_reg_dom, regs_dom, obj_info = odi'
      // Step2: update cache for new MRU object
      obj_cache_dom = update_cache(obj_non_cache_dom)
      // Step3: update address dom and object info
      addrs_dom.assign(mru_obj_ref, ref) // refer mru object to a new one
      obj_info_env.set(obj_id,
                       obj_info.refcount_val(),
                       false /* cache is not dirty*/,
                       true /* cache is used */)
    }
    // Step4: update cache_reg if a field equals to some register
    symbol_ptr = m_flds_symb_map.find(rgn)
    if (symbol_ptr) { // symbol has been created
      symbol = *symbol_ptr
    } else { // create a fresh symbol for rgn
      symbol = obj_cache_reg_dom.make_fresh_symbol()
      m_flds_symb_map.set(rgn, symbol)
    }
    // Step5: update odi map
    odi_map.set(obj_id, {obj_non_cache_dom, 
                         obj_cache_dom, obj_cache_reg_dom})
  let ret = base_dom, odi_map, obj_info_env, addrs_dom, regs_dom, symbol
  return ret
```

#### `reduce_regs_flds_between_cache_and_base`: transfer regs-flds' constriants between MRU and base 
```c=
reduce_regs_flds_between_cache_and_base(base_dom, cache_dom, cache_reg_dom, regs_dom, obj_flds_vec) ->
  flds_regs = regs_dom.meet(cache_reg_dom)
  lin_cons = flds_regs.to_linear_constraint_system()
  cache_base = base_dom.meet(cache_dom)
  cache_base.add_linear_constraint_system(lin_cons)
  base_dom'' = cache_base.forget(obj_flds_vec)
  cache_dom'' = cache_base.project(obj_flds_vec)
  return base_dom'', cache_dom''
```

#### `commit_cache`: commit cache contents into non_cache.
```c=
commit_cache(non_cache, cache) ->
  // join non_cache with cache
  non_cache' = non_cache.join(cache)
  return non_cache'
```

#### `commit_cache_if_dirty`: commit cache contents into non_cache, reset cache.
```c=
commit_cache_if_dirty(obj) ->
  match obj with (base_dom, non_cache_dom, cache_dom, cache_reg_dom, regs_dom, obj_info, obj_id) ->
  cache_dirty = obj_info.cachedirty_val()
  // Step1: commit cache if the cache is dirty
  if (cache_dirty.is_true()) {
   obj_flds_vec = get_obj_fields(obj_id)
   base_dom, cache_dom = reduce_regs_flds_between_cache_and_base(base_dom, 
                                             obj_cache_dom,
                                             obj_cache_reg_dom,
                                             regs_dom, obj_flds_vec)
   non_cache_dom = commit_cache(non_cache_dom, cache_dom)
  }
  cache_dom = cache_dom.make_top()
  obj_cache_reg_dom = obj_cache_reg_dom.make_top()
  let obj' = base_dom, non_cache_dom, cache_dom, cache_reg_dom, regs_dom, obj_info
  return obj'
```

#### `update_cache`: update cache contents from non_cache to cache
```c=
update_cache(non_cache) ->
  new_cache = copy(non_cache) // copy non_cache to cache
  return new_cache
```

#### `get_cache_obj_reference`: get variable representing the base address of the mru object
```c=
get_cache_obj_reference(obj_id) ->
  return the variable named `<obj_id>_mru_base`
```

#### `get_base_addr_by_reference`: get variable representing the base address by given a reference
```c=
get_base_addr_by_reference(ref) ->
  return the variable named `<ref>_base`
```

#### `check_mru_objects_refer_same`: check whether two mru objects refer the same
```c=
check_mru_objects_refer_same(obj_id, l_addrs_dom, r_addrs_dom) ->
  // We choose to use UF domain for address domain
  // The APIs for UF domain:
  // get_term: Var -> const term *
  l_term_ptr = l_addrs_dom.get_term(obj_id)
  r_term_ptr = r_addrs_dom.get_term(obj_id)
  if (l_term_ptr and r_term_ptr and (*l_term_ptr).equals_to(*r_term_ptr)) {
    return true
  }
  return false
```

#### `reduce_cache_2`: commit cache if two caches are required.
```c=
reduce_cache_2(l_odi, r_odi, obj_id) ->
  match l_odi with (l_base_dom, l_non_cache_dom, l_cache_dom, 
                    l_cache_reg_dom, l_regs_dom, l_obj_info) ->
  match r_odi with (r_base_dom, r_non_cache_dom, r_cache_dom, 
                    r_cache_reg_dom, r_regs_dom, r_obj_info) ->
  l_cache_used = l_obj_info.cacheused_val()
  r_cache_used = r_obj_info.cacheused_val()
  if (l_cache_used.is_true() and r_cache_used.is_true()) {
    if (not check_mru_objects_refer_same(obj_id, l_addrs_dom, r_addrs_dom)) {
      // commit cache for both states if dirty
      l_odi' = commit_cache_if_dirty(l_odi)
      r_odi' = commit_cache_if_dirty(r_odi)
      return l_odi', r_odi'
    }
  } else if (l_cache_used.is_true()) {
    // commit if dirty
    l_odi' = commit_cache_if_dirty(l_odi)
    return l_odi', r_odi
  } else if (r_cache_used.is_true()) {
    r_odi' = commit_cache_if_dirty(r_odi)
    return l_odi, r_odi'
  }
  return l_odi, r_odi
```


### Join operation
- Given two abstract states $S_1$ and $S_2$:
```c=
join(S1, S2) ->
  match S1, S2 with 
  ((l_base_dom, l_odi_map, l_flds_id_map, l_obj_info_env, l_addrs_dom, l_regs_dom),
   (r_base_dom, r_odi_map, r_flds_id_map, r_obj_info_env, r_addrs_dom, r_regs_dom)) ->
  /*
   * Each abstract object might be singleton or non-singleton
   * The singleton stores in base dom, others are in odi map.
   * There are two cases we need to consider for new design.
   * One case is one state has a singleton object, 
   * and another state contains non-singleton.
   * In this case, for non-singleton object, 
   * we need to commit the cache if the cache is used
   * before joining singleton with non-singleton, 
   * because our model only knows there is a memory object 
   * but not the base address of the singleton object.
   * 
   * In addition, now odi map contains non-cache and cache domain.
   * The odi map is still only for non-singleton objects. 
   * When both states contain a non-singleton object,
   * (1) for non-cache domain, we just perform the join.
   * (2) for cache domain, we can join them
   * iff both caches refer the same memory object;
   * otherwise, we have to commit cache before the join.
   * To determine whether both caches refer the same obj, we can only
   * know if those caches are loaded from the common state.
   */
  
  // 1. join the parts of each state that do not require above consideration
  o_obj_info_env = l_obj_info_env.join(r_obj_info_env)
  o_addrs_dom = l_addrs_dom.join(r_addrs_dom)
  
  // flds_id_map does not require join, it shares accross all states
  
  // 2. join the odi map
  let o_odi_map be the output odi map
  for (<obj_id, l_obj_info> : l_obj_info_env) {
    if (r_obj_info = r_obj_info_env.find(obj_id)) {
      l_num_refs = l_obj_info.refcount_val()
      r_num_refs = r_obj_info.refcount_val()
      l_non_cache_dom, l_cache_dom, l_cache_reg_dom = l_odi_map.find(obj_id)
      r_non_cache_dom, r_cache_dom, r_cache_reg_dom = r_odi_map.find(obj_id)
      // Step 1. Join the odi map if the object is non-singleton in both states
      if (l_num_refs.is_one_or_more() and r_num_refs.is_one_or_more()) {
        let l_odi = l_base_dom, l_non_cache_dom, l_cache_dom, 
                    l_cache_reg_dom, l_regs_dom, l_obj_info
        let r_odi = r_base_dom, r_non_cache_dom, r_cache_dom, 
                    r_cache_reg_dom, r_regs_dom, r_obj_info
        l_odi', r_odi' = reduce_cache_2(l_odi, r_odi, obj_id)
        let l_base_dom, l_non_cache_dom, l_cache_dom, 
              l_cache_reg_dom, l_regs_dom, l_obj_info = l_odi'
        let r_base_dom, r_non_cache_dom, r_cache_dom, 
            r_cache_reg_dom, r_regs_dom, r_obj_info = r_odi'
        o_cache_dom = l_cache_dom.join(r_cache_dom)
        o_non_cache_dom = l_non_cache_dom.join(r_non_cache_dom)
        o_cache_reg_dom = l_cache_reg_dom.join(r_cache_reg_dom)
        o_odi_map.set(obj_id, {o_non_cache_dom, o_cache_dom, o_cache_reg_dom})
      }
      // Step 2. Handle the case for joining singleton with non-singleton
      else if (l_num_refs.is_one() and r_num_refs.is_one_or_more()) {
        r_cache_used = r_obj_info.cacheused_val()
        r_cache_dirty = r_obj_info.cachedirty_val()
        if (r_cache_used.is_true()) { // commit cache if dirty
          let r_odi = r_base_dom, r_non_cache_dom, r_cache_dom, 
                r_cache_reg_dom, r_regs_dom, r_obj_info, obj_id
          r_odi' = commit_cache_if_dirty(r_odi)
          let r_base_dom, r_non_cache_dom, r_cache_dom, 
              r_cache_reg_dom, r_regs_dom, r_obj_info, _ = r_odi'
        }
        r_cache_dom = r_cache_dom.make_top()
        // join singleton with non_singleton
        // The same procedure as current implementation
      }
      else if (l_num_refs.is_one_or_more() and r_num_refs.is_one()) {
        l_cache_used = l_obj_info.cacheused_val()
        l_cache_dirty = l_obj_info.cachedirty_val()
        if (l_cache_used.is_true()) { // commit cache if dirty
          let l_odi = l_base_dom, l_non_cache_dom, l_cache_dom, 
                l_cache_reg_dom, l_regs_dom, l_obj_info, obj_id
          l_odi' = commit_cache_if_dirty(l_odi)
          let l_base_dom, l_non_cache_dom, l_cache_dom, 
              l_cache_reg_dom, l_regs_dom, l_obj_info, _ = l_odi'
        }
        l_cache_dom = l_cache_dom.make_top()
        // join singleton with non_singleton
        // The same procedure as current implementation
      }
    }
  }
  
  // 3. join the base domain
  o_base_dom = l_base_dom.join(r_base_dom)
    
  // 4 join the regs domain
  o_regs_dom = l_regs_dom.join(r_regs_dom)
  
  let output state So = (o_base_dom, o_odi_map, l_flds_id_map, 
                         o_obj_info_env, o_addrs_dom, o_regs_dom)
  return So
```


### Meet operation
- Given two abstract states $S_1$ and $S_2$:
```c=
meet(S1, S2) ->
  match S1, S2 with 
  ((l_base_dom, l_odi_map, l_flds_id_map, l_obj_info_env, l_addrs_dom, l_regs_dom),
   (r_base_dom, r_odi_map, r_flds_id_map, r_obj_info_env, r_addrs_dom, r_regs_dom)) ->
  
  // Meet is similiar to join, each time we have to consider cases for commiting cache

  // 1. meet the parts of each state that do not require above consideration
  o_obj_info_env = l_obj_info_env.join(r_obj_info_env)
  o_addrs_dom = l_addrs_dom.join(r_addrs_dom)
  
  // flds_id_map does not require meet, it shares accross all states
  
  // 2. meet the odi map
  let o_odi_map be the output odi map
  for (<obj_id, l_obj_info> : l_obj_info_env) {
    if (r_obj_info = r_obj_info_env.find(obj_id)) {
      l_num_refs = l_obj_info.refcount_val()
      r_num_refs = r_obj_info.refcount_val()
      l_non_cache_dom, l_cache_dom, l_cache_reg_dom = l_odi_map.find(obj_id)
      r_non_cache_dom, r_cache_dom, r_cache_reg_dom = r_odi_map.find(obj_id)
      // Step 1. Meet the odi map if the object is non-singleton in both states
      // Require committing cache before meet
      if (l_num_refs.is_one_or_more() and r_num_refs.is_one_or_more()) {
        let l_odi = l_base_dom, l_non_cache_dom, l_cache_dom, 
                    l_cache_reg_dom, l_regs_dom, l_obj_info
        let r_odi = r_base_dom, r_non_cache_dom, r_cache_dom, 
                    r_cache_reg_dom, r_regs_dom, r_obj_info
        l_odi', r_odi' = reduce_cache_2(l_odi, r_odi, obj_id)
        let l_base_dom, l_non_cache_dom, l_cache_dom, 
              l_cache_reg_dom, l_regs_dom, l_obj_info = l_odi'
        let r_base_dom, r_non_cache_dom, r_cache_dom, 
            r_cache_reg_dom, r_regs_dom, r_obj_info = r_odi'
        o_cache_dom = l_cache_dom.meet(r_cache_dom)
        o_non_cache_dom = l_non_cache_dom.meet(r_non_cache_dom)
        o_cache_reg_dom = l_cache_reg_dom.meet(r_cache_reg_dom)
        o_odi_map.set(obj_id, {o_non_cache_dom, o_cache_dom, o_cache_reg_dom})
      }
      // Step 2. Handle the case for meet non-singleton with singleton
    }
  }
```

## Examples

### One: two DSA nodes, each of them has two objects in the program
#### Notations

- Regions:
  - objA,
    - `V_a` is a region variable for field `a`
    - `V_b` is a region variable for field `b`
    - `V_c` is a region variable for field `c`
  - objB,
    - `V_d` is a region variable for field `d`
    - `V_e` is a region variable for field `e`
- Size of int: assume the size of int is **4** bytes

#### Program 1 in C
```c=
struct objA a1 = {.a = 0; .b = 1; .c = 1};
struct objA a2 = {.a = 1; .b = 2; .c = 2};
struct objB b1 = {.d = 4; .e = 5};
struct objB b2  = {.d = 5; .e = 6};

assume(&a1 == 100);
assume(&a2 == 112);
assume(&b1 == 200);
assume(&b2 == 208);

struct objA *ra = &a1;
struct objB *rb = &b1;
int *p = ra->b;
/** abstract pre-state is here */
int x = *p;
int *r = rb->d;
int *s = rb->e;
x = *r;
y = *s;
assert(x < y);
/** abstract post-state is here */
```

##### Instructions in Crab IR
Assume we use some relational domain like Octagon:
```c=
/* Invariants in abstract pre-state
  base: { p = ra + 4 && ra = 100 && rb = 200 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: top,
    regs: top,
    cache_used: false,
    cache_dirty: false,
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: top,
    regs: top,
    cache_used: false,
    cache_dirty: false
  )},
  addrs: { ra == p }
  regs: top
*/
  x = load_ref(V_b, p)
/*
  base: { p = ra + 4 && ra = 100 && rb = 200 && 1 <= x && x <= 2 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    regs: { V_b = $symb1 },
    cache_used: true,
    cache_dirty: false
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: top,
    regs: top,
    cache_used: false,
    cache_dirty: false
  )},
  addrs: { V_a_ref == p && ra == p }
  regs: { x = $symb1 }
*/
  V_d, r = gep_ref(V_d, rb, 0)
/*
  base: { p = ra + 4 && ra = 100 && rb = 200 && 1 <= x && x <= 2 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    regs: { V_b = $symb1 },
    cache_used: true,
    cache_dirty: false
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: top,
    regs: top,
    cache_used: false,
    cache_dirty: false
  )},
  addrs: { V_a_ref == p && ra == p && r == rb }
  regs: { x = $symb1 }
*/
  V_e, s = gep_ref(V_d, rb, 4)
/*
  base: { p = ra + 4 && r = rb && s = rb + 4 && ra = 100 && rb = 200 && 1 <= x && x <= 2 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    regs: { V_b = $symb1 },
    cache_used: true,
    cache_dirty: false
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: top,
    regs: top,
    cache_used: false,
    cache_dirty: false
  )},
  addrs: { V_a_ref == p && ra == p && r == rb && s == rb }
  regs: { x = $symb1 }
*/
  x = load_ref(V_d, r)
/*
  base: { p = ra + 4 && r = rb && s = rb + 4 && ra = 100 && rb = 200 && 4 <= x && x <= 5 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    regs: { V_b = $symb1 },
    cache_used: true,
    cache_dirty: false
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    regs: { V_d = $symb2 },
    cache_used: true,
    cache_dirty: false
  )},
  addrs: { V_a_ref == p && ra == p && r == rb && s == rb && V_b_ref == r }
  regs: { x = $symb2 }
*/
  y = load_ref(V_e, s)
/*
  base: { p = ra + 4 && r = rb && s = rb + 4 && ra = 100 && rb = 200 
          && 4 <= x && x <= 5 && 5 <= y && y <= 6 },
  objA: (
    non_cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    cache: { 0 <= V_a && V_a < V_b && V_b = V_c && V_c <= 2 },
    regs: { V_b = $symb1 },
    cache_used: true,
    cache_dirty: false
  ),
  objB: (
    non_cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    cache: { 4 <= V_d && V_d < V_e && V_e <= 6 },
    regs: { V_d = $symb2 && V_e = $symb3 },
    cache_used: true,
    cache_dirty: false
  )},
  addrs: { V_a_ref == p && ra == p && r == rb && s == rb && V_b_ref == r }
  regs: { x = $symb2 && y = $symb3 }
*/
  assert(x < y)
```
Thus, based on the invariants, above assertion we cannot prove.
