#include "fastplyr.h"

static SEXP group_unaware_fns = NULL;
static SEXP group_unaware_fn_names = NULL;

[[cpp11::init]]
void init_group_unaware_fns(DllInfo* dll){

  group_unaware_fns = Rf_allocVector(VECSXP, 21);
  R_PreserveObject(group_unaware_fns);

  group_unaware_fn_names = Rf_allocVector(STRSXP, 21);
  R_PreserveObject(group_unaware_fn_names);

  for (int i = 0; i < 21; ++i){
    SET_VECTOR_ELT(group_unaware_fns, i, Rf_mkChar("base"));
  }

  SET_STRING_ELT(group_unaware_fn_names, 0, Rf_mkChar("|"));
  SET_STRING_ELT(group_unaware_fn_names, 1, Rf_mkChar("&"));
  SET_STRING_ELT(group_unaware_fn_names, 2, Rf_mkChar(">="));
  SET_STRING_ELT(group_unaware_fn_names, 3, Rf_mkChar(">"));
  SET_STRING_ELT(group_unaware_fn_names, 4, Rf_mkChar("<="));
  SET_STRING_ELT(group_unaware_fn_names, 5, Rf_mkChar("<"));
  SET_STRING_ELT(group_unaware_fn_names, 6, Rf_mkChar("=="));
  SET_STRING_ELT(group_unaware_fn_names, 7, Rf_mkChar("+"));
  SET_STRING_ELT(group_unaware_fn_names, 8, Rf_mkChar("-"));
  SET_STRING_ELT(group_unaware_fn_names, 9, Rf_mkChar("*"));
  SET_STRING_ELT(group_unaware_fn_names, 10, Rf_mkChar("/"));
  SET_STRING_ELT(group_unaware_fn_names, 11, Rf_mkChar("abs"));
  SET_STRING_ELT(group_unaware_fn_names, 12, Rf_mkChar("sign"));
  SET_STRING_ELT(group_unaware_fn_names, 13, Rf_mkChar("floor"));
  SET_STRING_ELT(group_unaware_fn_names, 14, Rf_mkChar("trunc"));
  SET_STRING_ELT(group_unaware_fn_names, 15, Rf_mkChar("round"));
  SET_STRING_ELT(group_unaware_fn_names, 16, Rf_mkChar("signif"));
  SET_STRING_ELT(group_unaware_fn_names, 17, Rf_mkChar("exp"));
  SET_STRING_ELT(group_unaware_fn_names, 18, Rf_mkChar("log"));
  SET_STRING_ELT(group_unaware_fn_names, 19, Rf_mkChar("("));
  SET_STRING_ELT(group_unaware_fn_names, 20, Rf_mkChar("{"));

  set_names(group_unaware_fns, group_unaware_fn_names);
}

// Only checks the current call and not all nested calls
bool maybe_is_group_unaware_call(SEXP expr, SEXP env){
  bool maybe = is_fn_call(expr, group_unaware_fn_names, R_NilValue, env);

  if (!maybe) return false;

  // Get fn name as a symbol
  SEXP fn = R_NilValue;
  if (call_is_namespaced(expr)){
    fn = CAR(CDDR(CAR(expr)));
  } else {
    fn = CAR(expr);
  }

  SEXP fn_str = SHIELD(rlang::sym_as_string(fn));
  SEXP actual_ns = SHIELD(get_fun_ns(fn, env)); // CHARSXP namespace
  SEXP target_ns = get_list_element(group_unaware_fns, CHAR(fn_str));

  if (Rf_isNull(target_ns)){
    YIELD(2);
    return false;
  }

  bool out = target_ns == actual_ns;
  YIELD(2);
  return out;
}

[[cpp11::register]]
bool is_group_unaware_call(SEXP expr, SEXP env){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int32_t NP = 0;

  if (!maybe_is_group_unaware_call(expr, env)){
    return false;
  }

  bool out = true;

  SEXP tree = SHIELD(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (!is_group_unaware_call(branch, env)){
        out = false;
        break;
      }
    }
  }
  YIELD(NP);
  return out;
}

// Helpers for working with R expressions

// Basically R's get()

SEXP get(SEXP sym, SEXP rho){

  int32_t NP = 0;
  if (TYPEOF(sym) != SYMSXP){
    SHIELD(sym = Rf_coerceVector(sym, SYMSXP)); ++NP;
  }

  if (TYPEOF(rho) != ENVSXP){
    Rf_error("second argument to '%s' must be an environment", __func__);
  }

  SEXP val = Rf_findVar(sym, rho);
  if (val == R_MissingArg){
    YIELD(NP);
    Rf_error("arg `sym` cannot be missing");
  } else if (val == R_UnboundValue){
    YIELD(NP);
    return R_NilValue;
  } else if (TYPEOF(val) == PROMSXP){
    SHIELD(val);
    val = Rf_eval(val, rho);
    YIELD(1);
  }
  YIELD(NP);
  return val;
}

bool exists(SEXP sym, SEXP rho){
  return !Rf_isNull(get(sym, rho));
}

// Convert call to list of symbols
SEXP as_list_call(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP) {
    Rf_error("`expr` must be a language object");
  }
  int n = Rf_length(expr);
  SEXP result = SHIELD(new_vec(VECSXP, n));
  SEXP current = expr;
  for (int i = 0; i < n; i++) {
    SET_VECTOR_ELT(result, i, CAR(current));
    current = CDR(current);
  }
  YIELD(1);
  return result;
}

[[cpp11::register]]
bool is_nested_call(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP) {
    return false;
  }
  SEXP current = CDR(expr);
  while (TYPEOF(current) != NILSXP) {
    SEXP arg = CAR(current);
    if (TYPEOF(arg) == LANGSXP) {
      return true;
    }
    current = CDR(current);
  }
  return false;
}

[[cpp11::register]]
bool call_is_namespaced(SEXP expr){

  int32_t NP = 0;

  if (TYPEOF(expr) != LANGSXP){
    Rf_error("`expr` must be a `call` in %s", __func__);
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  SEXP call_list = SHIELD(as_list_call(expr_call)); ++NP;

  if (Rf_length(call_list) != 3){
    YIELD(NP);
    return false;
  }

  SEXP first = SHIELD(VECTOR_ELT(call_list, 0)); ++NP;
  SEXP second = SHIELD(VECTOR_ELT(call_list, 1)); ++NP;

  if (TYPEOF(first) != SYMSXP ||
      (first != R_DoubleColonSymbol &&
      first != R_TripleColonSymbol)){
    YIELD(NP);
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    YIELD(NP);
    return false;
  }
  YIELD(NP);
  return true;
}

// Basic version of rlang::is_call(expr, ns = ns)
bool is_ns_call(SEXP expr, SEXP ns){

  int32_t NP = 0;

  if (TYPEOF(ns) != STRSXP){
    Rf_error("`ns` must be a character vector in %s", __func__);
  }

  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  SEXP ns_str = SHIELD(Rf_asChar(ns)); ++NP;
  SEXP call_list = SHIELD(as_list_call(expr_call)); ++NP;

  if (Rf_length(call_list) != 3){
    YIELD(NP);
    return false;
  }

  SEXP first = SHIELD(VECTOR_ELT(call_list, 0)); ++NP;
  SEXP second = SHIELD(VECTOR_ELT(call_list, 1)); ++NP;

  if (TYPEOF(first) != SYMSXP ||
      (first != R_DoubleColonSymbol &&
      first != R_TripleColonSymbol)){
    YIELD(NP);
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    YIELD(NP);
    return false;
  }
  bool out = rlang::sym_as_string(second) == ns_str;
  YIELD(NP);
  return out;
}

// get the namespace of a function

SEXP get_fun_ns(SEXP x, SEXP rho){
  int32_t NP = 0;
  if (!Rf_isFunction(x)){
    SHIELD(x = get(x, rho)); ++NP;
  }
  if (!Rf_isFunction(x)){
    YIELD(NP);
    Rf_error("Internal error, function cannot be found");
  }
  if (TYPEOF(x) != CLOSXP){
    // This is e.g. a primitive function
    YIELD(NP); return Rf_mkChar("base");
  }
  SEXP env_call = SHIELD(Rf_lang2(Rf_install("environment"), x)); ++NP;
  SEXP env = SHIELD(Rf_eval(env_call, rho)); ++NP;
  if (Rf_isNull(x) || Rf_isNull(env)){
    YIELD(NP); return R_BlankString;
  } else if (env == R_BaseNamespace){
    YIELD(NP); return Rf_mkChar("base");
  } else if (R_IsNamespaceEnv(env)) {
    SEXP ns_name = SHIELD(R_NamespaceEnvSpec(env)); ++NP;
    SEXP names = SHIELD(get_names(ns_name)); ++NP;
    SEXP name = SHIELD(Rf_mkString("name")); ++NP;
    SEXP name_loc = SHIELD(Rf_match(names, name, NA_INTEGER)); ++NP;

    if (TYPEOF(ns_name) == STRSXP &&
        Rf_length(name) != 0 &&
        INTEGER(name_loc)[0] != NA_INTEGER){
      SEXP result = SHIELD(STRING_ELT(ns_name, INTEGER(name_loc)[0] - 1)); ++NP;
      YIELD(NP);
      return result;
    }
    YIELD(NP); return R_BlankString;
  } else {
    YIELD(NP); return R_BlankString;
  }
}

[[cpp11::register]]
SEXP fun_ns(SEXP x, SEXP rho){
  return Rf_ScalarString(get_fun_ns(x, rho));
}

// is this call a call to any function supplied to `fn`?

bool is_call2(SEXP expr, SEXP fn){

  if (TYPEOF(fn) != STRSXP){
    Rf_error("`fn` must be a character vector %s", __func__);
  }

  int32_t NP = 0;

  SEXP fn_sym;
  PROTECT_INDEX fn_sym_idx;
  R_ProtectWithIndex(fn_sym = R_NilValue, &fn_sym_idx); ++NP;

  for (int i = 0; i < Rf_length(fn); ++i){
    R_Reprotect(fn_sym = Rf_installChar(STRING_ELT(fn, i)), fn_sym_idx);

    if (TYPEOF(expr) == LANGSXP && call_is_namespaced(expr)){
      SEXP call_tree = SHIELD(as_list_call(expr)); ++NP;
      SEXP fn_expr_tree = SHIELD(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
      if (TYPEOF(VECTOR_ELT(fn_expr_tree, 2)) == SYMSXP &&
          VECTOR_ELT(fn_expr_tree, 2) == fn_sym){
        YIELD(NP);
        return true;
      }
    } else if (TYPEOF(expr) == LANGSXP && TYPEOF(CAR(expr)) == SYMSXP){
      if (CAR(expr) == fn_sym){
        YIELD(NP);
        return true;
      }
    }
  }
  YIELD(NP);
  return false;
}

// A function similar to rlang::is_call(expr, fn)
// but it is more aggressive in finding the specified function
// The call need not be namespaced to check that the call contains a function
// in a specified namespace
// for example, is_fn_call(quote(mean()), "mean", "base", globalenv())
// returns true whereas
// rlang::is_call(quote(mean()), "mean", ns = "base") returns false

[[cpp11::register]]
bool is_fn_call(SEXP expr, SEXP fn, SEXP ns, SEXP rho){
  if (TYPEOF(fn) != STRSXP){
    Rf_error("`fn` must be a character vector in %s", __func__);
  }

  if (!Rf_isNull(ns) && (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1)){
    Rf_error("`ns` must be `NULL` or a character vector of length one in %s", __func__);
  }

  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  int32_t NP = 0;
  int n_fns = Rf_length(fn);

  if (Rf_isNull(ns)){
    return is_call2(expr, fn);
  } else {
    SEXP ns_char = STRING_ELT(ns, 0);
    SEXP fn_ns, fn_sym;
    PROTECT_INDEX fn_ns_idx, fn_sym_idx;
    R_ProtectWithIndex(fn_ns = R_NilValue, &fn_ns_idx); ++NP;
    R_ProtectWithIndex(fn_sym = R_NilValue, &fn_sym_idx); ++NP;
    bool out = is_call2(expr, fn);
    if (!out){
      YIELD(NP);
      return out;
    }
    out = false; // Reset
    if (call_is_namespaced(expr)){
      SEXP call_tree = SHIELD(as_list_call(expr)); ++NP;
      SEXP fn_expr_tree = SHIELD(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
      R_Reprotect(fn_ns = rlang::sym_as_string(VECTOR_ELT(fn_expr_tree, 1)), fn_ns_idx);
      out = fn_ns == ns_char;
    } else {
      for (int i = 0; i < n_fns; ++i){
        R_Reprotect(fn_sym = Rf_installChar(STRING_ELT(fn, i)), fn_sym_idx);
        R_Reprotect(fn_ns = get_fun_ns(fn_sym, rho), fn_ns_idx);
        out = out || (fn_ns == ns_char);
      }
    }
    YIELD(NP);
    return out;
  }
}

// checks if call is or contains any calls to a namespace
// it doesn't require the function to actually be called via `::`
bool call_contains_ns(SEXP expr, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int32_t NP = 0;
  if (is_ns_call(expr, ns)){
    return true;
  }
  bool out = false;

  if (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1){
    Rf_error("`ns` must be a length 1 character vector in %s", __func__);
  }

  SEXP ns_str = SHIELD(STRING_ELT(ns, 0)); ++NP;
  SEXP tree = SHIELD(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (call_contains_ns(branch, ns, rho)){
        out = true;
        break;
      }
    }
    if (TYPEOF(branch) == SYMSXP){
      SEXP branch_name = SHIELD(rlang::sym_as_character(branch)); ++NP;
      SEXP fun_ns = SHIELD(get_fun_ns(branch_name, rho)); ++NP;
      if (fun_ns == ns_str){
        out = true;
        break;
      }
    }
  }
  YIELD(NP);
  return out;
}

bool call_contains_fn(SEXP expr, SEXP fn, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int32_t NP = 0;
  if (is_fn_call(expr, fn, ns, rho)){
    return true;
  }
  bool out = false;

  if (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1){
    Rf_error("`fn` must be a length 1 character vector in %s", __func__);
  }
  if (TYPEOF(ns) != NILSXP && (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1)){
    Rf_error("`ns` must be `NULL` or a length 1 character vector in %s", __func__);
  }
  // SEXP fn_str = SHIELD(STRING_ELT(fn, 0)); ++NP;
  // SEXP fn_sym = SHIELD(Rf_installChar(fn_str)); ++NP;
  // SEXP ns_str;
  // if (TYPEOF(ns) == NILSXP){
  //   ns_str = SHIELD(new_vec(STRSXP, 1)); ++NP;
  // } else {
  //   ns_str = SHIELD(STRING_ELT(ns, 0)); ++NP;
  // }
  SEXP tree = SHIELD(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (call_contains_fn(branch, fn, ns, rho)){
        out = true;
        break;
      }
    }
    if (is_fn_call(branch, fn, ns, rho)){
      out = true;
      break;
    }
  }
  YIELD(NP);
  return out;
}
