#include "fastplyr.h"

// Helpers for working with R expressions

bool functions_equal(SEXP x, SEXP y){
  if (!Rf_isFunction(x) || !Rf_isFunction(y)){
    Rf_error("`x` and `y` must be functions");
  }

  return R_compute_identical(x, y, 8);
}

// Helper to get exported package function
SEXP find_pkg_fun(const char *name, const char *pkg, bool all_fns){
  if (all_fns){
    return Rf_eval(Rf_lang3(R_TripleColonSymbol, Rf_install(pkg), Rf_install(name)), R_BaseEnv);
  } else {
    return Rf_eval(Rf_lang3(R_DoubleColonSymbol, Rf_install(pkg), Rf_install(name)), R_BaseEnv);
  }
}

// Match fn to a list of fns
[[cpp11::register]]
int match_fun(SEXP x, SEXP fns){

  if (TYPEOF(fns) != VECSXP){
    Rf_error("`fns` must be a list of functions in %s", __func__);
  }

  int out = NA_INTEGER;
  int n = Rf_length(fns);

  for (int i = 0; i < n; ++i){
    if (functions_equal(x, VECTOR_ELT(fns, i))){
      out = i + 1;
      break;
    }
  }
  return out;
}

// Basically R's get()

SEXP get(SEXP sym, SEXP rho){

  int32_t NP = 0;
  if (TYPEOF(sym) != SYMSXP){
    SHIELD(sym = Rf_coerceVector(sym, SYMSXP)); ++NP;
  }

  if (TYPEOF(rho) != ENVSXP){
    Rf_error("second argument to '%s' must be an environment", __func__);
  }

  // SEXP val = Rf_findVarInFrame(rho, sym); // get(inherits = F)
  SEXP val = Rf_findVar(sym, rho); // get(inherits = T)

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

// basically as.list(call)[-1]
// and is always named

[[cpp11::register]]
SEXP call_args(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP) {
    Rf_error("`expr` must be a language object in %s", __func__);
  }
  int n = Rf_length(expr);
  SEXP out = SHIELD(new_vec(VECSXP, n - 1));
  SEXP names = SHIELD(new_vec(STRSXP, n - 1));

  SEXP current = CDR(expr);
  for (int i = 1; i < n; i++) {
    int j = i - 1;
    SEXP tag = TAG(current);
    if (Rf_isNull(tag)){
      SET_STRING_ELT(names, j, R_BlankString);
    } else {
      SET_STRING_ELT(names, j, PRINTNAME(tag));
    }
    SET_VECTOR_ELT(out, j, CAR(current));
    current = CDR(current);
  }
  set_names(out, names);
  YIELD(2);
  return out;
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

  if (TYPEOF(expr) != LANGSXP){
    Rf_error("`expr` must be a `call` in %s", __func__);
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  if (Rf_length(expr_call) != 3){
    return false;
  }

  SEXP first = CAR(expr_call);
  SEXP second = CAR(CDR(expr_call));

  if (TYPEOF(first) != SYMSXP ||
      (first != R_DoubleColonSymbol &&
      first != R_TripleColonSymbol)){
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    return false;
  }
  return true;
}

SEXP get_namespaced_call_fn(SEXP expr){
  if (!call_is_namespaced(expr)){
    Rf_error("`expr` must be a namespaced call");
  }
  return CAR(CDDR(CAR(expr)));
}

SEXP get_namespaced_call_ns(SEXP expr){
  if (!call_is_namespaced(expr)){
    Rf_error("`expr` must be a namespaced call");
  }
  return CAR(CDR(CAR(expr)));
}

// Basic version of rlang::is_call(expr, ns = ns)
// bool is_ns_call(SEXP expr, SEXP ns){
//
//   int32_t NP = 0;
//
//   if (TYPEOF(ns) != STRSXP){
//     Rf_error("`ns` must be a character vector in %s", __func__);
//   }
//
//   if (TYPEOF(expr) != LANGSXP){
//     return false;
//   }
//
//   SEXP expr_call = CAR(expr);
//   if (TYPEOF(expr_call) != LANGSXP) return false;
//
//   SEXP ns_str = SHIELD(Rf_asChar(ns)); ++NP;
//   SEXP call_list = SHIELD(as_list_call(expr_call)); ++NP;
//
//   if (Rf_length(call_list) != 3){
//     YIELD(NP);
//     return false;
//   }
//
//   SEXP first = SHIELD(VECTOR_ELT(call_list, 0)); ++NP;
//   SEXP second = SHIELD(VECTOR_ELT(call_list, 1)); ++NP;
//
//   if (TYPEOF(first) != SYMSXP ||
//       (first != R_DoubleColonSymbol &&
//       first != R_TripleColonSymbol)){
//     YIELD(NP);
//     return false;
//   }
//   if (TYPEOF(second) != SYMSXP){
//     YIELD(NP);
//     return false;
//   }
//   bool out = rlang::sym_as_string(second) == ns_str;
//   YIELD(NP);
//   return out;
// }

// get the namespace of a function

SEXP get_fun_ns(SEXP x, SEXP rho){
  int32_t NP = 0;
  if (!Rf_isFunction(x)){
    SHIELD(x = get(x, rho)); ++NP;
  }
  if (!Rf_isFunction(x)){
    YIELD(NP);
    return R_BlankString;
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
// doesn't make sense if expr contains inlined-function

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
      SEXP fn_expr_tree = CAR(expr);
      if (TYPEOF(CAR(CDDR(fn_expr_tree))) == SYMSXP &&
          CAR(CDDR(fn_expr_tree)) == fn_sym){
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
// Furthermore, the expression may contain an inlined function instead of the
// function name

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

  // If expr contains inlined-function
  // instead of a symbol to a function

  if (Rf_isFunction(CAR(expr))){

    bool out = false;

    SEXP actual_ns = SHIELD(get_fun_ns(CAR(expr), rho)); ++NP;

    // Check namespace of actual function matches user-supplied one
    // if they give a non-NULL namespace
    if (!Rf_isNull(ns)){
      SEXP target_ns = STRING_ELT(ns, 0);
      if (actual_ns != target_ns){
        YIELD(NP);
        return out;
      }
    }
    SEXP target_fn;
    PROTECT_INDEX target_fn_idx;
    R_ProtectWithIndex(target_fn = R_NilValue, &target_fn_idx); ++NP;

    for (int i = 0; i < n_fns; ++i){
      R_Reprotect(target_fn = get(Rf_installChar(STRING_ELT(fn, i)), rho), target_fn_idx);

      if (!Rf_isFunction(target_fn)){
        continue;
        // YIELD(NP);
        // Rf_error("Could not find function %s", CHAR(STRING_ELT(fn, i)));
      }
      if (functions_equal(target_fn, CAR(expr))){
        out = true;
        break;
      }
    }

    // out will be false if we get to this stage
    YIELD(NP);
    return out;

  // The below code deals with more standard calls

  } else if (Rf_isNull(ns)){
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
      SEXP fn_expr_tree = CAR(expr);
      R_Reprotect(fn_ns = rlang::sym_as_string(CAR(CDR(fn_expr_tree))), fn_ns_idx);
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

// Initialise environment of group unaware fns

static SEXP group_unaware_fns = NULL;
static SEXP group_unaware_fn_names = NULL;

[[cpp11::init]]
void init_group_unaware_fns(DllInfo* dll) {

  // New environment
  group_unaware_fns = R_NewEnv(R_EmptyEnv, TRUE, 60);
  R_PreserveObject(group_unaware_fns);

  group_unaware_fn_names = Rf_allocVector(STRSXP, 50);
  R_PreserveObject(group_unaware_fn_names);

  // fn names
  const char* const names[50] =
    {
    "|", "&", "!", ">=", ">", "<=", "<", "==", "!=", "%%", "%/%",
      "+", "-",  "*", "/", "^", "abs",  "sign", "floor",
      "trunc", "round", "signif", "exp", "log", "(", "{",
      "expm1", "log1p", "cos", "sin", "tan",
      "cospi", "sinpi", "tanpi", "acos", "asin", "atan",
      "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
      "lgamma", "gamma", "digamma", "trigamma",
      "identity",
      "gcd2", "scm2" // cheapr
    };

  SEXP fn = R_NilValue;
  for (int i = 0; i < 48; ++i) {
    fn = Rf_install(names[i]);
    SET_STRING_ELT(group_unaware_fn_names, i, Rf_mkChar(names[i]));
    Rf_defineVar(fn, get(fn, R_BaseEnv), group_unaware_fns);
  }

  for (int i = 48; i < 50; ++i) {
    fn = Rf_install(names[i]);
    SET_STRING_ELT(group_unaware_fn_names, i, Rf_mkChar(names[i]));
    Rf_defineVar(fn, find_pkg_fun(names[i], "cheapr", false), group_unaware_fns);
  }
}

[[cpp11::register]]
SEXP cpp_group_unaware_fns(){
  int n = Rf_length(group_unaware_fns);
  SEXP out = SHIELD(new_vec(VECSXP, n));
  SEXP names = SHIELD(Rf_duplicate(group_unaware_fn_names));

  for (int i = 0; i < n; ++i){
    SEXP fn_name = Rf_installChar(STRING_ELT(names, i));
    SEXP fn = Rf_findVarInFrame(group_unaware_fns, fn_name);
    SET_VECTOR_ELT(out, i, fn);
  }
  set_names(out, names);
  YIELD(2);
  return out;
}

// Recursively check call is group-unaware
// If symbol belongs to mask top env it is group-unaware
// If fn being called belongs to group-unaware fn list
// then it is a group-unaware call
// But all calls and symbols within that must also be group-unaware

[[cpp11::register]]
bool is_group_unaware_call(SEXP expr, SEXP env, SEXP mask){

  if (TYPEOF(expr) != LANGSXP && TYPEOF(expr) != SYMSXP){
    return false;
  }

  int32_t NP = 0;

  if (TYPEOF(expr) == SYMSXP){
    SEXP temp = SHIELD(new_vec(VECSXP, 1)); ++NP;
    SEXP new_quo = SHIELD(rlang::new_quosure(expr, env)); ++NP;
    SET_VECTOR_ELT(temp, 0, new_quo);
    SEXP expr_vars = SHIELD(quo_vars(temp, mask, true)); ++NP;

    SEXP expr_str = SHIELD(rlang::sym_as_string(expr)); ++NP;

    bool sym_in_mask = false;

    for (int i = 0; i < Rf_length(expr_vars); ++i){
      if (expr_str == STRING_ELT(expr_vars, i)){
        sym_in_mask = true;
        break;
      }
    }
    YIELD(NP);
    return sym_in_mask;
  }

  if (is_data_pronoun_call(expr, env)){

    SEXP var = SHIELD(Rf_installChar(data_pronoun_var(expr, env))); ++NP;

    bool sym_in_mask = exists(var, get_mask_top_env(mask));

    YIELD(NP);
    return sym_in_mask;
  }

  bool maybe = is_fn_call(expr, group_unaware_fn_names, R_NilValue, env);

  if (!maybe){
    YIELD(NP);
    return false;
  }

  // Verify that the fn the user is calling is the same as the one
  // stored in our internal group-unaware fn list

  SEXP actual_fn = SHIELD(Rf_eval(CAR(expr), env)); ++NP;
  SEXP group_unaware_functions = SHIELD(cpp_group_unaware_fns()); ++NP;
  maybe = match_fun(actual_fn, group_unaware_functions) != NA_INTEGER;

  if (!maybe){
    YIELD(NP);
    return false;
  }

  // Check remaining symbols and nested calls
  SEXP tree = SHIELD(call_args(expr)); ++NP;
  for (int i = 0; i < Rf_length(tree); ++i){
    SEXP branch = VECTOR_ELT(tree, i);
    if (!is_group_unaware_call(branch, env, mask)){
      YIELD(NP);
      return false;
    }
  }

  YIELD(NP);
  return true;
}
