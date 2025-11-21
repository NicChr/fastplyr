#include "fastplyr.h"

static SEXP top_env_sym = NULL;
static SEXP data_pronoun_sym = NULL;

[[cpp11::init]]
void init_mask_symbols(DllInfo* dll){

  // Symbols get added to internal R protected list so no need to preserve

  top_env_sym = Rf_installChar(Rf_mkCharCE(".top_env", CE_UTF8));
  data_pronoun_sym = Rf_installChar(Rf_mkCharCE(".data", CE_UTF8));
}

SEXP get_mask_top_env(SEXP mask){
  return Rf_findVarInFrame(mask, top_env_sym);
}

SEXP get_mask_data_vars(SEXP mask){

  SEXP top_env = SHIELD(get_mask_top_env(mask));

  SEXP top_env_vars = SHIELD(
    R_lsInternal3(
      top_env,
      TRUE, // Don't exclude objects beginning with `.` like `.data`
      FALSE // Unsorted
    )
  );

  SEXP data_pronoun = SHIELD(rlang::sym_as_character(data_pronoun_sym));

  SEXP out = SHIELD(cheapr::val_remove(top_env_vars, data_pronoun));

  YIELD(4);
  return out;

}

SEXP new_bare_data_mask(){
  SEXP env = SHIELD(R_NewEnv(R_EmptyEnv, false, 0));
  SEXP mask = SHIELD(rlang::new_data_mask(env, env));
  SEXP top_env = SHIELD(get_mask_top_env(mask));

  // Add .data pronoun
  SEXP data_pronoun = SHIELD(rlang::as_data_pronoun(env));
  Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  YIELD(4);
  return mask;
}

// void reset_mask_top_env(SEXP mask, int env_size){
//   Rf_defineVar(top_env_sym, mask, R_NewEnv(R_EmptyEnv, false, env_size));
// }
//
// void add_to_mask(SEXP mask, SEXP result, SEXP name){
//
//   SEXP top_env = get_mask_top_env(mask);
//
//   if (name != R_UnboundValue){
//     Rf_defineVar(name, result, top_env);
//   }
//
//   if (Rf_isNull(result)){
//     R_removeVarFromFrame(name, top_env);
//   }
// }
