functions {
  
  vector combine_lambdas(
    vector state, vector county, vector school
  ) {
    return state + county + school
  }
  vector combine_lambdas(
    vector state, vector county, vector school, vector other
  ) {
    return state + county + school + other
  }
  
}