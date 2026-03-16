/**
 * Pretty function from R
 * @name ___PRETTY_FROM_R___
 * @memberof rglwidgetClass
 * @kind function
 * @instance
 */


/* This file is translated from pretty.c, which was
 taken from the R sources, r61744 of src/appl/pretty.c,
 with minimal changes */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2012  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/**     
 * Construct pretty values to cover an interval
 * @param { number } lo - lower end of interval
 * @param { number } up - upper end of interval
 * @param { number } ndiv - requested number of divisions
 * @param { number } min_n - minimum divisions
 * @param { number } shrink_sml - if too many cells, amount to shrink by
 * @param { number } high_u_fact - bias in favour of larger units
 * @param { number } eps_correction - correction to bounds
 * @param { Boolean } return_bounds - whether to return bounds
 * @description
 * Pretty Intervals

 * Constructs m "pretty" values which cover the given interval	*lo <= *up
 *	m ~= *ndiv + 1	(i.e., ndiv := approximate number of INTERVALS)
 *
 * It is not quite clear what should happen for	 *lo = *up;
 * S itself behaves quite funilly, then.
 *
 * In my opinion, a proper 'pretty' should always ensure
 * *lo < *up, and hence *ndiv >=1 in the result.
 * However, in S and here, we allow  *lo == *up, and *ndiv = 0.
 * Note however, that we are NOT COMPATIBLE to S. [Martin M.]
 *
 * NEW (0.63.2): ns, nu are double (==> no danger of integer overflow)
 *
 * We determine
 * if the interval (up - lo) is ``small'' [<==>	 i_small == TRUE, below].
 * For the ``i_small'' situation, there is a parameter  shrink_sml,
 * the factor by which the "scale" is shrunk.		~~~~~~~~~~
 * It is advisable to set it to some (smaller) integer power of 2,
 * since this enables exact floating point division.
 */
rglwidgetClass.prototype.R_pretty = function(
  lo, up, ndiv, min_n, shrink_sml, high_u_fact,
  eps_correction, return_bounds) {
  /* From version 0.65 on, we had rounding_eps := 1e-5, before, r..eps = 0
   * 1e-7 is consistent with seq.default() */
  var rounding_eps = 1e-7,  h = high_u_fact[0],
                                           h5 = high_u_fact[1], 
                                                           dx, cell, unit, base, U, ns, nu, k, i_small,
                                                           DBL_EPSILON = Number.EPSILON,
                                                           DBL_MIN = Number.MIN_VALUE,
                                                           DBL_MAX = Number.MAX_VALUE;
  
  dx = up - lo;
  /* cell := "scale"	here */
  if (dx === 0 && up === 0) { /*  up == lo == 0	 */
  cell = 1;
    i_small = true;
  } else {
    cell = Math.max(Math.abs(lo), Math.abs(up));
    /* U = upper bound on cell/unit */
    U = (1 + (h5 >= 1.5*h+0.5)) ? 1/(1+h) : 1.5/(1+h5);
    /* added times 3, as several calculations here */
    i_small = dx < cell * U * Math.max(1,ndiv) * DBL_EPSILON *3;
  }
  
  /*OLD: cell = FLT_EPSILON+ dx / *ndiv; FLT_EPSILON = 1.192e-07 */
  if(i_small) {
    if(cell > 10)
      cell = 9 + cell/10;
    cell *= shrink_sml;
    if(min_n > 1) cell /= min_n;
  } else {
    cell = dx;
    if(ndiv > 1) cell /= ndiv;
  }
  
  if(cell < 20*DBL_MIN) {
    /* warning(_("Internal(pretty()): very small range.. corrected")); */
    cell = 20*DBL_MIN;
  } else if(cell * 10 > DBL_MAX) {
    /* warning(_("Internal(pretty()): very large range.. corrected")); */
    cell = 0.1*DBL_MAX;
  }
  base = Math.pow(10, Math.floor(Math.log10(cell))); /* base <= cell < 10*base */
    
    /* unit : from { 1,2,5,10 } * base
     *	 such that |u - cell| is small,
     * favoring larger (if h > 1, else smaller)  u  values;
     * favor '5' more than '2'  if h5 > h  (default h5 = .5 + 1.5 h) */
    unit = base;
    if((U = 2*base)-cell <  h*(cell-unit)) { unit = U;
      if((U = 5*base)-cell < h5*(cell-unit)) { unit = U;
        if((U =10*base)-cell <  h*(cell-unit)) unit = U; }}
    /* Result: c := cell,  u := unit,  b := base
     *	c in [	1,	      (2+ h) /(1+h) ] b ==> u=  b
     *	c in ( (2+ h)/(1+h),  (5+2h5)/(1+h5)] b ==> u= 2b
     *	c in ( (5+2h)/(1+h), (10+5h) /(1+h) ] b ==> u= 5b
     *	c in ((10+5h)/(1+h),	         10 ) b ==> u=10b
     *
     *	===>	2/5 *(2+h)/(1+h)  <=  c/u  <=  (2+h)/(1+h)	*/
    
    ns = Math.floor(lo/unit+rounding_eps);
    nu = Math.ceil (up/unit-rounding_eps);
    
    if(eps_correction && (eps_correction > 1 || !i_small)) {
      if(lo !== 0.0) lo *= (1- DBL_EPSILON); else lo = -DBL_MIN;
      if(up !== 0.0) up *= (1+ DBL_EPSILON); else up = +DBL_MIN;
    }
    
    while(ns*unit > lo + rounding_eps*unit) ns--;
    
    while(nu*unit < up - rounding_eps*unit) nu++;
    
    k = Math.floor(0.5 + nu - ns);
    if(k < min_n) {
      /* ensure that	nu - ns	 == min_n */
      
      k = min_n - k;
      if(ns >= 0) {
        nu += k/2;
        ns -= k/2 + k%2;/* ==> nu-ns = old(nu-ns) + min_n -k = min_n */
      } else {
        ns -= k/2;
        nu += k/2 + k%2;
      }
      ndiv = min_n;
    } else {
      ndiv = k;
    }
    if(return_bounds) { /* if()'s to ensure that result covers original range */
      if(ns * unit < lo) lo = ns * unit;
      if(nu * unit > up) up = nu * unit;
    } else {
      lo = ns;
      up = nu;
    }
    return {lo:lo, up:up, ndiv:ndiv, unit:unit};
};
