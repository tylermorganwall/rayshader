/**
     * Methods related to animations
     * @name ___METHODS_FOR_ANIMATION___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
    /**
     * Binary search
     * @param x - x coordinates in increasing order
     * @param newx - value to find, assumed to be in the range of x
     * @result index of largest x value below newx
     */
    rglwidgetClass.bisect = function(x, newx) {
      var lo = 0, hi = x.length - 1, mid;
      while (lo < hi - 1) {
        mid = Math.round((lo + hi)/2);
        if (x[mid] < newx)
          lo = mid;
        else
          hi = mid;
      }
      return lo;
    };
    
    /**
     * Step interpolation (constant outside bounds)
     * @param x - x coordinates in increasing order
     * @param v - values at x; either a vector or matrix
     * @param newx - value at which to evaluate
     */
    rglwidgetClass.step = function(x, v, newx) {
      var n, lo;
      if (newx <= x[0])
        return v[0];    
      n = x.length;
      if (newx >= x[n-1])
        return v[n-1];
      lo = this.bisect(x, newx);
      return v[lo];
    };
    
    /**
     * Linear interpolation (constant outside bounds)
     * @param x - x coordinates in increasing order
     * @param v - values at x; either a vector or matrix
     * @param newx - value at which to evaluate
     */
    rglwidgetClass.lerp = function(x, v, newx) {
      var i, n, lo, hi, alpha, result;
      if (newx <= x[0])
        return v[0];    
      n = x.length;
      if (newx >= x[n-1])
        return v[n-1];
      lo = this.bisect(x, newx);
      if (newx === x[lo])
        return v[lo];
      hi = lo + 1;
      if (newx === x[hi])
        return v[hi];
      alpha = (newx - x[lo])/(x[hi] - x[lo]);
      result = v[lo];
      n = result.length;
      if (typeof n !== "undefined") {
        for (i = 0; i < n; i++)
          result[i] = (1 - alpha)*result[i] + alpha*v[hi][i];
      } else
        result = (1 - alpha)*result + alpha*v[hi];
      return result;
    };
    
    /**
     * Spherical linear interpolation (constant outside bounds)
     * @param x - x coordinates in increasing order
     * @param v - a matrix of unit quaternions
     * @param newx - value at which to evaluate
     */
    rglwidgetClass.slerp = function(x, v, newx) {
      var n, lo, hi, alpha, result,
          p0, p1, dot, Omega, alpha0, alpha1, len;
      if (newx <= x[0])
        return v[0];    
      if (newx >= x[n-1])
        return v[n-1];
      lo = this.bisect(x, newx);
      if (newx === x[lo])
        return v[lo];
      hi = lo + 1;
      if (newx === x[hi])
        return v[hi];
      p0 = v[lo];
      p1 = v[hi];
      dot = p0[0]*p1[0] + 
            p0[1]*p1[1] +
            p0[2]*p1[2] +
            p0[3]*p1[3];
      if (dot < 0) {
        p1 = [-p1[0], -p1[1], -p1[2], -p1[3]];
        dot = -dot;
      }
      if (dot >= 1)
        result = p1;
      else {
        alpha = (newx - x[lo])/(x[hi] - x[lo]);
        Omega = Math.acos(dot);
        alpha0 = Math.sin((1 - alpha)*Omega);
        alpha1 = Math.sin(alpha*Omega);
        result = [alpha0*p0[0] + alpha1*p1[0],
                  alpha0*p0[1] + alpha1*p1[1],
                  alpha0*p0[2] + alpha1*p1[2],
                  alpha0*p0[3] + alpha1*p1[3]];
      }
      len = Math.sqrt(result[0]*result[0] +
                      result[1]*result[1] +
                      result[2]*result[2] +
                      result[3]*result[3]);
      return [result[0]/len,
              result[1]/len,
              result[2]/len,
              result[3]/len];
    };

    /**
     * Rotate using unit quaternion
     * @param q - a single unit quaternion
     */
    rglwidgetClass.rotateByQuaternion = function(M, q) {

    var xx = q[0]*q[0],
        xy = q[0]*q[1],
        xz = q[0]*q[2],
        xw = q[0]*q[3],
        yy = q[1]*q[1],
        yz = q[1]*q[2],
        yw = q[1]*q[3],
        zz = q[2]*q[2],
        zw = q[2]*q[3],
        matrix = new CanvasMatrix4();
      matrix.m11 = 1 - 2*(yy + zz);
      matrix.m12 = 2*(xy + zw);
      matrix.m13 = 2*(xz - yw);
        
      matrix.m21 = 2*(xy - zw);
      matrix.m22 = 1 - 2*(xx + zz);
      matrix.m23 = 2*(yz + xw);

      matrix.m31 = 2*(xz + yw);
      matrix.m32 = 2*(yz - xw);
      matrix.m33 = 1 - 2*(xx + yy);

      M.multRight(matrix);      
    };
