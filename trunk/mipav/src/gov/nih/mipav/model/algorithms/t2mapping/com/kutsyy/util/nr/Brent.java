package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.Vect;

class BrentState extends NumericBase
{
	public double d, e, v, w;
    public double[] v_v, v_w;
    public double f_v, f_w;

	public BrentState(int n)
	{
		v_v = new double[n]; v_w = new double[n];
	}
};

class Brent extends NumericBase
{
	private int iter;

	private VectorValue left;
	private VectorValue middle;
	private VectorValue right;

    public Brent() 
	{
		_verbose = 4;
		_tol = 1e-6;
        _maxiter=30;
    }

	public double eval(double[] v)
	{
		return getObjFunction().eval(v);
	}

	//-------------------------------------------------------------------------
    public int findBracket( VectorValue start, double[] vDirection) 
	{
		if( left == null ) { left = new VectorValue(vDirection.length); }
		if( middle == null ) { middle = new VectorValue(vDirection.length); }
		if( right == null ) { right = new VectorValue(vDirection.length); }

		// bracket:
		ObjectiveFunction fn = getObjFunction();

		if (getVerbose()>=2) System.out.println("bracketing...");

		left.setValues(start);
		left.x = 0.0;

		right.x = 1.0; 
        Vect.lincomb(start.v,right.x,vDirection,right.v);
		right.f = fn.eval(right.v);

		if (right.f > left.f)
		{
			middle.x = (right.x - left.x) * GOLD + left.x;
            Vect.lincomb(start.v,middle.x,vDirection,middle.v);
			middle.f = fn.eval(middle.v);
		}
		else
		{
			middle.setValues(right);
			right.x = (middle.x - left.x) * INVGOLD + left.x;
			Vect.lincomb(start.v,right.x,vDirection,right.v);
			right.f = fn.eval(right.v);
		}

		do
		{
			if (middle.f < left.f)
			{
				if (middle.f < right.f) return SUCCESS;
				else if (middle.f > right.f)
				{
					left.setValues(middle);
					middle.setValues(right);

					right.x = (middle.x - left.x) * INVGOLD + left.x;
					Vect.lincomb(start.v,right.x,vDirection,right.v);
					right.f = fn.eval(right.v);
				}
				else
				{
					right.setValues(middle);
					middle.x = (right.x - left.x) * GOLD + left.x;
            		Vect.lincomb(start.v,middle.x,vDirection,middle.v);
					middle.f = fn.eval(middle.v);
				}
			}
			else
			{
				// This section was modified by Erick, because the one in
				// GSL is inappropriate for a bidirectional line search.
				right.setValues(middle);
				middle.setValues(left);

				left.x = (middle.x - right.x) * INVGOLD + right.x;
            	Vect.lincomb(start.v,left.x,vDirection,left.v);
				left.f = fn.eval(left.v);
			}

			if ( (right.x - left.x) < getTolerance() * (right.x + left.x)*0.5 )
			return ERROR_BRACKETING_FAILED;

			if ( Vect.linfnorm(vDirection) * (right.x - left.x) < getTolerance() )
			return ERROR_BRACKETING_FAILED;

		} while (++iter <= _maxiter);

		return ERROR_MAXIMUM_ITERATIONS_REACHED;
    }


	//-------------------------------------------------------------------------
    int brentIterate( BrentState st, double[] vStart, double[] vDirection ) 
	{
		ObjectiveFunction fn = getObjFunction();

		double z = middle.x;
		double d = st.e;
		double e = st.d;
		double u, f_u; 
		double v = st.v;
		double w = st.w;
		double[] v_u = new double[vStart.length];
		double[] v_w = Vect.clone(st.v_w), v_z = Vect.clone(middle.v);
		double f_v = st.f_v;
		double f_w = st.f_w;
		double f_z = middle.f;

		double w_lower = (z - left.x);  // distance to boundaries
		double w_upper = (right.x - z);

		double toler = getTolerance() * (1.0 + Math.abs(z));
		double p=0, q=0, r=0;

		double midpoint = 0.5 * (left.x + right.x);

	//cout << "TOL = " << toler << endl;
	//cout << "a: " << left.x << " -> " << left.f << endl;
	//cout << "v: " << st.v << " -> " << f_v << endl;
	//cout << "w: " << st.w << " -> " << f_w << endl;
	//cout << "z: " << z << " -> " << f_z << endl;
	//cout << "b: " << right.x << " -> " << right.f << endl;

		if (Math.abs(e) > toler)
		  {
			// fit parabola
			r = (z - w) * (f_z - f_v);
			q = (z - v) * (f_z - f_w);
			p = (z - v) * q - (z - w) * r;
			q = 2 * (q - r);

			if (q > 0) { p = -p; } else { q = -q; }

			r = e; e = d;
		  }

		if (Math.abs (p) < Math.abs (0.5 * q * r) && p < q * w_lower && p < q * w_upper)
		{
		  double t2 = 2 * toler ;

		  d = p / q; u = z + d;

		  if ((u - left.x) < t2 || (right.x - z) < t2)
		  {
			d = (z < midpoint) ? toler : -toler ;
		  }
		}

		else
		{
		  e = (z < midpoint) ? right.x - z : -(z - left.x) ;
		  d = GOLD * e;
		}

		if (Math.abs(d) >= toler)
		  { u = z + d; }
		else
		  { u = z + ((d > 0) ? toler : -toler) ; }

		st.e = e; st.d = d;

		Vect.lincomb(vStart, u, vDirection, v_u); f_u = fn.eval(v_u);

	//cout << "u: " << u << " -> " << f_u << endl;

		if (f_u > f_z)
		{
		  if (u < z)
		  {
			left.x = u; Vect.copy(left.v,v_u); left.f = f_u;
			return SUCCESS;
		  }
		  else
		  {
			right.x = u; Vect.copy(right.v,v_u); right.f = f_u;
			return SUCCESS;
		  }
		}
		else // if (f_u < f_z)
		{
		  if (u < z) { right.x = z; Vect.copy(right.v,v_z); right.f = f_z; }
		  else       { left.x = z; Vect.copy(left.v,v_z); left.f = f_z; }

		  st.v = w; Vect.copy(st.v_v,v_w); st.f_v = f_w;
		  st.w = z; Vect.copy(st.v_w,v_z); st.f_w = f_z;

		  middle.x = u; Vect.copy(middle.v,v_u); middle.f = f_u;

		  if (f_u <= f_w || w == z)
		  {
			st.v = w; Vect.copy(st.v_v,v_w); st.f_v = f_w;
			st.w = u; Vect.copy(st.v_w,v_u); st.f_w = f_u;
			return SUCCESS;
		  }
		  else if (f_u <= f_v || v == z || v == w)
		  {
			st.v = u; Vect.copy(st.v_v,v_u); st.f_v = f_u;
			return SUCCESS;
		  }
		  else return ERROR_LINE_MINIMIZATION_FAILED;
		}
	}


	//-------------------------------------------------------------------------
	public int findMin( VectorValue start, double[] vDirection,
						VectorValue end )   // RETURNED
	{

		ObjectiveFunction fn = getObjFunction();

		iter = 0;

			if (_verbose>=1) {
				System.out.println("\nmyLineMinimizer:");
				System.out.println("    vStart: " + start);
				System.out.println("vDirection: " + Vect.toString(vDirection));
			}
		
		int stat = findBracket(start,vDirection);

			if (_verbose>=2) {
				System.out.println( "L: " + left );
				System.out.println( "M: " + middle );
				System.out.println( "R: " + right );
			}

		if (stat != SUCCESS) return stat;

			if (_verbose>=2) System.out.println("ranging...");

		iter = 0;

		BrentState st = new BrentState(start.v.length);

		// Initialise state
		st.w = st.v = left.x + GOLD*(right.x - left.x);
        Vect.lincomb(start.v,st.v,vDirection,st.v_w);
		Vect.copy(st.v_v,st.v_w);
		st.f_w = st.f_v = fn.eval(st.v_v);
		st.d = st.e = 0;

		do
		{
		  stat = brentIterate(st,start.v,vDirection);

		  if (stat != SUCCESS) return stat;

			if (_verbose>=3) {
				System.out.println("L: " + left);
				System.out.println("M: " + middle);
				System.out.println("R: " + right);
				System.out.println("--" + "\n");
			}

		  // Now check convergence
		  double min_abs = Math.min(Math.abs(left.x),Math.abs(right.x));

		  if ((left.x>0 && right.x<0) || (left.x<0 && right.x>0)) min_abs = 0;
		  if (left.x > right.x) return ERROR_LINE_MINIMIZATION_FAILED;

		  double toler = getTolerance() * (1.0 + min_abs);

		  // Tolerance has been reached
		  if (right.x - left.x < 4.0*toler) break;

		} while (++iter <= _maxiter);

		end.setValues(middle);

		if (_verbose>=2) System.out.println("iter = " + iter);

		if (iter > _maxiter) return ERROR_MAXIMUM_ITERATIONS_REACHED;

		return SUCCESS;
	}
}
