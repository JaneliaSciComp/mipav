package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;

public class PPolynomial {

	public int polyCount;
	public StartingPolynomial[] polys;
	public int Degree;

	public PPolynomial(int _Degree) {
		polyCount = 0;
		Degree = _Degree;
		polys = null;
	}

	public PPolynomial(final PPolynomial p) {
		polyCount = 0;
		polys = null;
		Degree = p.Degree;
		set(p.polyCount);
		// memcpy(polys,p.polys,sizeof(StartingPolynomial<Degree>)*p.polyCount);
		System.arraycopy(p.polys, 0, polys, 0, polyCount);
	}

	public void dispose() {
		if (polyCount != 0) {
			polys = null;
		}
		polyCount = 0;
		polys = null;
	}

	public void set(StartingPolynomial[] sps, final int count){
		int i,c=0;
		set(count);
		StartingPolynomial temp = new StartingPolynomial(sps[0].Degree);
		SortUtil.qsort(sps, temp);
		for(i=0;i<count;i++){
			if( c == 0 || sps[i].start!=polys[c-1].start){polys[c++]=sps[i];}
			else{polys[c-1].p.add_into(sps[i].p);}
		}
		reset(c);
	}

	public final int size() {
		return polyCount;
	}

	public void set(int size) {
		if (polyCount != 0) {
			polys = null;
		}
		polyCount = 0;
		polys = null;
		polyCount = size;
		if (size != 0) {
			polys = new StartingPolynomial[size];
			for (int i = 0; i < size; i++) {
				polys[i] = new StartingPolynomial(Degree);
			}
			// memset(polys,0,sizeof(StartingPolynomial<Degree>)*size);
		}
	}

	public void reset(final int newSize) {
		
	    // polys=(StartingPolynomial<Degree>*)realloc(polys,sizeof(StartingPolynomial<Degree>)*newSize);
		/*
		StartingPolynomial[] old = new StartingPolynomial[polys.length];
		System.arraycopy(polys, 0, old, 0, polys.length);
		polyCount = newSize;
		polys = new StartingPolynomial[newSize];
		System.arraycopy(old, 0, polys, 0, Math.min(old.length, polyCount));
	     */
		/*
		for (int i = Math.min(old.length, polyCount); i < newSize; i++ ) {
			polys[i] = new StartingPolynomial(Degree);
		}
		*/
		polyCount = newSize;
		StartingPolynomial[] old = new StartingPolynomial[size()];
		System.arraycopy(polys, 0, old, 0, size());
		polys = new StartingPolynomial[size()];
		System.arraycopy(old, 0, polys, 0, size());
		
	}

	// operator =
	public PPolynomial set(final PPolynomial p) {
		set(p.polyCount);
		Degree = p.Degree;
		// memcpy(polys,p.polys,sizeof(StartingPolynomial<Degree>)*p.polyCount);
		System.arraycopy(p.polys, 0, polys, 0, p.polyCount);
		return this;
	}

	// operator =
	public PPolynomial set2(final PPolynomial p) {
		set(p.polyCount);
		Degree = p.Degree;
		for (int i = 0; i < polyCount; i++) {
			polys[i].start = p.polys[i].start;
			polys[i].p = (p.polys[i].p);
		}
		return this;
	}

	// operator ()
	public final double evaluate(final double t) {
		double v = 0;
		for (int i = 0; i < (int) (polyCount) && t > polys[i].start; i++) {
			v += polys[i].p.evaluate(t);
		}
		return v;
	}

	public final double integral(final double tMin, final double tMax) {
		int m = 1;
		double start, end, s, v = 0;
		start = tMin;
		end = tMax;
		if (tMin > tMax) {
			m = -1;
			start = tMax;
			end = tMin;
		}
		for (int i = 0; i < (int) (polyCount) && polys[i].start < end; i++) {
			if (start < polys[i].start) {
				s = polys[i].start;
			} else {
				s = start;
			}
			v += polys[i].p.integral(s, end);
		}
		return v * m;
	}

	public final double Integral() {
		return integral(polys[0].start, polys[polyCount - 1].start);
	}

	// operator +
	public final PPolynomial add(final PPolynomial p) {
		PPolynomial q = new PPolynomial(p.Degree);
		int i, j;
		int idx = 0;
		q.set(polyCount + p.polyCount);
		i = j = -1;

		while (idx < q.polyCount) {
			if (j >= (int) (p.polyCount) - 1) {
				q.polys[idx] = polys[++i];
			} else if (i >= (int) (polyCount) - 1) {
				q.polys[idx] = p.polys[++j];
			} else if (polys[i + 1].start < p.polys[j + 1].start) {
				q.polys[idx] = polys[++i];
			} else {
				q.polys[idx] = p.polys[++j];
			}
			// if(idx && polys[idx].start==polys[idx-1].start)
			// {polys[idx-1].p+=polys[idx].p;}
			// else{idx++;}
			idx++;
		}
		return q;
	}

	// operator -
	public final PPolynomial sub(final PPolynomial p) {
		PPolynomial q = new PPolynomial(p.Degree);
		int i, j;
		int idx = 0;
		q.set(polyCount + p.polyCount);
		i = j = -1;

		while (idx < q.polyCount) {
			if (j >= (int) (p.polyCount) - 1) {
				q.polys[idx] = polys[++i];
			} else if (i >= (int) (polyCount) - 1) {
				q.polys[idx].start = p.polys[++j].start;
				q.polys[idx].p = (p.polys[j].p.mul(-1.0));
			} else if (polys[i + 1].start < p.polys[j + 1].start) {
				q.polys[idx] = polys[++i];
			} else {
				q.polys[idx].start = p.polys[++j].start;
				q.polys[idx].p = (p.polys[j].p.mul(-1.0));
			}
			// if(idx && polys[idx].start==polys[idx-1].start)
			// {polys[idx-1].p+=polys[idx].p;}
			// else{idx++;}
			idx++;
		}
		return q;
	}

	
	public PPolynomial addScaled(final PPolynomial p, final double scale) {
		int i, j;
		StartingPolynomial[] oldPolys = polys;
		int idx = 0, cnt = 0, oldPolyCount = polyCount;
		polyCount = 0;
		polys = null;
		set(oldPolyCount + p.polyCount);
		i = j = -1;
		while (cnt < polyCount) {
			if (j >= (int) (p.polyCount) - 1) {
				polys[idx] = oldPolys[++i];
			} else if (i >= (int) (oldPolyCount) - 1) {
				polys[idx].start = p.polys[++j].start;
				polys[idx].p = (p.polys[j].p.mul(scale));
			} else if (oldPolys[i + 1].start < p.polys[j + 1].start) {
				polys[idx] = oldPolys[++i];
			} else {
				polys[idx].start = p.polys[++j].start;
				polys[idx].p = (p.polys[j].p.mul(scale));
			}
			if (idx != 0 && polys[idx].start == polys[idx - 1].start) {
				polys[idx - 1].p.add_into(polys[idx].p);
			} else {
				idx++;
			}
			cnt++;
		}
		oldPolys = null;
		reset(idx);
		return this;
	}

	// operator *
	public final PPolynomial mul(final PPolynomial p) {
		PPolynomial q = new PPolynomial(Degree + p.Degree);
		StartingPolynomial[] sp;
		int i, j, spCount = (int) (polyCount * p.polyCount);

		// sp=(StartingPolynomial<Degree+Degree2>*)malloc(sizeof(StartingPolynomial<Degree+Degree2>)*spCount);
		sp = new StartingPolynomial[spCount];
		for (i = 0; i < (int) (polyCount); i++) {
			for (j = 0; j < (int) (p.polyCount); j++) {
				sp[i * p.polyCount + j] = new StartingPolynomial(Degree + p.Degree);
				sp[i * p.polyCount + j] = polys[i].mul(p.polys[j]);
			}
		}
		q.set(sp, spCount);
		sp = null;
		return q;
	}

	// operator *
	public final PPolynomial mul(final Polynomial p) {
		PPolynomial q = new PPolynomial(Degree + p.Degree);
		q.set(polyCount);
		for (int i = 0; i < (int) (polyCount); i++) {
			q.polys[i].start = polys[i].start;
			q.polys[i].p = (polys[i].p.mul(p));
		}
		return q;
	}

	public final PPolynomial scale(final double s) {
		PPolynomial q = new PPolynomial(Degree);
		q.set(polyCount);
		for (int i = 0; i < polyCount; i++) {
			q.polys[i] = polys[i].scale(s);
		}
		return q;
	}

	public final PPolynomial shift(final double s) {
		PPolynomial q = new PPolynomial(Degree);
		q.set(polyCount);
		for (int i = 0; i < polyCount; i++) {
			q.polys[i] = polys[i].shift(s);
		}
		return q;
	}

	public final PPolynomial derivative() {
		PPolynomial q = new PPolynomial(Degree - 1);
		q.set(polyCount);
		for (int i = 0; i < polyCount; i++) {
			q.polys[i].start = polys[i].start;
			q.polys[i].p = (polys[i].p.derivative());
		}
		return q;
	}

	public final PPolynomial integral() {
		int i;
		PPolynomial q = new PPolynomial(Degree + 1);
		q.set(polyCount);
		for (i = 0; i < (int) (polyCount); i++) {
			q.polys[i].start = polys[i].start;
			q.polys[i].p = (polys[i].p.integral());
			q.polys[i].p.sub_into(q.polys[i].p.evaluate(q.polys[i].start));
		}
		return q;
	}

	// operator +=
	public PPolynomial add_into(final double s) {
		polys[0].p.add_into(s);
		return this;
	}

	// operator -=
	public PPolynomial sub_into(final double s) {
		polys[0].p.sub_into(s);
		return this;
	}

	// operator *=
	public PPolynomial mul_into(final double s) {
		for (int i = 0; i < (int) (polyCount); i++) {
			polys[i].p.mul_into(s);
		}
		return this;
	}

	// operator /=
	public PPolynomial div_into(final double s) {
		for (int i = 0; i < polyCount; i++) {
			polys[i].p.div_into(s);
		}
		return this;
	}

	// operator +
	public final PPolynomial add(final double s) {
		PPolynomial q = new PPolynomial(this);
		q.add_into(s);
		return q;
	}

	// operator -
	public final PPolynomial sub(final double s) {
		PPolynomial q = new PPolynomial(this);
		q.sub_into(s);
		return q;
	}

	// operator *
	public final PPolynomial mul(final double s) {
		PPolynomial q = new PPolynomial(this);
		q.mul_into(s);
		return q;
	}

	// operator /
	public final PPolynomial div(final double s) {
		PPolynomial q = new PPolynomial(this);
		q.div_into(s);
		return q;
	}

	public final void Print() {
		Polynomial p = new Polynomial(Degree);

		if (polyCount == 0) {
			p = new Polynomial(Degree);
			System.err.println("[-Infinity,Infinity]\n");
		} else {
			for (int i = 0; i < polyCount; i++) {
				System.err.print("[");
				if (polys[i].start == Double.MAX_VALUE) {
					System.err.print("Infinity,");
				} else if (polys[i].start == -Double.MAX_VALUE) {
					System.err.print("-Infinity,");
				} else {
					System.err.print(polys[i].start + ",");
				}
				if (i + 1 == polyCount) {
					System.err.print("Infinity]\t");
				} else if (polys[i + 1].start == Double.MAX_VALUE) {
					System.err.print("Infinity]\t");
				} else if (polys[i + 1].start == -Double.MAX_VALUE) {
					System.err.print("-Infinity]\t");
				} else {
					System.err.print(polys[i + 1].start + "]\t");
				}
				p = p.add(polys[i].p);
				p.Print();
			}
		}
		System.err.print("\n");
	}

	public PPolynomial ConstantFunction() {
		double radius = 0.5;
		if (Degree < 0) {
			System.err.print("Could not set degree " + Degree
					+ " polynomial as constant\n");
			System.exit(0);
		}
		PPolynomial q = new PPolynomial(Degree);
		q.set(2);

		q.polys[0].start = -radius;
		q.polys[1].start = radius;

		q.polys[0].p.coefficients[0] = 1.0;
		q.polys[1].p.coefficients[0] = -1.0;
		return q;
	}

	public PPolynomial ConstantFunction(double radius) {
		if (Degree < 0) {
			System.err.print("Could not set degree " + Degree
					+ " polynomial as constant\n");
			System.exit(0);
		}
		PPolynomial q = new PPolynomial(Degree);
		q.set(2);

		q.polys[0].start = -radius;
		q.polys[1].start = radius;

		q.polys[0].p.coefficients[0] = 1.0;
		q.polys[1].p.coefficients[0] = -1.0;
		return q;
	}

	public PPolynomial GaussianApproximation0(final double width) {
		return ConstantFunction(width);
	}

	public PPolynomial GaussianApproximation(final double width, int Degree) {
		if ( Degree == 0 ) return GaussianApproximation0(width);
		PPolynomial p = new PPolynomial(Degree-1);
		return p.GaussianApproximation(0.5, p.Degree).MovingAverage(width);
	}

	public PPolynomial MovingAverage(final double radius) {
		PPolynomial A = new PPolynomial(Degree + 1);
		Polynomial p = new Polynomial(Degree + 1);
		StartingPolynomial[] sps;
        int i;
		
		// sps=(StartingPolynomial<Degree+1>*)malloc(sizeof(StartingPolynomial<Degree+1>)*polyCount*2);
		sps = new StartingPolynomial[polyCount * 2];
		for (i = 0; i < polyCount * 2; i++ ) {
			sps[i] = new StartingPolynomial(Degree+1);
		}

		for (i = 0; i < (int) (polyCount); i++) {
			sps[2 * i].start = polys[i].start - radius;
			sps[2 * i + 1].start = polys[i].start + radius;
			p = polys[i].p.integral().sub(
					polys[i].p.integral().evaluate(polys[i].start));
			sps[2 * i].p = p.shift(-radius);
			sps[2 * i + 1].p = p.shift(radius).mul(-1);
		}
		A.set(sps, (int) (polyCount * 2));
		sps = null;
		return A.mul(1.0f / (2f * radius));
	}

	public final void getSolutions(final double c, Vector<Double> roots, final double EPS,
			final double min, final double max) {
		Polynomial p = new Polynomial(Degree);
		Vector<Double> tempRoots = new Vector<Double>();

		p.setZero();
		for (int i = 0; i < polyCount; i++) {
			p.add_into(polys[i].p);
			if (polys[i].start > max) {
				break;
			}
			if (i < polyCount - 1 && polys[i + 1].start < min) {
				continue;
			}
			p.getSolutions(c, tempRoots, EPS);
			for (int j = 0; j < tempRoots.size(); j++) {
				if (tempRoots.get(j) > polys[i].start
						&& (i + 1 == polyCount || tempRoots.get(j) <= polys[i + 1].start)) {
					if (tempRoots.get(j) > min && tempRoots.get(j) < max) {
						roots.add(tempRoots.get(j));
					}
				}
			}
		}
	}

	/*
	void write(FILE* fp,int& samples,double& min,double& max) const{
		fwrite(&samples,sizeof(int),1,fp);
		for(int i=0;i<samples;i++){
			double x=min+i*(max-min)/(samples-1);
			float v=(*this)(x);
			fwrite(&v,sizeof(float),1,fp);
		}
	}
	 */

}