package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;

public class Polynomial {

	public double coefficients[];
	public int Degree;

	public Polynomial(int _Degree) {
		// memset(coefficients,0,sizeof(double)*(Degree+1));
		Degree = _Degree;
		coefficients = new double[Degree + 1];
	}

	public Polynomial(final Polynomial P, int _Degree) {
		Degree = _Degree;
		coefficients = new double[Degree + 1];
		// memset(coefficients,0,sizeof(double)*(Degree+1));
		for (int i = 0; i <= Degree && i <= P.Degree; i++) {
			coefficients[i] = P.coefficients[i];
		}
	}

	// operator =
	public Polynomial set(final Polynomial p) {
		int Degree2 = p.Degree;
		int d = Degree < Degree2 ? Degree : Degree2;
		// memset(coefficients,0,sizeof(double)*(Degree+1));
		coefficients = new double[Degree + 1];
		// memcpy(coefficients,p.coefficients,sizeof(double)*(d+1));
		System.arraycopy(p.coefficients, 0, coefficients, 0, (d + 1));
		return this;
	}

	
	public final Polynomial derivative() {
		Polynomial p = new Polynomial(Degree - 1);
		for (int i = 0; i < Degree; i++) {
			p.coefficients[i] = coefficients[i + 1] * (i + 1);
		}
		return p;
	}

	public final Polynomial integral() {
		Polynomial p = new Polynomial(Degree + 1);
		p.coefficients[0] = 0;
		for (int i = 0; i <= Degree; i++) {
			p.coefficients[i + 1] = coefficients[i] / (i + 1);
		}
		return p;
	}

	// operator(),
	public final double evaluate(final double t) {
		double temp = 1;
		double v = 0;
		for (int i = 0; i <= Degree; i++) {
			v += temp * coefficients[i];
			temp *= t;
		}
		return v;
	}

	public final double integral(final double tMin, final double tMax) {
		double v = 0;
		double t1, t2;
		t1 = tMin;
		t2 = tMax;
		for (int i = 0; i <= Degree; i++) {
			v += coefficients[i] * (t2 - t1) / (i + 1);
			if (t1 != -Double.MAX_VALUE && t1 != Double.MAX_VALUE) {
				t1 *= tMin;
			}
			if (t2 != -Double.MAX_VALUE && t2 != Double.MAX_VALUE) {
				t2 *= tMax;
			}
		}
		return v;
	}

	// operator ==
	public final int equals(final Polynomial p) {
		for (int i = 0; i <= Degree; i++) {
			if (coefficients[i] != p.coefficients[i]) {
				return 0;
			}
		}
		return 1;
	}

	// operator !=
	public final int not_equals(final Polynomial p) {
		for (int i = 0; i <= Degree; i++) {
			if (coefficients[i] == p.coefficients[i]) {
				return 0;
			}
		}
		return 1;
	}

	public final int isZero() {
		for (int i = 0; i <= Degree; i++) {
			if (coefficients[i] != 0) {
				return 0;
			}
		}
		return 1;
	}

	public final void setZero() {
		// memset(coefficients,0,sizeof(double)*(Degree+1));
		coefficients = new double[Degree + 1];
	}

	public Polynomial addScaled(final Polynomial p, final double s) {
		for (int i = 0; i <= Degree; i++) {
			coefficients[i] += p.coefficients[i] * s;
		}
		return this;
	}

	// operator +=
	public Polynomial add_into(final Polynomial p) {
		for (int i = 0; i <= Degree; i++) {
			coefficients[i] += p.coefficients[i];
		}
		return this;
	}

	// operator -=
	public Polynomial sub_into(final Polynomial p) {
		for (int i = 0; i <= Degree; i++) {
			coefficients[i] -= p.coefficients[i];
		}
		return this;
	}

	// operator +
	public final Polynomial add(final Polynomial p) {
		Polynomial q = new Polynomial(p.Degree);
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = (coefficients[i] + p.coefficients[i]);
		}
		return q;
	}

	// operator -
	public final Polynomial sub(final Polynomial p) {
		Polynomial q = new Polynomial(p.Degree);
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = coefficients[i] - p.coefficients[i];
		}
		return q;
	}

	public void Scale(final Polynomial p, final double w, Polynomial q) {
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = p.coefficients[i] * w;
		}
	}

	public void AddScaled(final Polynomial p1, final double w1, final Polynomial p2, final double w2,
			Polynomial q) {
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = p1.coefficients[i] * w1 + p2.coefficients[i]
					* w2;
		}
	}

	public void AddScaled(final Polynomial p1, final double w1, final Polynomial p2, Polynomial q) {
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = p1.coefficients[i] * w1 + p2.coefficients[i];
		}
	}

	public void AddScaled(final Polynomial p1, final Polynomial p2, final double w2, Polynomial q) {
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = p1.coefficients[i] + p2.coefficients[i] * w2;
		}
	}

	public void Subtract(final Polynomial p1, final Polynomial p2, Polynomial q) {
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = p1.coefficients[i] - p2.coefficients[i];
		}
	}

	public void Negate(final Polynomial in, Polynomial out) {
		out = in;
		for (int i = 0; i <= Degree; i++) {
			out.coefficients[i] = -out.coefficients[i];
		}
	}

	// operator -
	public final Polynomial neg() {
		Polynomial q= new Polynomial(this, this.Degree);
		for (int i = 0; i <= Degree; i++) {
			q.coefficients[i] = -q.coefficients[i];
		}
		return q;
	}

	// operator *
	public final Polynomial mul(final Polynomial p) {
		Polynomial q = new Polynomial(Degree + p.Degree);
		for (int i = 0; i <= Degree; i++) {
			for (int j = 0; j <= p.Degree; j++) {
				q.coefficients[i + j] += coefficients[i] * p.coefficients[j];
			}
		}
		return q;
	}

	// operator += 
	public Polynomial add_into(final double s){
		coefficients[0]+=s;
		return this;
	}
	
	// operator -= 
	public Polynomial sub_into(final double s){
		coefficients[0]-=s;
		return this;
	}
	
	// operator *= 
	public Polynomial mul_into(final double s){
		for(int i=0;i<=Degree;i++){coefficients[i]*=s;}
		return this;
	}
	
	// operator /= 
	public Polynomial div_into(final double s){
		for(int i=0;i<=Degree;i++){coefficients[i]/=s;}
		return this;
	}
	
	// operator + 
	public final Polynomial add(final double s) {
		Polynomial q= new Polynomial(this, this.Degree);
		q.coefficients[0]+=s;
		return q;
	}
	
	// operator - 
	public final Polynomial sub(final double s) {
		Polynomial q=new Polynomial(this, this.Degree);
		q.coefficients[0]-=s;
		return q;
	}
	
	// operator * 
	public final Polynomial mul(final double s) {
		Polynomial q = new Polynomial(Degree);
		for(int i=0;i<=Degree;i++){ q.coefficients[i]=coefficients[i]*s;}
		return q;
	}
	
	// operator / 
	public final Polynomial div(final double s) {
		Polynomial q = new Polynomial(Degree);
		for(int i=0;i<=Degree;i++){q.coefficients[i]=coefficients[i]/s;}
		return q;
	}
	
	public final Polynomial scale(final double s) {
		Polynomial q= new Polynomial(this, this.Degree);
		double s2=1.0;
		for(int i=0;i<=Degree;i++){
			q.coefficients[i]*=s2;
			s2/=s;
		}
		return q;
	}
	
	public final Polynomial shift(final double t) {
		Polynomial q = new Polynomial(Degree);
		for(int i=0;i<=Degree;i++){
			double temp=1;
			for(int j=i;j>=0;j--){
				q.coefficients[j]+=coefficients[i]*temp;
				temp*=-t*j;
				temp/=(i-j+1);
			}
		}
		return q;
	}
	
	public final void Print() {
		for(int j=0;j<=Degree;j++){
			System.err.print(coefficients[j] + "x^ " + j);
			if(j<Degree && coefficients[j+1]>=0){
				System.err.print("+");}
		}
	    System.err.print("\n");
	}
	
	public final void getSolutions(final double c, Vector<Double> roots, final double EPS) {
		double[][] r = new double[4][2];
		Factor f = new Factor();
		int rCount=0;
		roots.clear();
		switch(Degree){
		case 1:
			rCount=f.Factor(coefficients[1],coefficients[0]-c,r,EPS);
			break;
		case 2:
			rCount=f.Factor(coefficients[2],coefficients[1],coefficients[0]-c,r,EPS);
			break;
		case 3:
			rCount=f.Factor(coefficients[3],coefficients[2],coefficients[1],coefficients[0]-c,r,EPS);
			break;
//		case 4:
//			rCount=Factor(coefficients[4],coefficients[3],coefficients[2],coefficients[1],coefficients[0]-c,r,EPS);
//			break;
		default:
			System.err.println("Can't solve polynomial of degree: " + Degree);
		}
		for(int i=0;i<rCount;i++){
			if((float)Math.abs(r[i][1])<=EPS){
				roots.add(r[i][0]);
	//printf("%d] %f\t%f\n",i,r[i][0],(*this)(r[i][0])-c);
			}
		}
	}
}
