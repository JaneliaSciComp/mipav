package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;

public class StartingPolynomial implements Comparator {
	
	public Polynomial p;
	public double start;
	public int Degree;
	
	public StartingPolynomial(int _Degree) {
		Degree = _Degree;
		p = new Polynomial(Degree);
	}
	
	//operator * 
	public final StartingPolynomial mul(final StartingPolynomial p) {
		StartingPolynomial sp = new StartingPolynomial(Degree+p.Degree);
		if(start>p.start){sp.start=start;}
		else{sp.start=p.start;}
		sp.p=this.p.mul(p.p);
		return sp;
	}

	public final StartingPolynomial scale(final double s) {
		StartingPolynomial q = new StartingPolynomial(Degree);
		q.start=start*s;
		q.p=p.scale(s);
		return q;
	}

	public final StartingPolynomial shift(final double s) {
		StartingPolynomial q = new StartingPolynomial(Degree);
		q.start=start+s;
		q.p=p.shift(s);
		return q;
	}


	//operator < 
	public final int LessThan(final StartingPolynomial sp) {
		if(start<sp.start){return 1;}
		else{return 0;}
	}
	
	public int compare(final Object v1, final Object v2){
		double d=((StartingPolynomial)(v1)).start-((StartingPolynomial)(v2)).start;
		if		(d<0)	{return -1;}
		else if	(d>0)	{return  1;}
		else			{return  0;}
	}
	
	
}