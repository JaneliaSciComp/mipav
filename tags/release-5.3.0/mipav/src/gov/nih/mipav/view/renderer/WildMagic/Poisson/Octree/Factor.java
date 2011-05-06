package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class Factor {
	
    public static final float PI = 3.1415926535897932384f;
	public static final float SQRT_3  = 1.7320508075688772935f;

	// roots[1][2]
	public int Factor(double a1,double a0,double roots[][], final double EPS){
		if((float)Math.abs(a1)<=EPS){return 0;}
		roots[0][0]=-a0/a1;
		roots[0][1]=0;
		return 1;
	}
	
	// roots[2][2]
	public int Factor(double a2,double a1,double a0,double roots[][], final double EPS){
		double d;
		if((float)Math.abs(a2)<=EPS){return Factor(a1,a0,roots,EPS);}

		d=a1*a1-4*a0*a2;
		a1/=(2*a2);
		if(d<0){
			d=Math.sqrt(-d)/(2*a2);
			roots[0][0]=roots[1][0]=-a1;
			roots[0][1]=-d;
			roots[1][1]= d;
		}
		else{
			d=Math.sqrt(d)/(2*a2);
			roots[0][1]=roots[1][1]=0;
			roots[0][0]=-a1-d;
			roots[1][0]=-a1+d;
		}
		return 2;
	}
	// Solution taken from: http://mathworld.wolfram.com/CubicFormula.html
	// and http://www.csit.fsu.edu/~burkardt/f_src/subpak/subpak.f90
	public int Factor(double a3,double a2,double a1,double a0,double roots[][], final double EPS){
		double q,r,r2,q3;

		if(Math.abs(a3)<=EPS){return Factor(a2,a1,a0,roots,EPS);}
		a2/=a3;
		a1/=a3;
		a0/=a3;

		q=-(3*a1-a2*a2)/9;
		r=-(9*a2*a1-27*a0-2*a2*a2*a2)/54;
		r2=r*r;
		q3=q*q*q;

		if(r2<q3){
			double sqrQ=Math.sqrt(q);
			double theta = Math.acos ( r / (sqrQ*q) );
			double cTheta=Math.cos(theta/3)*sqrQ;
			double sTheta=Math.sin(theta/3)*sqrQ*SQRT_3/2;
			roots[0][1]=roots[1][1]=roots[2][1]=0;
			roots[0][0]=-2*cTheta;
			roots[1][0]=-2*(-cTheta*0.5-sTheta);
			roots[2][0]=-2*(-cTheta*0.5+sTheta);
		}
		else{
			double s1,s2,sqr=Math.sqrt(r2-q3);
			double t;
			t=-r+sqr;
			if(t<0){s1=-Math.pow(-t,1.0/3);}
			else{s1=Math.pow(t,1.0/3);}
			t=-r-sqr;
			if(t<0){s2=-Math.pow(-t,1.0/3);}
			else{s2=Math.pow(t,1.0/3);}
			roots[0][1]=0;
			roots[0][0]=s1+s2;
			s1/=2;
			s2/=2;
			roots[1][0]= roots[2][0]=-s1-s2;
			roots[1][1]= SQRT_3*(s1-s2);
			roots[2][1]=-roots[1][1];
		}
		roots[0][0]-=a2/3;
		roots[1][0]-=a2/3;
		roots[2][0]-=a2/3;
		return 3;
	}
	
	public double ArcTan2(final double y, final double x){
		/* This first case should never happen */
		if(y==0 && x==0){return 0;}
		if(x==0){
			if(y>0){return PI/2.0;}
			else{return -PI/2.0;}
		}
		if(x>=0){return Math.atan(y/x);}
		else{
			if(y>=0){return Math.atan(y/x)+PI;}
			else{return Math.atan(y/x)-PI;}
		}
	}
	
	// double in[2]
	public double Angle(final double in[]){
		if((in[0]*in[0]+in[1]*in[1])==0.0){return 0;}
		else{return ArcTan2(in[1],in[0]);}
	}
	
	// double in[2], double out[2]
	public void Sqrt(final double in[], final double out[]){
		double r=Math.sqrt(Math.sqrt(in[0]*in[0]+in[1]*in[1]));
		double a=Angle(in)*0.5;
		out[0]=r*Math.cos(a);
		out[1]=r*Math.sin(a);
	}
	
	// double in1[2],double in2[2], out[2]
	public void Add(final double in1[], final double in2[], double out[]){
		out[0]=in1[0]+in2[0];
		out[1]=in1[1]+in2[1];
	}
	
	// double in1[2], double in2[2], out[2]
	public void Subtract(final double in1[], final double in2[], double out[]){
		out[0]=in1[0]-in2[0];
		out[1]=in1[1]-in2[1];
	}
	
	// double in1[2], double in2[2], out[2]
	public void Multiply(final double in1[], final double in2[], double out[]){
		out[0]=in1[0]*in2[0]-in1[1]*in2[1];
		out[1]=in1[0]*in2[1]+in1[1]*in2[0];
	}
	
	// double in1[2], double in2[2], out[2]
	public void Divide(final double in1[], final double in2[], double out[]){
		double[] temp = new double[2];
		double l=in2[0]*in2[0]+in2[1]*in2[1];
		temp[0]= in2[0]/l;
		temp[1]=-in2[1]/l;
		Multiply(in1,temp,out);
	}
	// Solution taken from: http://mathworld.wolfram.com/QuarticEquation.html
	// and http://www.csit.fsu.edu/~burkardt/f_src/subpak/subpak.f90
	// double roots[4][2]
	public int Factor(double a4,double a3,double a2,double a1,double a0, double roots[][], final double EPS){
		double[] R = new double[2];
		double[] D = new double[2];
		double[] E = new double[2];
		double[] R2 = new double[2];

		if((float)Math.abs(a4)<EPS){return Factor(a3,a2,a1,a0,roots,EPS);}
		a3/=a4;
		a2/=a4;
		a1/=a4;
		a0/=a4;

		Factor(1.0,-a2,a3*a1-4.0*a0,-a3*a3*a0+4.0*a2*a0-a1*a1,roots,EPS);

		R2[0]=a3*a3/4.0-a2+roots[0][0];
		R2[1]=0;
		Sqrt(R2,R);
		if((float)Math.abs(R[0])>10e-8){
			double[] temp1 = new double[2];
			double[] temp2 = new double[2];
			double[] p1 = new double[2];
			double[] p2 = new double[2];

			p1[0]=a3*a3*0.75-2.0*a2-R2[0];
			p1[1]=0;

			temp2[0]=((4.0*a3*a2-8.0*a1-a3*a3*a3)/4.0);
			temp2[1]=0;
			Divide(temp2,R,p2);

			Add     (p1,p2,temp1);
			Subtract(p1,p2,temp2);

			Sqrt(temp1,D);
			Sqrt(temp2,E);
		}
		else{
			R[0]=R[1]=0;
			double[] temp1= new double[2];
			double[] temp2 = new double[2];
			temp1[0]=roots[0][0]*roots[0][0]-4.0*a0;
			temp1[1]=0;
			Sqrt(temp1,temp2);
			temp1[0]=a3*a3*0.75-2.0*a2+2.0*temp2[0];
			temp1[1]=                  2.0*temp2[1];
			Sqrt(temp1,D);
			temp1[0]=a3*a3*0.75-2.0*a2-2.0*temp2[0];
			temp1[1]=                 -2.0*temp2[1];
			Sqrt(temp1,E);
		}

		roots[0][0]=-a3/4.0+R[0]/2.0+D[0]/2.0;
		roots[0][1]=        R[1]/2.0+D[1]/2.0;

		roots[1][0]=-a3/4.0+R[0]/2.0-D[0]/2.0;
		roots[1][1]=        R[1]/2.0-D[1]/2.0;

		roots[2][0]=-a3/4.0-R[0]/2.0+E[0]/2.0;
		roots[2][1]=       -R[1]/2.0+E[1]/2.0;

		roots[3][0]=-a3/4.0-R[0]/2.0-E[0]/2.0;
		roots[3][1]=       -R[1]/2.0-E[1]/2.0;
		return 4;
	}

	public int Solve(final double[] eqns, final double[] values,double[] solutions, final int dim){
		int i,j,eIndex;
		double v,m;
		int[] index=new int[dim];
		int[] set=new int[dim];
		double[] myEqns=new double[dim*dim];
		double[] myValues=new double[dim];

		for(i=0;i<dim*dim;i++){myEqns[i]=eqns[i];}
		for(i=0;i<dim;i++){
			myValues[i]=values[i];
			set[i]=0;
		}
		for(i=0;i<dim;i++){
			// Find the largest equation that has a non-zero entry in the i-th index
			m=-1;
			eIndex=-1;
			for(j=0;j<dim;j++){
				if(set[j] == 1){continue;}
				if(myEqns[j*dim+i]!=0 && (float)Math.abs(myEqns[j*dim+i])>m){
					m=(float)Math.abs(myEqns[j*dim+i]);
					eIndex=j;
				}
			}
			if(eIndex==-1){
				index = null;
				myValues = null;
				myEqns = null;
				set = null;
				return 0;
			}
			// The position in which the solution for the i-th variable can be found
			index[i]=eIndex;
			set[eIndex]=1;

			// Normalize the equation
			v=myEqns[eIndex*dim+i];
			for(j=0;j<dim;j++){myEqns[eIndex*dim+j]/=v;}
			myValues[eIndex]/=v;

			// Subtract it off from everything else
			for(j=0;j<dim;j++){
				if(j==eIndex){continue;}
				double vv=myEqns[j*dim+i];
				for(int k=0;k<dim;k++){myEqns[j*dim+k]-=myEqns[eIndex*dim+k]*vv;}
				myValues[j]-=myValues[eIndex]*vv;
			}
		}
		for(i=0;i<dim;i++){solutions[i]=myValues[index[i]];}
		index = null;
		myValues = null;
		myEqns = null;
		set = null;
		return 1;
	}

	
	
	
}