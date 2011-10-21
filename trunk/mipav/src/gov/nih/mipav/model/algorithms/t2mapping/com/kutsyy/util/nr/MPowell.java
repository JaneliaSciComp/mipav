package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

class MPowell extends NumericBase
{
	// We're not using _lsb here
	Brent linefunction = new Brent();

	MPowell() 
	{ 
	}

	//virtual 
	public int minimize(
			//OBJECTIVE_FUNCTION function,
			//LINE_MINIMIZER_FUNCTION linefunction,
			double[] initialSolution,
			VectorValue finalSolution) 
	{

		if(_verbose>=1) {
			System.out.println("Powell::minimize(): tol="+_tol+" maxiter="+_maxiter+" BEGIN");
		}
		//finalSolution=initialSolution;

		//OBJECTIVE_FUNCTION function=objectiveFunction();
		ObjectiveFunction function = getObjFunction();

		//linefunction->setObjectiveFunction(function);
		linefunction.setObjFunction(function);
		linefunction.setVerbose(getVerbose()-1);
		linefunction.setTolerance(getTolerance());

		int n = initialSolution.length;
		
		// create n initial unit vectors from the axis in U

	// +++ Ideally powell.java should have access to this through an accessor
		double[][] U = Matrix.eye(n);
		if (_verbose>=4) System.out.println(Matrix.toString(U));

		int itermax=_maxiter; //1;
		double[] viterStart=Vect.clone(initialSolution);
		double fiterStart=function.eval(initialSolution); 

		int iter=0;
		int ret=ERROR_MAXIMUM_ITERATIONS_REACHED;

		while (iter<itermax) {
			
			if (_verbose>=2) {
				System.out.println("\n--------------------------------------------------------");
				System.out.println("Powell::iter "+iter);
			}

			// do a line-minimization along each direction in U
			double[][] vPoints = Matrix.alloc(U);
			double[] fPoints = new double[n];
			double[] v = Vect.clone(viterStart);
			double  f=fiterStart;
			VectorValue vv1 = new VectorValue(n);
			VectorValue vv2 = new VectorValue(n);

			int iBest=-1; // -1 means not initialized
			double fdelBest=0.0; // best decrease so far
			for (int i=0; i<n; i++) {
				vv1.v = v; vv1.f = f;
				vv2.v = vPoints[i]; vv2.f = fPoints[i];
				int stat = linefunction.findMin(vv1,U[i],vv2);
				fPoints[i] = vv2.f;

			if (stat != SUCCESS) {
				if (_verbose>=2) System.out.println(toString(stat));
				if (stat != ERROR_MAXIMUM_ITERATIONS_REACHED) return stat;
			}

				double fdel = fPoints[i]-f;
				if (iBest==-1 || fdel<fdelBest) {
					iBest=i;
					fdelBest=fdel;
				}
				Vect.copy(v,vPoints[i]);
				f=fPoints[i];
			}
			
			if (Math.abs(fiterStart-f)<_tol) {
				if (_verbose>=2) System.out.println("Powell:: early termination - no improvement in error over last iteration.");
				iter++;						
				ret=SUCCESS;
				break;
			}

			if (iter+1<itermax) { // if not last iteration
				if (_verbose>=2) System.out.println("Powell:: looking at average distance moved, extrapolating.");
				double[] vchange = Vect.sub(v, viterStart);

				double f0=fiterStart;
				double fn=f;
				double[] vtemp = Vect.add(v, vchange);
				double fe=function.eval(vtemp);
				if (_verbose>=4) System.out.println("fe=" +fe);
				if (fe>=f0) {
					if (_verbose>=2) {
						System.out.println("Keep set of old directions, since the average direction\nis all played out.");
					}
				}
				else {
					double fdelmag=Math.abs(fdelBest);
					//printf("fdelmag=%f\n",fdelmag);
					double a = (f0-fn-fdelmag);
					double b = (f0-fe);
					//{
					//	double k0=2.0*(f0-2.0*fn+fe)*a*a; printf("k0=%f\n",k0);
					//	double k1=b*b*fdelmag; printf("k1=%f\n",k1);
					//}
					if (2.0*(f0-2.0*fn+fe)*a*a >= b*b*fdelmag) {
						if (_verbose>=2) {
							System.out.println(
							"Keep old set, since either the decrease was not primarily\n" +
							"due to a single direction's decrease, or there is a large\n" +
							"curvature along the average direction and so we appear to\n" +
							"be near the bottom of the error surface.");
						}
					}
					else {
						if (_verbose>=3) 
						{
							System.out.println("Powell: Replacing direction " + 
												iBest + " by " + Vect.toString(vchange));
						}
						vchange=Vect.normalize(vchange);
						U[iBest]=vchange;
			
						if (iBest!=0) { // put the new vector in the first position
							// fvector vtemp=U[iBest]; 
							// U[iBest]=U[0]; 
							// U[0]=vtemp;

							vtemp = Vect.clone(U[iBest]);
							Vect.copy(U[iBest], U[0]);
							Vect.copy(U[0], vtemp);
						}
					}
				}
			}

			//viterStart=v;
			Vect.copy(viterStart, v);
			fiterStart=f;		

			iter++;

		}	

		//finalSolution=viterStart;
		Vect.copy(finalSolution.v, viterStart);
		finalSolution.f=fiterStart;		
		if (_verbose>=1) {
			System.out.println("Powell:: finalSolution after " + iter + " iterations:");
			System.out.println(finalSolution.f + " : " + Vect.toString(finalSolution.v));
		}

		return ret;
	}
}
