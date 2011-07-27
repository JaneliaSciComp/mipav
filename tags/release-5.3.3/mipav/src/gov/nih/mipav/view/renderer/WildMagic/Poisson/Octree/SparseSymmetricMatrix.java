package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;



public class SparseSymmetricMatrix extends SparseMatrix {

	public SparseSymmetricMatrix() {
		super();
	}
	
	// operator *
	public VectorF mul(VectorF V) {
		return Multiply(V);
	}

	public VectorD Multiply(final VectorD V) {
		VectorD R = new VectorD(this.rows);

		for (int i = 0; i < this.rows; i++) {
			for (int ii = 0; ii < this.rowSizes[i]; ii++) {
				int j = this.m_ppElements[i][ii].N;
				R.m_pV[i] += ((double)(this.m_ppElements[i][ii].Value) * V.m_pV[j]);
				R.m_pV[j] += ((double)(this.m_ppElements[i][ii].Value) * V.m_pV[i]);
			}
		}
		return R;
	}
	
	public VectorF Multiply(VectorF V) {
		VectorF R = new VectorF(this.rows);

		for (int i = 0; i < this.rows; i++) {
			for (int ii = 0; ii < this.rowSizes[i]; ii++) {
				int j = this.m_ppElements[i][ii].N;
				R.m_pV[i] += this.m_ppElements[i][ii].Value * V.m_pV[j];
				R.m_pV[j] += this.m_ppElements[i][ii].Value * V.m_pV[i];
			}
		}
		return R;
	}

	public final void Multiply(final VectorD In, VectorD Out) {
		Out.SetZero();
		for (int i = 0; i < this.rows; i++) {
			// MatrixEntry[] temp = this.m_ppElements[i];
			
			// float in1 = In.m_pV[i];
			// float out1 = Out.m_pV[i];
			int rs = this.rowSizes[i];
			for (int ii = 0; ii < rs; ii++) {
				MatrixEntry temp2 = this.m_ppElements[i][ii]; // temp[ii];
				int j = temp2.N;
				double v = temp2.Value;
				// System.err.println("ii = " + ii + " j = " + j + " v = " + v);
				Out.m_pV[i] += (v * In.m_pV[j]);
				Out.m_pV[j] += (v * In.m_pV[i]);
				// System.err.println("Out.m_pV[" + i + "] = " + Out.m_pV[i] + " Out.m_pV[" + j + "] = " + Out.m_pV[j]);
			}
		}
	}
	
	public final void Multiply(final VectorF In, VectorF Out) {
		Out.SetZero();
		for (int i = 0; i < this.rows; i++) {
			MatrixEntry[] temp = this.m_ppElements[i];
			
			// float in1 = In.m_pV[i];
			// float out1 = Out.m_pV[i];
			int rs = this.rowSizes[i];
			for (int ii = 0; ii < rs; ii++) {
				MatrixEntry temp2 = temp[ii];
				int j = temp2.N;
				float v = temp2.Value;
				// System.err.println("ii = " + ii + " j = " + j + " v = " + v);
				Out.m_pV[i] += v * In.m_pV[j];
				Out.m_pV[j] += v * In.m_pV[i];
				// System.err.println("Out.m_pV[" + i + "] = " + Out.m_pV[i] + " Out.m_pV[" + j + "] = " + Out.m_pV[j]);
			}
		}
	}

	public static int Solve(final SparseSymmetricMatrix M, final VectorD b, final int iters,
			VectorD solution, final double eps, final int reset) {
		VectorD d = new VectorD();
		VectorD r = new VectorD();
		VectorD Md = new VectorD();
		double alpha, beta, rDotR, bDotB;
		Md.Resize(b.Dimensions());
		if (reset == 1) {
			solution.Resize(b.Dimensions());
			solution.SetZero();
		}
		d = b.sub(M.Multiply(solution)); 
	    r = b.sub(M.Multiply(solution));
		// System.err.print("d: ");
		// d.print();
		// System.err.print("r: ");
		// r.print();
		rDotR = r.Dot(r);
		bDotB = (float)b.Dot(b);
		// System.err.println("rDotR = " + rDotR + " bDotB = " + bDotB);
		if (b.Dot(b) <= eps) {
			solution.SetZero();
			return 0;
		}
		
		int i;
		for (i = 0; i < iters; i++) {
			// System.err.println("iters = " + iters + " i = " + i);
			double temp;
			// Md.print();
			M.Multiply(d, Md);
			
			// System.err.print("d: ");
			// d.print();
			
			// System.err.print("Md: ");
			// Md.print();
			
			temp = d.Dot(Md);
			// System.err.println("temp = " + temp);
			if ((float)Math.abs(temp) <= eps) {
				break;
			}
			
			alpha = rDotR / temp;
			
			
			// System.err.println("before sub");
			// Md.print();
			
			// GVectorf gv = new GVectorf(Md.m_N, Md.m_pV);
			
			
			r.SubtractScaled(Md, alpha);
			
			// System.err.println("after sub");
			// Md.print();
			
			
			// System.err.println("rDotR = " + rDotR + " temp = " + temp + " alpha = " + alpha);
		    
		    
			temp = r.Dot(r);
			if (temp / bDotB <= eps) {
				break;
			}
			
			
			beta = temp / rDotR;
			 //  System.err.println("before: ");
			 //  solution.print();
			
			// d.print();
			solution.AddScaled(d, alpha);
			// if ( debug == true ) {
				// System.err.println("after: ");
				// d.print();
				// solution.print();
			// }
			// Octree.pause();
			
			
			if (beta <= eps) {
				break;
			}
			rDotR = temp;
			VectorD.Add(d, beta, r, d);
			
			// Octree.pause();
		}
		return i;
	}
	

	public static int Solve(final SparseSymmetricMatrix M, final VectorF diagonal, final VectorF b,
			final int iters, VectorF solution, final float eps, final int reset) {
		VectorF d = new VectorF();
		VectorF r = new VectorF();
		VectorF Md = new VectorF();

		if (reset == 1) {
			solution.Resize(b.Dimensions());
			solution.SetZero();
		}
		Md.Resize(M.rows);
		for (int i = 0; i < iters; i++) {
			M.Multiply(solution, Md);
			r = b.sub(Md);
			for (int j = 0; j < (int) (M.rows); j++) {
				solution.set(j, solution.get(j) + r.get(j) / diagonal.get(j));
			}
		}
		return iters;
	}

	public static int Solve(final SparseSymmetricMatrix M, final VectorD b, final int iters,
			VectorD solution, final double eps, final int reset, boolean debug) {
		VectorD d = new VectorD();
		VectorD r = new VectorD();
		VectorD Md = new VectorD();
		double alpha, beta, rDotR, bDotB;
		Md.Resize(b.Dimensions());
		if (reset == 1) {
			solution.Resize(b.Dimensions());
			solution.SetZero();
		}
		d = b.sub(M.Multiply(solution)); 
	    r = b.sub(M.Multiply(solution));
	    /*
		if ( debug ) {
		    System.err.print("d: ");
			d.print();
			Octree.pause();
			System.err.print("r: ");
			r.print();
			Octree.pause();
		}
		*/
		rDotR = r.Dot(r);
		bDotB = b.Dot(b);
		/*
		if ( debug ) {
			System.err.println("rDotR = " + rDotR + " bDotB = " + bDotB);
			Octree.pause();
		}
		*/
		if (b.Dot(b) <= eps) {
			solution.SetZero();
			return 0;
		}
		
		int i;
		for (i = 0; i < iters; i++) {
			// System.err.println("iters = " + iters + " i = " + i);
			double temp;
			/*
			if ( debug ) {
			   System.err.println("M:");
			   M.print();
			   Octree.pause();
				
			   System.err.println("Md:");
			   Md.print();
			   Octree.pause();
			}
			*/
			M.Multiply(d, Md);
			/*
			if ( debug ) {
			    System.err.print("d: ");
			    d.print();
			    Octree.pause();
			
			    System.err.print("Md: ");
			    Md.print();
			    Octree.pause();
			}			
			*/
			temp = d.Dot(Md);
			// System.err.println("temp = " + temp);
			if (Math.abs(temp) <= eps) {
				break;
			}
			
			alpha = rDotR / temp;
			
			
			// System.err.println("before sub");
			// Md.print();
			
			// GVectorf gv = new GVectorf(Md.m_N, Md.m_pV);
			
			
			r.SubtractScaled(Md, alpha);
			
			// System.err.println("after sub");
			// Md.print();
			
			
			// System.err.println("rDotR = " + rDotR + " temp = " + temp + " alpha = " + alpha);
		    
		    
			temp = r.Dot(r);
			if (temp / bDotB <= eps) {
				break;
			}
			
			
			beta = temp / rDotR;
		    /*
			if ( debug == true ) {
			   System.err.println("before: alpha = " + alpha);
			   solution.print();
			   // d.print();
			   System.err.println("before: alpha = " + alpha);
			}
			Octree.pause();
			*/
			solution.AddScaled(d, alpha);
			/*
			if ( debug == true ) {
				System.err.println("after: alpha = " + alpha);
				solution.print();
				// solution.print();
				System.err.println("after: alpha = " + alpha);
			}
			Octree.pause();
			*/
			
			if (beta <= eps) {
				break;
			}
			rDotR = temp;
			VectorD.Add(d, beta, r, d);
			
			// Octree.pause();
		}
		return i;
	}
	
}