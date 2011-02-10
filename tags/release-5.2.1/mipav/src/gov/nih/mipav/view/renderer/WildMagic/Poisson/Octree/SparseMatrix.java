package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class SparseMatrix {

	public static Allocator<MatrixEntry> Allocator = new Allocator<MatrixEntry>();

	public int rows;
	public int[] rowSizes;
	public MatrixEntry[][] m_ppElements;

	public static int UseAlloc = 0;

	public static int UseAllocator() {
		return UseAlloc;
	}

	public static void SetAllocator(int blockSize) {
		if (blockSize > 0) {
			UseAlloc = 1;
			Allocator.set(blockSize);
		} else {
			UseAlloc = 0;
		}
	}

	public SparseMatrix() {
		rows = 0;
		rowSizes = null;
		m_ppElements = null;
	}

	public SparseMatrix(int rows) {
		Resize(rows);
	}

	public SparseMatrix(SparseMatrix M) {
		Resize(M.rows);
		for (int i = 0; i < rows; i++) {
			SetRowSize(i, M.rowSizes[i]);
			for (int j = 0; j < rowSizes[i]; j++) {
				m_ppElements[i][j] = M.m_ppElements[i][j];
			}
		}
	}

	public int Entries() {
		int e = 0;
		for (int i = 0; i < rows; i++) {
			e += (int) (rowSizes[i]);
		}
		return e;
	}

	// operator =
	public SparseMatrix set(SparseMatrix M) {
		Resize(M.rows);
		for (int i = 0; i < rows; i++) {
			SetRowSize(i, M.rowSizes[i]);
			for (int j = 0; j < rowSizes[i]; j++) {
				m_ppElements[i][j] = M.m_ppElements[i][j];
			}
		}
		return this;
	}

	public void dispose() {
		Resize(0);
	}

	public void Resize(int r) {
		int i, j;
		if (rows > 0) {
			if (UseAlloc == 0) {
				for (i = 0; i < rows; i++) {
					if (rowSizes[i] != 0) {
						m_ppElements[i] = null;
					}
				}
			}
			m_ppElements = null;
			rowSizes = null;
		}
		rows = r;
		if (r != 0 ) {
			rowSizes = new int[r];
			// memset(rowSizes,0,sizeof(int)*r);
			// m_ppElements = (MatrixEntry[][]) (new Object[r][r]);
			// m_ppElements = new MatrixEntry[r];
			m_ppElements = new MatrixEntry[r][];
			/*
			for ( i = 0; i < r; i++ )
				// for ( j = 0; j < r; j++) 
					m_ppElements[i] = new MatrixEntry[];
			*/
		}
	}
	
	public void SetRowSize(int row,int count){
		int i;
		if(row>=0 && row<rows){
			if(UseAlloc == 1 ){
				// m_ppElements[row]=Allocator.newElements(count);
				// m_ppElements[row]= (MatrixEntry[])new Object[count];
				m_ppElements[row] = new MatrixEntry[count];
				for ( i = 0; i < count; i++ )
					m_ppElements[row][i] = new MatrixEntry();
				
			} else{
				if(rowSizes[row] != 0 ){ m_ppElements[row] = null;}
				if(count>0) {
					// m_ppElements[row]= (MatrixEntry[])new Object[count];
					m_ppElements[row] = new MatrixEntry[count];
					for ( i = 0; i < count; i++ )
						m_ppElements[row][i] = new MatrixEntry();
				}
			}
			rowSizes[row]=count;
		}
	}
	
	public void SetZero()
	{
		// Resize(this.m_N, this.m_M);
		for (int i = 0; i < rows; i++) {
			// SetRowSize(i, M.rowSizes[i]);
			for (int j = 0; j < rowSizes[i]; j++) {
				m_ppElements[i][j].Value = 0f;
			}
		}
	}
	
	public void print() {
		for (int i = 0; i < rows; i++) {
			for (int j = 0; j < rowSizes[i]; j++) {
				System.err.print("  [" + i + "][" + j + "].Value = " + m_ppElements[i][j].Value);			
			}
			System.err.println();
			Octree.pause();
			
		}
	}
	

	public int Rows() {
		return rows;
	}
	
	public int Columns() {
		return m_ppElements[0].length;
	}
	
	public void SetIdentity()
	{
		SetZero();
		for(int ij=0; ij < Math.min( this.Rows(), this.Columns() ); ij++) {
			// (*this)(ij,ij) = 1;
			m_ppElements[ij][ij].Value = 1f;
		}
	}

	// operator * 
	public SparseMatrix mul( float V)
	{
		SparseMatrix M = new SparseMatrix(this);
		M.mul_into(V);
		return M;
	}
	
	// operator *= 
	public SparseMatrix mul_into(float V)
	{
		for (int i=0; i<this.Rows(); i++)
		{
			for(int ii=0;ii<m_ppElements[i].length ;i++) {
				m_ppElements[i][ii].Value*=V;
			}
		}
		return this;
	}
	
	public SparseMatrix Multiply( SparseMatrix M )
	{
		SparseMatrix R = new SparseMatrix( M.Columns() );
		for(int i=0; i<R.Rows(); i++){
			for(int ii=0;ii<m_ppElements[i].length;ii++){
				int N=m_ppElements[i][ii].N;
				float Value=m_ppElements[i][ii].Value;
				for(int jj=0;jj<M.m_ppElements[N].length;jj++){
					R.m_ppElements[i][M.m_ppElements[N][jj].N].Value += Value * M.m_ppElements[N][jj].Value;
				}
			}
		}
		return R;		
	}
	
	public VectorF Multiply(VectorF V) {
		VectorF R = new VectorF(rows);

		for (int i = 0; i < rows; i++) {
			float temp = 0;
			for (int ii = 0; ii < rowSizes[i]; ii++) {
				temp += m_ppElements[i][ii].Value
						* V.m_pV[m_ppElements[i][ii].N];
			}
			R.set(i, temp);
		}
		return R;
	}

	public void Multiply(VectorF In, VectorF Out) {
		for (int i = 0; i < rows; i++) {
			float temp = 0;
			for (int j = 0; j < rowSizes[i]; j++) {
				temp += (m_ppElements[i][j].Value * In.m_pV[m_ppElements[i][j].N]);
			}
			Out.m_pV[i] = temp;
		}
	}
	
	// operator * 
	public SparseMatrix mul(SparseMatrix M)
	{
		return Multiply(M);
	}

	// operator * 
	public VectorF mul(VectorF V)
	{
		return Multiply(V);
	}
	
	public SparseMatrix Transpose()
	{
		SparseMatrix M = new SparseMatrix(  Rows() );

		for (int i=0; i<Rows(); i++)
		{
			for(int ii=0;ii<m_ppElements[i].length;ii++){
				M.m_ppElements[m_ppElements[i][ii].N][i].Value = m_ppElements[i][ii].Value;
			}
		}
		return M;
	}

	public static int SolveSymmetric(SparseMatrix M, VectorF b,int iters,VectorF solution,float eps,int reset){
		VectorF d = new VectorF();
		VectorF r = new VectorF();
		VectorF Md = new VectorF();
		float alpha,beta,rDotR;
		Md.Resize(b.Dimensions());
		if(reset == 1){
			solution.Resize(b.Dimensions());
			solution.SetZero();
		}
		d=r=b.sub(M.Multiply(solution));
		rDotR=r.Dot(r);
		if(b.Dot(b)<=eps){
			solution.SetZero();
			return 0;
		}

		int i;
		for(i=0;i<iters;i++){
			float temp;
			M.Multiply(d,Md);
			temp=d.Dot(Md);
			if(temp<=eps){break;}
			alpha=rDotR/temp;
			r.SubtractScaled(Md,alpha);
			temp=r.Dot(r);
			if(temp/b.Dot(b)<=eps){break;}
			beta=temp/rDotR;
			solution.AddScaled(d,alpha);
			if(beta<=eps){break;}
			rDotR=temp;
			VectorF.Add(d,beta,r,d);
		}
		return i;
	}
	
	// Solve for x s.t. M(x)=b by solving for x s.t. M^tM(x)=M^t(b)
 	public static int Solve(SparseMatrix M, VectorF b,int iters,VectorF solution, float eps){
		SparseMatrix mTranspose=M.Transpose();
		VectorF bb=mTranspose.mul(b);
		VectorF d = new VectorF();
		VectorF r = new VectorF();
		VectorF Md = new VectorF();
		float alpha,beta,rDotR;
		int i;

		solution.Resize(M.Columns());
		solution.SetZero();

		d=r=bb;
		rDotR=r.Dot(r);

		for(i=0;i<iters && rDotR>eps;i++){
			float temp;
			Md=mTranspose.mul(M.mul(d));
			alpha=rDotR/d.Dot(Md);
			solution.add_into(d.mul(alpha));
			r.sub_into(Md.mul(alpha));
			temp=r.Dot(r);
			beta=temp/rDotR;
			rDotR=temp;
			d=r.add(d.mul(beta));
		}
		return i;
	}
	
	
}


