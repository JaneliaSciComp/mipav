package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class VectorD{
	
	double[] m_pV;
	int m_N;
	
	public VectorD()
	{
		m_N = 0;
		m_pV = null;
	}
	
	public VectorD( VectorD V )
	{
		m_N = 0;
		m_pV = null;
	    Resize(V.m_N);
		// memcpy( m_pV, V.m_pV, m_N*sizeof(T) );
	    for ( int i = 0; i < V.m_N; i++ ) {
	    	m_pV[i] = V.m_pV[i];
	    }
	}
	
	public VectorD( int N )
	{
		m_N=0;
		m_pV=null;
		Resize(N);
	}
	
	public void print() {
		for ( int i = 0; i < m_N; i++ ) {
			System.err.print("  m_pV[" + i + "]= " + m_pV[i]);
			if ( i % 5 == 0 )
				System.err.println();
		}
		// Octree.pause();
		System.err.println();
	}
		
	public void Resize( int N )
	{
		if(m_N!=N){
			if(m_N == 0 ){m_pV = null;}
			m_pV=null;
			m_N = N;
			if( N != 0 ){
				m_pV = new double[N];
			}
		}
		// memset( m_pV, 0, N*sizeof(T) );
	}
	
	public VectorD( int N, double[] pV )
	{
		Resize(N);
		// memcpy( m_pV, pV, N*sizeof(T) );
		for ( int i = 0; i < pV.length; i++ ) {
	    	m_pV[i] = pV[i];
	    }
	}
	
	public void dispose() {
		Resize(0);
	}
	
	// operator = 
	public VectorD set(VectorD V) {
		Resize(V.m_N);
		for ( int i = 0; i < V.m_N; i++ ) {
	    	m_pV[i] = V.m_pV[i];
	    }
		return this;
	}
	
	// operator (), operator []
	public double get(int i)
	{
		assert( i < m_N );
		return m_pV[i];
	}
	
	public void set(int i, double value) {
		m_pV[i] = value;
	}
	
	
	public int Dimensions() {
		return m_N;
	}
	
	public void SetZero() {
		for (int i=0; i<m_N; i++){
			m_pV[i] = (double)0;
		}
		
	}
	
	// operator * 
	public VectorD mul(double A)
	{
		VectorD V = new VectorD(this);
		for (int i=0; i<m_N; i++)
			 V.m_pV[i] *= A;
		return V;
	}
	
	// operator *=
	public VectorD mul_into(double A)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] *= A;
		return this;
	}
	
	// operator / 
	public VectorD div(double A)
	{
		VectorD V = new VectorD(this);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] /= A;
		return V;
	}

	// operator /= 
	public VectorD div_into(double A)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] /= A;
		return this;
	}
	
	// operator + 
	public VectorD add(VectorD V0)
	{
		VectorD V = new VectorD(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = m_pV[i] + V0.m_pV[i];

		return V;
	}
	
	// operator +=
	public VectorD add_into(VectorD V)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] += V.m_pV[i];

		return this;
	}
	
	// operator - 
	public VectorD sub(VectorD V0)
	{
		VectorD V = new VectorD(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = m_pV[i] - V0.m_pV[i];

		return V;
	}
	
	
	// operator -=
	public VectorD sub_into(VectorD V)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] -= V.m_pV[i];

		return this;
	}
	
	// operator - 
	public VectorD neg()
	{
		VectorD V = new VectorD(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = -m_pV[i];
		return V;
	}
	
	public double Norm( int Ln ) 
	{
		double N = 0f;
		for (int i = 0; i<m_N; i++)
			N += Math.pow(m_pV[i], (double)Ln);
		return Math.pow(N, (double)1.0f/Ln);	
	}
	
	public void Normalize()
	{
		double N = 1.0f/Norm(2);
		for (int i = 0; i<m_N; i++)
			m_pV[i] *= N;
	}
	
	public double Length()
	{
		double N = 0;
		for (int i = 0; i<m_N; i++)
			N += m_pV[i]*m_pV[i];
		return Math.sqrt(N);	
	}
	
	public VectorD AddScaled(final VectorD V, final double scale)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] += V.m_pV[i]*scale;

		return this;
	}

	
	public double Dot(final VectorD V )
	{
		double V0 = 0f;
		for (int i=0; i<m_N; i++)
			V0 += m_pV[i]*V.m_pV[i];

		return V0;
	}
	
	public VectorD SubtractScaled(final VectorD V, final double scale)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] -= V.m_pV[i]*scale;

		return this;
	}
	
	public void Add(VectorD V1,double scale1, VectorD V2, double scale2, VectorD Out){
		for (int i=0; i<V1.m_N; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i]*scale2;
	}

	public static void Add(final VectorD V1, final double scale1, final VectorD V2, VectorD Out){
		for (int i=0; i<V1.m_N; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i];
	}

	
}