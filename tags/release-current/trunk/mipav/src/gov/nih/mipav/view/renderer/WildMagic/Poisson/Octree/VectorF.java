package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;



public class VectorF{
	
	float[] m_pV;
	int m_N;
	
	public VectorF()
	{
		m_N = 0;
		m_pV = null;
	}

	
	public VectorF( VectorF V )
	{
		m_N = 0;
		m_pV = null;
	    Resize(V.m_N);
		// memcpy( m_pV, V.m_pV, m_N*sizeof(T) );
	    for ( int i = 0; i < V.m_N; i++ ) {
	    	m_pV[i] = V.m_pV[i];
	    }
	}
	
	public VectorF( int N )
	{
		m_N=0;
		m_pV=null;
		Resize(N);
	}

	
	
	public void print() {
		for ( int i = 0; i < m_N; i++ ) {
			System.err.print("  m_pV[" + i + "]= " + m_pV[i]);
			if ( i % 5 == 0 ) System.err.println();
		}
		System.err.println();
	}
		
	public void Resize( int N )
	{
		if(m_N!=N){
			if(m_N != 0 ){m_pV = null;}
			m_pV=null;
			m_N = N;
			if( N != 0 ){
				m_pV = new float[N];
			}
		}
		// memset( m_pV, 0, N*sizeof(T) );
	}
	
	public VectorF( int N, float[] pV )
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
	public VectorF set(VectorF V) {
		Resize(V.m_N);
		for ( int i = 0; i < V.m_N; i++ ) {
	    	m_pV[i] = V.m_pV[i];
	    }
		return this;
	}
	
	// operator (), operator []
	public float get(int i)
	{
		assert( i < m_N );
		return m_pV[i];
	}
	
	public void set(int i, float value) {
		m_pV[i] = value;
	}
	
	public int Dimensions() {
		return m_N;
	}
	
	public void SetZero() {
		for (int i=0; i<m_N; i++){
			m_pV[i] = (float)0;
		}
		
	}
	
	// operator * 
	public VectorF mul(float A)
	{
		VectorF V = new VectorF(this);
		for (int i=0; i<m_N; i++)
			 V.m_pV[i] *= A;
		return V;
	}
	
	// operator *=
	public VectorF mul_into(float A)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] *= A;
		return this;
	}
	
	// operator / 
	public VectorF div(float A)
	{
		VectorF V = new VectorF(this);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] /= A;
		return V;
	}

	// operator /= 
	public VectorF div_into(float A)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] /= A;
		return this;
	}
	
	// operator + 
	public VectorF add(VectorF V0)
	{
		VectorF V = new VectorF(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = m_pV[i] + V0.m_pV[i];

		return V;
	}
	
	// operator +=
	public VectorF add_into(VectorF V)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] += V.m_pV[i];

		return this;
	}
	
	// operator - 
	public VectorF sub(VectorF V0)
	{
		VectorF V = new VectorF(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = m_pV[i] - V0.m_pV[i];

		return V;
	}
	
	// operator -=
	public VectorF sub_into(VectorF V)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] -= V.m_pV[i];

		return this;
	}
	
	// operator - 
	public VectorF neg()
	{
		VectorF V = new VectorF(m_N);
		for (int i=0; i<m_N; i++)
			V.m_pV[i] = -m_pV[i];
		return V;
	}
	
	public float Norm( int Ln ) 
	{
		float N = 0f;
		for (int i = 0; i<m_N; i++)
			N += (float)Math.pow(m_pV[i], (float)Ln);
		return (float)Math.pow(N, (float)1.0f/Ln);	
	}
	
	public void Normalize()
	{
		float N = 1.0f/Norm(2);
		for (int i = 0; i<m_N; i++)
			m_pV[i] *= N;
	}
	
	public float Length()
	{
		float N = 0;
		for (int i = 0; i<m_N; i++)
			N += m_pV[i]*m_pV[i];
		return (float)Math.sqrt(N);	
	}
	
	public VectorF AddScaled(final VectorF V, final float scale)
	{
		for (int i=0; i<m_N; i++)
			m_pV[i] += V.m_pV[i]*scale;

		return this;
	}
	
	public final float Dot(final VectorF V )
	{
		float V0 = 0f;
		for (int i=0; i<m_N; i++)
			V0 += m_pV[i]*V.m_pV[i];

		return V0;
	}
	
	public VectorF SubtractScaled(final VectorF V, final float scale)
	{
		for (int i=0; i<m_N; i++) {
			m_pV[i] -= V.m_pV[i]*scale;
			// System.err.println("m_pV["+i+"]=" + m_pV[i] + " V.m_pV[" + i + "] = " + V.m_pV[i]);
		}

		return this;
	}
	
	
	public void Add(VectorF V1,float scale1, VectorF V2, float scale2, VectorF Out){
		for (int i=0; i<V1.m_N; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i]*scale2;
	}

	public static void Add(final VectorF V1, final float scale1, final VectorF V2, VectorF Out){
		for (int i=0; i<V1.m_N; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i];
	}
	
}