package gov.nih.mipav.view.renderer.WildMagic.Poisson;

public class NVectorF {
	
	int m_N;
	int Dim;
	float[] m_pV;
	
	public NVectorF()
	{
		m_N = 0;
		m_pV = null;
	}
	
	public NVectorF( int _Dim, NVectorF V )
	{
		m_N = 0;
		m_pV = null;
		Dim = _Dim;
		Resize(V.m_N);
		// memcpy( m_pV, V.m_pV, m_N*sizeof(T)*Dim );
		for ( int i = 0; i < V.m_N; i++ ) {
	    	m_pV[i] = V.m_pV[i];
	    }
	}
	
	public NVectorF(int _Dim, int N )
	{
		m_N=0;
		m_pV=null;
		Dim = _Dim;
		Resize(N);
	}
	
	public void Resize(int N )
	{
		if(m_N!=N){
			if(m_N != 0 ){m_pV = null;}
			m_pV=null;
			m_N = N;
			if(N != 0){m_pV = new float[Dim*N];}
		}
		// memset( m_pV, 0, N*sizeof(T)*Dim );
		m_pV = new float[N*Dim];
	}
	
	public NVectorF(int _Dim, int N, float[] pV )
	{
		Dim = _Dim;
		Resize(N);
		// memcpy( m_pV, pV, N*sizeof(T)*Dim );
		for ( int i = 0; i < N * Dim; i++ ) {
			m_pV[i] = pV[i];
		}
	}
	
	public void dispose() {
		Resize(0);
	}
	
	// operator = 
	public NVectorF set(NVectorF V)
	{
		Resize(V.m_N);
		// memcpy( m_pV, V.m_pV, m_N*sizeof(T)*Dim );
		for ( int i = 0; i < m_N * Dim; i++ ) {
			m_pV[i] = V.m_pV[i];
		}
		return this;
	}
	
	public int Dimensions() {
		return m_N;
	}
	
	public void SetZero() {
		for (int i=0; i<m_N*Dim; i++) {
			m_pV[i] = m_pV[0];
		}
	}
	
	// operator (), operator []
	public float get(int i)
	{
		assert( i < m_N );
		return m_pV[i*Dim];
	}
	
	// operator * 
	public NVectorF mul(float A) 
	{
		NVectorF V = new NVectorF(Dim, this);
		for (int i=0; i<m_N*Dim; i++)
			V.m_pV[i] *= A;
		return V;
	}
	
	// operator *= 
	public NVectorF mul_into(float A)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] *= A;
		return this;
	}
	
	// operator /
	public NVectorF div(float A)
	{
		NVectorF V = new NVectorF(this.Dim, this);
		for (int i=0; i<m_N*Dim; i++)
			V.m_pV[i] /= A;
		return V;
	}
	
	// operator /=
	public NVectorF div_into(float A)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] /= A;
		return this;
	}
	
	// operator + 
	public NVectorF add(NVectorF V0)
	{
		NVectorF V = new NVectorF(Dim, m_N);
		for (int i=0; i<m_N*Dim; i++)
			V.m_pV[i] = m_pV[i] + V0.m_pV[i];

		return V;
	}
	
	// operator +=
	public NVectorF add_into (NVectorF V)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] += V.m_pV[i];

		return this;
	}
	
	// operator - 
	public NVectorF sub(NVectorF V0)
	{
		NVectorF V =new NVectorF(Dim, m_N);
		for (int i=0; i<m_N*Dim; i++)
			V.m_pV[i] = m_pV[i] - V0.m_pV[i];

		return V;
	}
	
	// operator - 
	public NVectorF neg()
	{
		NVectorF V = new NVectorF(Dim, m_N);

		for (int i=0; i<m_N*Dim; i++)
			V.m_pV[i] = -m_pV[i];

		return V;
	}

	// operator -= 
	public NVectorF sub_into(NVectorF V)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] -= V.m_pV[i];
		return this;
	}
	
	public float Norm( int Ln )
	{
		float N = 0f;
		for (int i = 0; i<m_N*Dim; i++)
			N += (float)Math.pow(m_pV[i], Ln);
		return (float)Math.pow(N, (float)1.0f/Ln);	
	}
	
	public void Normalize()
	{
		float N = 1.0f/Norm(2);
		for (int i = 0; i<m_N*3; i++)
			m_pV[i] *= N;
	}
	
	public float Length()
	{
		float N = 0f;
		for (int i = 0; i<m_N*Dim; i++)
			N += m_pV[i]*m_pV[i];
		return (float)Math.sqrt(N);	
	}
	
	public float Dot(NVectorF V )
	{
		float V0 = 0f;
		for (int i=0; i<m_N*Dim; i++)
			V0 += m_pV[i]*V.m_pV[i];

		return V0;
	}
	
	
	public NVectorF AddScaled(NVectorF V, float scale)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] += V.m_pV[i]*scale;
		return this;
	}
	
	public NVectorF SubtractScaled(NVectorF V, float scale)
	{
		for (int i=0; i<m_N*Dim; i++)
			m_pV[i] -= V.m_pV[i]*scale;

		return this;
	}
	
	public void Add(NVectorF V1,float scale1,NVectorF V2,float scale2,NVectorF Out){
		for (int i=0; i<V1.m_N*Dim; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i]*scale2;
	}
	
	public void Add(NVectorF V1, float scale1,NVectorF V2,NVectorF Out){
		for (int i=0; i<V1.m_N*Dim; i++)
			Out.m_pV[i]=V1.m_pV[i]*scale1+V2.m_pV[i];
	}
	
	
	
}