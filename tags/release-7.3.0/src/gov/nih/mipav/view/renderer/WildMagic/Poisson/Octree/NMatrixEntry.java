package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class NMatrixEntry<T> {
	
	public NMatrixEntry(int Dim )		{ N =-1; Value = (T[])new Object[Dim]; }
	public NMatrixEntry( int i, int Dim )	{ N = i; Value = (T[]) new Object[Dim]; }
	int N;
	T Value[];
	
}