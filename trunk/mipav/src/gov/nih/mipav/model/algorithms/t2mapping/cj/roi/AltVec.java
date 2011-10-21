package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.util.*;
import java.lang.System.*;

public class AltVec
{
final static private int BLOCK_BITS = 8;
final static private int BLOCK_SIZE = (1<<BLOCK_BITS);
final static private int BLOCK_MASK = (1<<BLOCK_BITS)-1;

private Vector blocks;
private Object cache=null;
private int n,last=-1;

public AltVec() { blocks = new Vector(); n = 0; }

final public int size() { return n; }
final public void clear() { blocks.clear(); n = 0; last = -1; }

/*
// Argument is vector-level index
final private static int whichBlock(int n) { return (n >> BLOCK_BITS); }
final private static int indexInBlock(int n) { return (n & BLOCK_MASK); }
// Argument is block-level index
final private static int blockSize(int n) { return BLOCK_SIZE; }
final private static int blockStart(int n) { return (n << BLOCK_BITS); }
*/

final private static int whichBlock(int n)
  { return n>0?((n-1) >> BLOCK_BITS)+1:0; }
final private static int indexInBlock(int n)
  { return n>0?((n-1) & BLOCK_MASK):0; }

final private static int blockSize(int n)
  { return n>0?BLOCK_SIZE:1; }
final private static int blockStart(int n)
  { return n>0?((n-1) << BLOCK_BITS)+1:0; }

/*
final private static int isqrt(int n)
  { int q=0,r=1<<28;
	while(r>0) {q>>=1; r>>=2; if (n>=r+q) { n-=r|q; q+=r<<1; } } return q; }
final private static int whichBlock(int n)
  { return isqrt(n); }
final private static int indexInBlock(int n)
  { int s=isqrt(n); return n-s*s; }

final private static int blockSize(int n)
  { return 2*n+1; }
final private static int blockStart(int n)
  { return n*n; }
*/

final private void touchCache(int i)
{
	if (last != whichBlock(i))
	  { last = whichBlock(i); cache = blocks.get(last); }
}

final public short getShort(int i)
{
	touchCache(i);

	short[] t = (short[]) cache;
	return t[indexInBlock(i)];
}

final public float getFloat(int i)
{
	touchCache(i);

	float[] t = (float[]) cache;
	return t[indexInBlock(i)];
}

final public void add(short o)
{
	short[] t;
	int i = indexInBlock(n);

	if (i==0)
	{
		last = whichBlock(n);
		cache = t = new short[blockSize(last)];
		t[0] = o; blocks.add(t);
	}
	else
	{
		touchCache(n);
		t = (short[]) cache;
		t[i] = o;
	}
	n++;
}

final public void add(float o)
{
	float[] t;
	int i = indexInBlock(n);

	if (i==0)
	{
		last = whichBlock(n);
		cache = t = new float[blockSize(last)];
		t[0] = o; blocks.add(t);
	}
	else
	{
		touchCache(n);
		t = (float[]) cache;
		t[i] = o;
	}
	n++;
}

final public short[] toArray(short[] a)
{
	short[] t;
	int i,j=indexInBlock(n),k=whichBlock(n);

	for(i=0; i<k; i++)
	{
		t = (short[]) blocks.get(i);
		System.arraycopy(t, 0, a, blockStart(i), blockSize(i));
	}
	if (j>0)
	{
		t = (short[]) blocks.get(k);
		System.arraycopy(t, 0, a, blockStart(k), j);
	}

	return a;
}

final public float[] toArray(float[] a)
{
	float[] t;
	int i,j=indexInBlock(n),k=whichBlock(n);

	for(i=0; i<k; i++)
	{
		t = (float[]) blocks.get(i);
		System.arraycopy(t, 0, a, blockStart(i), blockSize(i));
	}
	if (j>0)
	{
		t = (float[]) blocks.get(k);
		System.arraycopy(t, 0, a, blockStart(k), j);
	}

	return a;
}

}
