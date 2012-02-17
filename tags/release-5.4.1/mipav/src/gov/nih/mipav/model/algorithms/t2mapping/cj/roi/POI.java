package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

public class POI {

	public short x,y,z;
	public float f;

	public POI() {}

	public POI(short x_in, short y_in, short z_in, float f_in) 
	{
		x = x_in;
		y = y_in;
		z = z_in;
		f = f_in;
	}

	final public short getX() { return x; }
	final public short getY() { return y; }
	final public short getZ() { return z; }
	final public float getF() { return f; }

	public String toString()
	{
		String s = (x+1) + " " + (y+1) + " " + (z+1) + " ";
		if (f == 1.0) s += "1"; else s += f;
		return s;
	}
};
