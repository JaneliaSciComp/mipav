package gov.nih.mipav.model.algorithms.t2mapping.cj.math.complex;

public class Complex implements Cloneable
{
	private double re;
	private double im;

	public Complex()
	{
		re = 0.0; im = 0.0;
	}

	public Complex(double re, double im)
	{
		this.re = re; this.im = im;
	}

    public Object clone()
    {
        Object o = null;
        try {
            o = super.clone();
			((Complex)o).re = re;
			((Complex)o).im = im;
        }
        catch( CloneNotSupportedException e) {
            System.out.println("Complex can not clone");
        }

        return o;
    }

	public void timesReal(final double other)
	{
		re *= other;
	}

	public void times(final Complex other)
	{
		double tt = re;

		re = re*other.re - im*other.im;
		im = tt*other.im + im*other.re;
	}

	public void setComplex(final Complex other)
	{
		this.re = other.re;
		this.im = other.im;
	}

	final public double real()
	{
		return re;
	}

	final public double imag()
	{
		return im;
	}

	final public void setReal(final double re)
	{
		this.re = re;
	}

	final public void setImag(final double im)
	{
		this.im = im;
	}

	final public double abs()
	{
		return Math.sqrt( re*re + im*im );
	}

	static public Complex times(Complex a, Complex b)
	{
		return new Complex( a.re*b.re-a.im*b.im,  a.re*b.im+a.im*b.re );
	}

	static public double abs(Complex other)
	{
		return Math.sqrt( other.re*other.re + other.im*other.im );
	}

	public String toString()
	{
		String s = "(" + re;

		if( im < 0.0 )
			s += " - " + (-im) + "i)";
		else
			s += " + " + im + "i)";
		
		return s;
	}
}
