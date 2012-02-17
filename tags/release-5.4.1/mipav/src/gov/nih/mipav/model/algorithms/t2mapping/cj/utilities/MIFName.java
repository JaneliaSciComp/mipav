package gov.nih.mipav.model.algorithms.t2mapping.cj.utilities;

import java.io.File;

public class MIFName
{
	final static String GZ_STR = ".gz", MIF_STR = ".MIF";

	protected String dir, base, ext = "", gzext = "";
	protected File f;
	protected boolean touched = false, validated = false, isGzipped = false;

	private String patient;
	private char mode;
	private String sequence, process = "";

	public MIFName(String s)
	{
		f = new File(s);
		dir = f.getParent();
		base = f.getName();

		if (base.endsWith(GZ_STR))
		{
			isGzipped = true; gzext = GZ_STR;
			base = base.substring(0,base.length()-GZ_STR.length());
		}

		int k = base.lastIndexOf('.');

		if (k >= 0)
		{
			ext = base.substring(k);
			base = base.substring(0,k);
		}

		if (isValid())
		{
			patient = base.substring(0,5);
			mode = base.charAt(5);
			sequence = base.substring(6,8);

			if (base.length() > 8) process = base.substring(8);
		}
	}

	public boolean isValid()
	{
		if (validated) return true;

		if (base == null) return false;
//		else if (!ext.equals(MIF_STR)) return false;
		else if (base.length() < 8) return false;
		else if (!Character.isUpperCase(base.charAt(0))) return false;
		else if (!Character.isDigit(    base.charAt(1))) return false;
		else if (!Character.isDigit(    base.charAt(2))) return false;
		else if (!Character.isDigit(    base.charAt(3))) return false;
		else if (!Character.isDigit(    base.charAt(4))) return false;
		else if (!Character.isUpperCase(base.charAt(5))) return false;
		else if (!Character.isDigit(    base.charAt(6))) return false;
		else if (!Character.isDigit(    base.charAt(7))) return false;

		else return validated = true;
	}

	public MIFName append(String s)
		{ touched = true; process += s; return this;}

	public MIFName recode(char c)
		{ touched = true; mode = c; return this; }

	public MIFName retype(String x)
		{ touched = true; ext = x; return this; }

	public File getFile()
	{
		if (touched) { f = new File(dir, getName()); touched = false; }
		return f;
	}

	public String toString() { return getFile().getPath(); }

	public String getDir() { return dir; }

	public String getSName()
		{ return patient + 'S' + sequence + process + ext + gzext; }

	public String getLName()
		{ return patient + 'L' + sequence + process + ext + gzext; }

	public String getName()
		{ return getScanCode() + process + ext + gzext; }

	public String getScanCode()
		{ return patient + mode + sequence; }

	public String getType()
		{ return "" + ext; }

	public String getRelatedPath(String suffix)
		{ return new File(dir, getScanCode() + process + suffix).getPath(); }
}
