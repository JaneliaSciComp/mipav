package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.diffusion;

import java.util.Vector;
import java.io.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.jama.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;

public class DiffusionGradients
{
	/** Setup the gradient directions */
	private Vector directions = null;

	/** Setup the gradient strengths */
	private Vector strengths = null;

	/**
	 *  Does nothing really.
	 */
	public DiffusionGradients()
	{
	}

	/**
	 *  Add a gradient direction.
	 */
	public void addDirection(double x, double y, double z)
	{
		System.out.println("Adding gradient direction: " + x + " " + y + " " + z);

		// Create the diffusion gradient direction vector.
		if( directions == null ) directions = new Vector();

		double[][] temp = {{x}, {y}, {z}};

		directions.add( new Matrix(temp, 3, 1) );
	}

	public void addGradientStrength(final double gradStrength)
	{
		System.out.println("Adding gradient strength: " + gradStrength);

		if( strengths == null ) strengths = new Vector();

		double[][] temp = {{gradStrength, 2.0*gradStrength}};

		strengths.add( new Matrix(temp, 1, 2) );

	}

	/**
	 *  Create the A matrix.
	 */
	public Matrix getA()
	{
		//  Make sure that the strengths and directions are defined
		//  before doing much.
		if( strengths == null || directions == null )
		{
			System.out.println("diffusion: Gradient strengths or directions undefined.");
			System.exit(-1);
		}

		Matrix A = new Matrix( strengths.size()*directions.size(), 7 );


		//  Run over the gradient strengths
		int ii=0;
		for(int gsi=0; gsi < strengths.size(); gsi++)
		{
			//  Run over the gradient directions
			for(int gdi=0; gdi < directions.size(); gdi++)
			{
				// Now compute the outer product of the gradients.
				//      the gradient is a column vector
				Matrix temp = (Matrix)directions.get(gdi);

				// Compute the outer product.
				Matrix outer = new Matrix( 3, 3 );
				for(int kk=0; kk<temp.getRowDimension(); kk++)
				{
					for(int ll=0; ll<temp.getRowDimension(); ll++)
					{
						outer.set(kk,ll, temp.get(kk,0)*temp.get(ll,0));
					}
				}

				double sum=temp.norm1();

				if( sum == 1.0 )
				{
					outer.timesEquals( ((Matrix)strengths.get(gsi)).get(0,0) );
				}
				else
				{
					outer.timesEquals( ((Matrix)strengths.get(gsi)).get(0,1) );
				}

				//  Fill in the A matrix.
				A.set(ii, 0, -outer.get(0,0) ) ;
				A.set(ii, 1, -outer.get(1,1) ) ;
				A.set(ii, 2, -outer.get(2,2) ) ;
				A.set(ii, 3, -2.0*outer.get(0,1) ) ;
				A.set(ii, 4, -2.0*outer.get(0,2) ) ;
				A.set(ii, 5, -2.0*outer.get(1,2) ) ;
				A.set(ii, 6, 1.0 ) ;

				ii++;
			}
		}

		return A;
	}

	public void readGradientFile(final String filename)
	{
		LEDataInputStream ldis = AppKit.openInput( filename );

		try
		{
			String line = " ";
			while( line != null  )
			{
				line = ldis.readLine();
				System.out.println("Going to parse out: " + line);

				if( line == null || line.trim().startsWith("#") || line.length() == 0 )
					continue;

//				//  See if it is the little delta value.
//				if( line.trim().startsWith("little:") )
//				{
//					int index = line.trim().indexOf(":");
//					littleDelta = Double.parseDouble( line.trim().substring(index+1).trim() );
//				}
//
//				//  See if it is the big delta value.
//				if( line.trim().startsWith("big:") )
//				{
//					int index = line.trim().indexOf(":");
//					bigDelta = Double.parseDouble( line.trim().substring(index+1).trim() );
//				}

				//  See if it is the big delta value.
				if( line.trim().startsWith("bvalue:") )
				{
					int index = line.trim().indexOf(":");
					double temp = Double.parseDouble( line.trim().substring(index+1).trim() );
					addGradientStrength(temp);
				}

				//  See if it is a gradient direction set
				if( line.trim().startsWith("direction:") ||
				    line.trim().startsWith("gradient:") )
				{
					int index = line.trim().indexOf(":");
					String tt = line.trim().substring(index+1).trim();

					// Now parse the directions.
					double[] vv = parseList(tt, 3);

					addDirection(vv[0], vv[1], vv[2]);
				}

				//  See if it is a gradient direction set
				if( line.trim().startsWith("strength:") )
				{
					int index = line.trim().indexOf(":");
					String tt = line.trim().substring(index+1).trim();

					// Now parse the directions.
					double[] vv = parseList(tt, 1);

					addGradientStrength(vv[0]);
				}

			}
		}
		catch(EOFException eof)
		{}
		catch( IOException e)
		{
			System.out.print("diffusion: Problem reading " + filename);
			System.out.println(": " + e);
			System.exit(-1);
		}
	}

	/**
	 *  This routine will parse a list of double values
	 *  and output them.
	 */
	private double[] parseList(String tt, int num)
	{
		double[] values = new double[ num ];	

		String bob = tt.trim();

		int end = 0;
		for(int ii=0; ii<num; ii++) 
		{
			// First character of bob must be the beginning of a number.

			end = bob.indexOf(",");

			if( end == -1 ) { end = bob.indexOf(" "); }
			if( end == -1 ) { end = bob.length(); }

			System.out.println("bob is " + bob);
			System.out.println("going to parse " + bob.substring(0,end).trim());
			values[ii] = Double.parseDouble( bob.substring(0,end).trim() );

			if( ii < num-1 )
				bob = bob.substring(end+1).trim();
		}

		return values;
	}

//	public double calculateB(final double gradStrength, final double littleDelta, final double bigDelta, final double alpha)
//	{
//		// gamma in 1/(gauss * seconds)
//		double gamma_proton =  26751.5255;
//
//		double alpha = gamma_proton * (littleDelta/1000.0) * 
//		               Math.sqrt( (bigDelta/1000.0) - (littleDelta/1000.0)/3.0);
//
//		//  Divide by 100 to get it into s/mm^2
//		return Math.pow(alpha, 2.0) * Math.pow(gradStrength, 2.0) / 100.0;
//	}

}
