package gov.nih.mipav.model.algorithms.t2mapping.cj.mri.relax.t2roi;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2fit.T2FitNNLS;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.t2bin.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

/** T2MapClass is responsible to calculate the T2 spectrum through a volume
 *  of points, to contain the results and to save the results.
 */
public class T2ROIAnimal extends T2ROI
{
	private int animal_size = 10;
	private Random rand_gen = null;

	/** 
	 *  All we really need to do is create the random number generator.
	 */
	public T2ROIAnimal()
	{
		rand_gen = new Random();
	}

	/**
	 *  Retrieve the name of this method.
	 */
	public String getMethodName() { return "animal"; }
	
	/**
	 *  Set the size of the animal.
	 */
	public void setAnimalSize( int animal_size )
	{
		this.animal_size = animal_size;
	}

	/**
	 *  Calculate the mean and standard deviation over an animal.
	 */
	protected void calculateMeanStd(final MCVolume vol, final int slice, final PixFile pix, 
							        double[] means, double[] stds)
	{
			// Create a random animal.
			int[] animal_indices = createAnimal( pix );

			// Zero everything out.
			double[] total_squared = new double[ means.length ];
			Arrays.fill(means, 0.0);
			Arrays.fill(total_squared, 0.0);
			Arrays.fill(stds, 0.0);

			// Compute the mean and standard deviation over the animal.
			for(int jj=0; jj<animal_size; jj++)
			{
				int rowcol[] = T2ROI.index2sub( animal_indices[jj], vol.getNRows(), vol.getNCols());
				for(int ch=0; ch<vol.getNChannels(); ch++)
				{
					double tempval = (double)vol.getData(rowcol[0],rowcol[1],slice,ch);
					means[ch] += tempval;
					total_squared[ch] += (tempval*tempval);
				}
			}

			// Compute the standard deviation.  
			for(int ch=0; ch<vol.getNChannels(); ch++)
			{
				stds[ch] = Math.sqrt((total_squared[ch]-(means[ch]*means[ch])/(double)animal_size)/(double)(animal_size-1) );   
				means[ch] /= (double)animal_size;
			}

	}

	/**  Create an animal.
	 */
	private int[] createAnimal( final PixFile pix)
	{
		int[] animal_indices = new int[ animal_size ];
		int num_animal_indices = 0;
		int nrows = 256;
		int ncols = 256;
		int[] pixels = pix.getIndices();

		BinarySlice slice_mask = new BinarySlice( nrows, ncols, pix );	

		/*
		 *  Find a random starting element in the range of 
		 *  [0, N).
		 */
		int rindex = rand_gen.nextInt( pix.getIndices().length );

		animal_indices[0] = pixels[rindex];
		num_animal_indices++;

		/*
		 *  Now try and find a 4-connected set of pixels.
		 */
		while( num_animal_indices < animal_size )
		{
			/*
			 *  Choose a random pixel in the animal 
			 *  from which to find a random direction.
			 */
			int start_index = rand_gen.nextInt( num_animal_indices );

			/*
			 *   Now choose a random direction and check to see if 
			 *   it is in the mask.
			 */
			int dir = rand_gen.nextInt( 4 );

			// Compute the row and column
			int row = animal_indices[start_index]/nrows;
			int col = animal_indices[start_index]%nrows;

			if( row < 1 || row > nrows-1 )
				continue;

			if( col < 1 || col > ncols-1 )
				continue;

			switch( dir )
			{
				case 0:
				 	if( slice_mask.getData(row+1,col) )
					{
						animal_indices[num_animal_indices] = animal_indices[start_index]+nrows;
						num_animal_indices++;
					}
				 	break;
				case 1:
				 	if( slice_mask.getData(row,col-1) )
					{
						animal_indices[num_animal_indices] = animal_indices[start_index]-1;
						num_animal_indices++;
					}
				 	break;
				case 2:
				 	if( slice_mask.getData(row-1,col) )
					{
						animal_indices[num_animal_indices] = animal_indices[start_index]+nrows;
						num_animal_indices++;
					}
				 	break;
				case 3:
				 	if( slice_mask.getData(row,col+1) )
					{
						animal_indices[num_animal_indices] = animal_indices[start_index]+1;
						num_animal_indices++;
					}
				 	break;
			 	default:
			}
		}
		

		return animal_indices;
	}
}
