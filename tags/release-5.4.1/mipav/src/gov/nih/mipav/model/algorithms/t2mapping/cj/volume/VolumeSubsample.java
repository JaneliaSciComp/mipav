package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.transform.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.matrix.*;

/**
 *  Use this class to resample a given volume either point-by-point 
 *  or else over the whoel volume.
 *
 *  All dimensions and sizes are of the "resampled volume".
 */
public class VolumeSubsample extends VolumeBase
{
	private double[] outputVoxelSize = new double[3];
	private double[] inputVoxelSize = new double[3];
	private double[] fact = new double[3];

	/** The interpolater to use. */
	private InterpolateVolume pol = null;

	private int nRows = 0, nCols = 0, nSlices = 0;

	public VolumeSubsample(final Volume vv, final double[] voxelSize, final double[] fact)
	{
		Vect.copy(this.inputVoxelSize,voxelSize);
		Vect.copy(this.fact,fact);

		outputVoxelSize[0] = voxelSize[0]*fact[0]; 
		outputVoxelSize[1] = voxelSize[1]*fact[1]; 
		outputVoxelSize[2] = voxelSize[2]*fact[2]; 

		setDimensions((int)Math.ceil(vv.getNRows()/fact[0]),
                      (int)Math.ceil(vv.getNCols()/fact[1]),
                      (int)Math.ceil(vv.getNSlices()/fact[2]));

		pol = new InterpolateVolume( vv, new GaussianRow((fact[0]-1)/2),
                  new GaussianRow((fact[1]-1)/2), new GaussianRow((fact[2]-1)/2) );
	}

	public void setDimensions( int nRows, int nCols, int nSlices )
		{ this.nRows = nRows; this.nCols = nCols; this.nSlices = nSlices; }

	public int getNRows() { return nRows; }
	public int getNCols() { return nCols; }
	public int getNSlices() { return nSlices; }

	/** Set the voxel sizes, rather than using the defaults. */
	public double[] getOutputVoxelSize()
	{
		return Vect.clone(outputVoxelSize);
	}
	
	//  Private temporary data.
	private double[] x_pre = new double[3];

	final public float getData(int x, int y, int z)
	{
		x_pre[1] = (x - (getNCols()-1)/2.0)*fact[0] + (pol.getNCols()-1)/2.0;
		x_pre[0] = (y - (getNRows()-1)/2.0)*fact[1] + (pol.getNRows()-1)/2.0;
		x_pre[2] = (z - (getNSlices()-1)/2.0)*fact[2] + (pol.getNSlices()-1)/2.0;

		return (float)pol.interpolate(x_pre[1],x_pre[0],x_pre[2]);
	}

	public Volume getVolume() { return new Volume(this); }
}
