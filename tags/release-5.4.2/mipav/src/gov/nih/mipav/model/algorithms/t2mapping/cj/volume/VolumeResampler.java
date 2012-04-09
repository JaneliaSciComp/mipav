package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.transform.*;

/**
 *  Use this class to resample a given volume either point-by-point 
 *  or else over the whoel volume.
 *
 *  All dimensions and sizes are of the "resampled volume".
 */
public class VolumeResampler extends VolumeBase
{

	private double[] r3t3 = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

	/** The volume we are interpolating on. */
	private double[] outputVoxelSize = new double[3];

	/** The volume we are interpolating on. */
	private double[] inputVoxelSize = {0.859, 0.859, 5.0};

	/** The interpolater to use. */
	private InterpolateVolume pol = null;

	/** Transformer */
	private TransformAffine T = null;

	private int nRows = 0;
	private int nCols = 0;
	private int nSlices = 0;

	/** Interpolator along the row -- default is Linear interpolation. */
	private InterpolateRow interpR = new InterpolateRowLinear();

	/** Interpolator along the column -- default is Linear interpolation. */
	private InterpolateRow interpC = new InterpolateRowLinear();

	/** Interpolator along the slice -- default is Linear interpolation. */
	private InterpolateRow interpS = new InterpolateRowLinear();

	public VolumeResampler(final Volume vv, final double[] inputVoxelSize)
	{
		pol = new InterpolateVolume( vv, interpR, interpC, interpS );

		this.inputVoxelSize[0] = inputVoxelSize[0]; 
		this.inputVoxelSize[1] = inputVoxelSize[1]; 
		this.inputVoxelSize[2] = inputVoxelSize[2]; 

		//  The default output voxel size is the same as the input
		//  voxel size.
		setOutputVoxelSize( inputVoxelSize );

		setDimensions(vv.getNRows(), vv.getNCols(), vv.getNSlices() );
	}

	public void setDimensions( int nRows, int nCols, int nSlices )
		{ this.nRows = nRows; this.nCols = nCols; this.nSlices = nSlices; }

	public int getNRows() 
		{ return nRows; }

	public int getNCols() 
		{ return nCols; }

	public int getNSlices() 
		{ return nSlices; }

	/** Set the voxel sizes, rather than using the defaults. */
	public void setOutputVoxelSize(final double[] voxelSize)
	{
		if( voxelSize == null || voxelSize.length != 3 )
		{
			throw new ArrayIndexOutOfBoundsException("VolumeResampler");
		}

		this.outputVoxelSize[0] = voxelSize[0]; 
		this.outputVoxelSize[1] = voxelSize[1]; 
		this.outputVoxelSize[2] = voxelSize[2]; 
	}
	
	/** Set the rotation around X in degrees. */
	public void setRotationX(final double tt)
		{ r3t3[0] = tt; initTransform(); }

	/** Set the rotation around Y in degrees. */
	public void setRotationY(final double tt)
		{ r3t3[1] = tt; initTransform(); }

	/** Set the rotation around Z in degrees. */
	public void setRotationZ(final double tt)
		{ r3t3[2] = tt; initTransform(); }

	/** Set the translation in X in spatial units (eg mm) . */
	public void setTranslationX(final double tt)
		{ r3t3[3] = tt; initTransform(); }

	/** Set the translation in Y in spatial units (eg mm) . */
	public void setTranslationY(final double tt)
		{ r3t3[4] = tt; initTransform(); }

	/** Set the translation in Z in spatial units (eg mm) . */
	public void setTranslationZ(final double tt)
		{ r3t3[5] = tt; initTransform(); }

	/** Set the interpolator for along the row direction . */
	public void setRowInterpolator(InterpolateRow I)
		{ interpR = I; pol.setRowInterpolator(interpR); }

	/** Set the interpolator for along the column direction . */
	public void setColInterpolator(InterpolateRow I)
		{ interpC = I; pol.setColInterpolator(interpC); }

	/** Set the interpolator for along the slice direction . */
	public void setSliceInterpolator(InterpolateRow I)
		{ interpS = I; pol.setSliceInterpolator(interpS); }

	/**
	 *  Intitialize the transform given the rotations, translations, and
	 *  the size of the volume.
	 */
	private void initTransform()
	{
		T = new TransformAffine();
		T.translateX(-inputVoxelSize[0]*(pol.getNRows()-1)/2.0);
		T.translateY(-inputVoxelSize[1]*(pol.getNCols()-1)/2.0);
		T.translateZ(-inputVoxelSize[2]*(pol.getNSlices()-1)/2.0);

		T.rotateX(r3t3[0]).rotateY(r3t3[1]).rotateZ(r3t3[2]);
		T.translateX(r3t3[3]).translateY(r3t3[4]).translateZ(r3t3[5]);

		T.translateX(outputVoxelSize[0]*(getNRows()-1)/2.0);
		T.translateY(outputVoxelSize[1]*(getNCols()-1)/2.0);
		T.translateZ(outputVoxelSize[2]*(getNSlices()-1)/2.0);

		T = T.inverse();
	}

	//  Private temporary data.
	private double[] x_pre = new double[3];
	private double[] x_post = new double[3];

	final public float getData(int x, int y, int z)
	{
		x_pre[1] = x * outputVoxelSize[1];
		x_pre[0] = y * outputVoxelSize[0];
		x_pre[2] = z * outputVoxelSize[2];

		T.transform(x_pre,x_post);

		x_post[0] /= inputVoxelSize[0];
		x_post[1] /= inputVoxelSize[1];
		x_post[2] /= inputVoxelSize[2];

		return (float)pol.interpolate(x_post[1],x_post[0],x_post[2]);
	}

	public Volume getVolume()
	{
		return new Volume(this);
	}
}
