package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process;

import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MIF4;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.transform.*;

/**
 *  Use this class to resample a given volume either point-by-point 
 *  or else over the whoel volume.
 */
public class Resample
{
	private double[] r3t3 = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

	/** The volume we are interpolating on. */
	private Volume volume = null;
	private double[] voxelSize = {0.859, 0.859, 5.0};

	/** The interpolater to use. */
	private InterpolateVolume pol = null;

	/** Transformer */
	private TransformAffine T = null;

	/** Interpolator along the row -- default is Linear interpolation. */
	private InterpolateRow interpR = new InterpolateRowLinear();

	/** Interpolator along the column -- default is Linear interpolation. */
	private InterpolateRow interpC = new InterpolateRowLinear();

	/** Interpolator along the slice -- default is Linear interpolation. */
	private InterpolateRow interpS = new InterpolateRowLinear();

	public Resample(VoxVolume volume)
	{
		this.volume = volume;
//		voxelSize = volume.getVoxelSize();
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
		{ interpR = I; }

	/** Set the interpolator for along the column direction . */
	public void setColInterpolator(InterpolateRow I)
		{ interpC = I; }

	/** Set the interpolator for along the slice direction . */
	public void setSliceInterpolator(InterpolateRow I)
		{ interpS = I; }

	/**
	 *  Intitialize the transform given the rotations, translations, and
	 *  the size of the volume.
	 */
	private void initTransform()
	{
		T = new TransformAffine();
		T.translateX(-voxelSize[0]*volume.getNRows()/2.0);
		T.translateY(-voxelSize[1]*volume.getNCols()/2.0);
		T.translateZ(-voxelSize[2]*volume.getNSlices()/2.0);

		T.rotateX(r3t3[0]).rotateY(r3t3[1]).rotateZ(r3t3[2]);
		T.translateX(r3t3[3]).translateY(r3t3[4]).translateZ(r3t3[5]);

		T.translateX(voxelSize[0]*volume.getNRows()/2.0);
		T.translateY(voxelSize[1]*volume.getNCols()/2.0);
		T.translateZ(voxelSize[2]*volume.getNSlices()/2.0);

		T = T.inverse();
	}

	//  Private temporary data.
	private double[] x_pre = new double[3];
	private double[] x_post = new double[3];

	/**  Resample the volume based on the co-ordinates at (xD, yD, zD). */
	final public double resample(double xD, double yD, double zD)
	{
		x_pre[0] = xD; x_pre[1] = yD; x_pre[2] = zD;

		 x_post[0] /= voxelSize[0];
		 x_post[1] /= voxelSize[1];
		 x_post[2] /= voxelSize[2];

		T.transform(x_pre,x_post);

		return pol.interpolate(x_post[0],x_post[1],x_post[2]);
	}

	public Volume resample(VoxVolume dyn)
	{
		return resample(dyn, dyn.getVoxelSize()[0], 
		                     dyn.getVoxelSize()[1],
							 dyn.getVoxelSize()[2]);
	}

	/**
	 *  Register two data volumes.
	 */
	public Volume resample(VoxVolume dyn, final double vvx, final double vvy, final double vvz)
	{
		double rx, ry, rz;

		//  Setup the interpolator.
		pol = new InterpolateVolume(dyn, interpR, interpC, interpS);

		//  Interpolate the new output volume based on the
		//  rotation and translation results.
		Volume out = new Volume(dyn);

		for(int si=0; si< out.getNSlices(); si++) 
		{
			rz = si * vvz;

			for(int ci=0; ci< out.getNCols(); ci++)
			{
				ry = ci * vvy;

				for(int ri=0; ri< out.getNRows(); ri++)
				{
					rx = ri * vvx;
					out.setData(resample(ri, ci, si), ri, ci, si);
				}
			}
		}

		return out;
	}
}
