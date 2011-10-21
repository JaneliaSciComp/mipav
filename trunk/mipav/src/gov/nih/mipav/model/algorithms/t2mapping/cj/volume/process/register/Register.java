package gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process.register;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.interpolate.*;

abstract public class Register
{
    protected InterpolateRow interpX = InterpolateRow.createInterpolator("linear");
    protected InterpolateRow interpY = InterpolateRow.createInterpolator("linear");
    protected InterpolateRow interpZ = InterpolateRow.createInterpolator("linear");
    protected InterpolateRow internalInterpX = InterpolateRow.createInterpolator("linear");
    protected InterpolateRow internalInterpY = InterpolateRow.createInterpolator("linear");
    protected InterpolateRow internalInterpZ = InterpolateRow.createInterpolator("linear");
	protected int verbose = 0;

	/**
	 *  Constructor that takes two data volumes.
	 */
	public Register()
	{
	}

	/**
	 *  The actual registration routine
	 */
	abstract public Volume register(final Volume fixed, final double[] fixedVox, final Volume dynamic, final double[] dynVox);

	public void setVerbose(int verbose) { this.verbose = verbose; }

    public void setInterpolatorX( final InterpolateRow I)
        { interpX = I; }

    public void setInterpolatorY( final InterpolateRow I)
        { interpY = I; }

    public void setInterpolatorZ( final InterpolateRow I)
        { interpZ = I; }

    public void setInternalInterpolatorX( final InterpolateRow I)
        { internalInterpX = I; }

    public void setInternalInterpolatorY( final InterpolateRow I)
        { internalInterpY = I; }

    public void setInternalInterpolatorZ( final InterpolateRow I)
        { internalInterpZ = I; }
}
