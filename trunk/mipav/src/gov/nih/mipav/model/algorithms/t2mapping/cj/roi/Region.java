package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public abstract class Region
{

	public Region() { }

	abstract public double getData(int nrows, int ncols, int nslices);

}
