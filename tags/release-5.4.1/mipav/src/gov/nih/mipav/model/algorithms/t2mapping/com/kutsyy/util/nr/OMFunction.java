package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.nr;

import gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util.*;

class OMFunction implements ObjectiveFunction
{
	MvFunction me;

	OMFunction(MvFunction f) { me = f; }

	public double eval(double[] x) { return me.f(x); }
}
