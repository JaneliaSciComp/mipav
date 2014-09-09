package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B.
 * Stegmann IMM, Informatics & Mathmatical Modelling Technical University of
 * Denmark Richard Petersens Plads Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * Abstract class for deformation basis.
 * 
 * @author Ruida Cheng
 * 
 */
public abstract class CAAMDeform extends CAAMObject {

	// / Flags that signals everything has been setup correctly.
	protected boolean m_bValid;

	public CAAMDeform() {
		m_bValid = false;
	}

	public boolean IsTruncated() {
		return NParameters() != NParametersOrg();
	}

	public boolean IsValid() {
		return m_bValid;
	}

	public abstract int NParameters();

	public abstract int NParametersOrg();

	public abstract double ParameterWeight(final int i, boolean asPercentage);

	public abstract double ParameterWeightOrg(final int i, boolean asPercentage);

	public abstract void Deform(final CDVector params, CDVector object);

	public abstract void Truncate(final int n);

	public abstract void ClearDataItems();

	public abstract int TruncateVar(final double retained_variance);

	public abstract int TruncateParallel();

}