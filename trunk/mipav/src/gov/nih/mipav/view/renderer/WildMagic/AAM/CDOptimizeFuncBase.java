package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation.
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B. Stegmann IMM, Informatics &
 * Mathmatical Modelling Technical University of Denmark Richard Petersens Plads
 * Building 321 DK-2800 Lyngby, Denmark http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * Copyright © 2000
 * 
 * DTU Image Viever and Analyser (DIVA) Department of Mathematical Modelling
 * Technical University of Denmark (DTU), Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/~diva
 * 
 * author: Rune Fisker
 * 
 * Function Evaluation Base Class
 * 
 * This abstract class is used to interface between the optimization
 * class/functions and the classes, which contains the problem to be optimized
 * (see Rosenbrock test example in the bottom of this file)
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public interface CDOptimizeFuncBase {
	/** evaluates the function value at postion vX */
	public abstract double EvalFunction(CDVector vX);

	/** evaluates the analytic gradient at postion vX (if exists) */
	public abstract void EvalGradient(CDVector vX, CDVector vGradient);

	/** function used to update e.g. interface */
	public abstract void Update(CDVector vX);
}