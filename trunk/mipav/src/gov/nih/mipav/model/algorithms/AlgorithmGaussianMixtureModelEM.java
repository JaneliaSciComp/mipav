package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;

import java.util.*;

/**
Copyright (c) 1995 The Board of Trustees of Purdue University.
Permission to use, copy, modify, and distribute this software and its documentation for any purpose,
without fee, and without written agreement is hereby granted, provided that the above copyright notice and
the following two paragraphs appear in all copies of this software.
In no event shall Purdue University be liable to any party for direct, indirect, special, incidental, or
consequential damages arising out of the use of this software and its documentation, even if Purdue University
has been advised of the possibility of such damage.
Purdue University specifically disclaims any warranties, including, but not limited to, the implied warranties
 of merchantability and fitness for a particular purpose. The software provided hereunder is on an “as
is” basis, and Purdue Univeristy has no obligation to provide maintenance, support, updates, enhancements,
or modifications.

This is a port of cluster-3.6.7 developed by:
Charles A. Bouman; School of ECE, Purdue University
Michael Shapiro; NCSA
Gregory W. Cook; School of ECE, Purdue University
C. Brian Atkins; School of ECE, Purdue University
Hui Cheng; School of ECE, Purdue University
Jennifer G. Dy; School of ECE, Purdue University
Sean Borman; Department of Electrical Engineering, University of Notre Dame

This is software for doing unsupervised
clustering. This is done by estimating the parameters
of a Gaussian mixture model using the EM algorithm.

*/

public class AlgorithmGaussianMixtureModelEM extends AlgorithmBase {
	// number of classes
	private int nclasses;
	
	public AlgorithmGaussianMixtureModelEM() {
		
	}
	
	public AlgorithmGaussianMixtureModelEM(int nclasses) {
		this.nclasses = nclasses;
	}
	
	public void runAlgorithm() {
		
	}
	
}