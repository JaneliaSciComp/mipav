package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;

public class AlgorithmDualContourSearch extends AlgorithmBase {
	
	private int innerIndex;
	private int outerIndex;
	private int contourPoints;
	private int linePoints;
	private double regularization;
	private VOI resultVOI = null;
	
	
	public  AlgorithmDualContourSearch(ModelImage srcImg, int innerIndex, int outerIndex, int contourPoints, int linePoints,
			                           double regularization) {
		
		super(null, srcImg);
		this.innerIndex = innerIndex;
		this.outerIndex = outerIndex;
		this.contourPoints = contourPoints;
		this.linePoints = linePoints;
		this.regularization = regularization;
	}
	
	/**
     * Accessor that returns the resultant VOI.
     *
     * @return  resultant VOI that has localized to the boundaries of the object
     */
    public VOI getResultVOI() {
        return resultVOI;
    }
	
	public void runAlgorithm() {
		  
		
		setCompleted(true);
		return;
	}
}