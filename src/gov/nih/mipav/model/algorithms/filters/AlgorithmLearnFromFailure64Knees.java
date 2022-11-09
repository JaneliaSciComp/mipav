package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.*;

import java.io.*;
import java.util.*;
import java.awt.geom.*;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.image.*;

import javax.imageio.ImageIO;

import com.sun.jimi.core.Jimi;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibFoundation.Curves.*;

import ar.com.hjg.pngj.*;
import ar.com.hjg.pngj.chunks.PngChunkTextVar;

public class AlgorithmLearnFromFailure64Knees extends AlgorithmBase implements AlgorithmInterface {

	private ModelImage srcImage;
	private int imageNum = 0;
	private int sliceNum = 0;
	private int patchNum = 0;
	private ModelImage cropImage;
	private AlgorithmAddMargins cropAlgo;
	private ModelImage resultImage;
	private boolean training;
	private static BufferedWriter outStream;
	private float minIntensity;
	private float maxIntensity;
	private enum ClassType { POS, NEG, UNDEFINED };
	private String savedImageDir;
	
	public AlgorithmLearnFromFailure64Knees(ModelImage _srcImage, int _imageNum, int _sliceNum, float min, float max, boolean train, 
			String _savedImageDir) {
		srcImage = _srcImage;
		imageNum = _imageNum;
		sliceNum = _sliceNum;
		minIntensity = min;
		maxIntensity = max;
		training = train;
		savedImageDir = _savedImageDir;
	}
	
	public AlgorithmLearnFromFailure64Knees(ModelImage _srcImage, int _imageNum, int _sliceNum, float min, float max, boolean train, 
			BufferedWriter _outStream, String _savedImageDir) {
		srcImage = _srcImage;
		imageNum = _imageNum;
		sliceNum = _sliceNum;
		minIntensity = min;
		maxIntensity = max;
		training = train;
		outStream = _outStream;
		savedImageDir = _savedImageDir;
	}
	
	
    public void disposeLocal() {	
    	if ( cropImage != null ) {
    		cropImage.disposeLocal();
    		cropImage = null;
    	}
    	
    	if ( resultImage != null ) {
    		resultImage.disposeLocal();
    		resultImage = null;
    	}
    	
    	if ( cropAlgo != null ) {
    		cropAlgo.finalize();
    		cropAlgo = null;
    	}
    }

	public void algorithmPerformed(AlgorithmBase algorithm) {
		
	}

	public void runAlgorithm() {
		// smoothVOI30(srcImage, srcImage);
		// smoothVOI128(srcImage, srcImage);
		
		if ( srcImage.getVOIs().size() == 1 ) {
			smoothVOI128single(srcImage, srcImage);
			generatePatches();
		}
		// learnFromFailure();
	}
	
	public void smoothVOI128single(ModelImage maskImage, ModelImage resultImage) {

        VOIVector v = maskImage.getVOIs();
        if (v.size() == 0)
            return;
        v.VOIAt(0).setActive(true);
        v.VOIAt(0).setAllActive(true);

        // new ViewJFrameImage(maskImage);
        try {
            AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 128, false);
            // smoothAlgo.addListener(this);
            smoothAlgo.run();

            VOIVector resultVOIs = resultImage.getVOIs();
            VOI resultVOI = smoothAlgo.getResultVOI();
            resultVOIs.VOIAt(0).removeCurves();
            resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
            resultImage.notifyImageDisplayListeners(null, true);
            // new ViewJFrameImage(resultImage);
            smoothAlgo.setCompleted(true);
            smoothAlgo.finalize();
            smoothAlgo = null;
        } catch (OutOfMemoryError x) {
            MipavUtil
                    .displayError("Dialog Smooth: unable to allocate enough memory");

            return;
        }
    }
	
	private void learnFromFailure() {

		// new ViewJFrameImage(srcImage);
		float x, y, z;
		float x1, y1, z1;
		float xNearest = 0f, yNearest = 0f, zNearest = 0f;
		float minDistance, distance;
		float[] newXPts = null, newYPts = null, newZPts = null;
		newXPts = new float[4];
		newYPts = new float[4];
		newZPts = new float[4];
		
		VOI voiGT = srcImage.getVOIs().VOIAt(0);
		VOIBase vTempGT = voiGT.getCurves().elementAt(0);
		float[]  xGTPts = null, yGTPts = null, zGTPts = null;
		int nPtsGT = vTempGT.size();
		xGTPts = new float[nPtsGT];
		yGTPts = new float[nPtsGT];
		zGTPts = new float[nPtsGT];
		vTempGT.exportArrays(xGTPts, yGTPts, zGTPts);
		
		VOI voiAAM = srcImage.getVOIs().VOIAt(1);
		VOIBase vTempAAM = voiAAM.getCurves().elementAt(0);
		float[] xAAMPts = null, yAAMPts = null, zAAMPts = null;
		int nPtsAAM = vTempAAM.size();
		xAAMPts = new float[nPtsAAM];
		yAAMPts = new float[nPtsAAM];
		zAAMPts = new float[nPtsAAM];
		vTempAAM.exportArrays(xAAMPts, yAAMPts, zAAMPts);
		
		// VOI lineVOI = new VOI( (short)srcImage.getVOIs().size(), "line", VOI.LINE, 0 );
		// srcImage.registerVOI(lineVOI);
		// lineVOI.setColor(Color.blue);
		
		// VOI squareVOI = new VOI((short)srcImage.getVOIs().size(), "square", VOI.CONTOUR, 0);
		// srcImage.registerVOI(squareVOI);
		
		// VOI squareVOINeg = new VOI((short)srcImage.getVOIs().size(), "squareNeg", VOI.CONTOUR, 0);
		// srcImage.registerVOI(squareVOINeg);
		// squareVOINeg.setColor(Color.green);
		
		for ( int i = 0; i < nPtsAAM; i++ ) {
			
			x = xAAMPts[i];
			y = yAAMPts[i];
			z = zAAMPts[i];
			minDistance = Float.MAX_VALUE;
			
			for ( int j = 0; j < nPtsGT; j++ ) {
				
				x1 = xGTPts[j];
				y1 = yGTPts[j];
				z1 = zGTPts[j];
				
				distance = (float)Math.sqrt((x-x1)*(x-x1) + (y-y1)*(y-y1));
				if ( distance < minDistance ) {
					minDistance = distance;
					xNearest = x1;
					yNearest = y1;
					zNearest = z1;
				}
			}
			
			// draw line
			
			// VOILine kLine = new VOILine();
			// kLine.add(new Vector3f(x, y, 0f));
			// kLine.add(new Vector3f(xNearest, yNearest, 0f));
		    // lineVOI.importCurve(kLine);
			// System.err.println("minDistance = " + minDistance);
			
			// set the GT point to be the positive patch. 
			// VOIBase squareGT = new VOIContour(true);
			newXPts[0] = xNearest - 32;
			newYPts[0] = yNearest - 32;
			newXPts[1] = xNearest + 32;
			newYPts[1] = yNearest - 32;
			newXPts[2] = xNearest + 32;
			newYPts[2] = yNearest + 32;
			newXPts[3] = xNearest - 32;
			newYPts[3] = yNearest + 32;
			// squareGT.importArrays(newXPts, newYPts, newZPts, 4);
			// squareVOI.importCurve(squareGT);
		    extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.POS);
			patchNum++;
			
			if ( minDistance <= 4.0f ) {   
				// 1) min distance is less than 4, set the AAM point to positive patches. 
				// VOIBase squareAAM = new VOIContour(true);
				newXPts[0] = x - 32;
				newYPts[0] = y - 32;
				newXPts[1] = x + 32;
				newYPts[1] = y - 32;
				newXPts[2] = x + 32;
				newYPts[2] = y + 32;
				newXPts[3] = x - 32;
				newYPts[3] = y + 32;
				// squareAAM.importArrays(newXPts, newYPts, newZPts, 4);
				// squareVOI.importCurve(squareAAM);
				extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.POS);
				patchNum++;
			} else {
				// 2) min distance is greater than 4, set the tracing path from AAM point to GT point, and set the tracing point 
				// patch to negative. 
				int tracingSteps = (int)Math.round(minDistance / 4.0f);
				float stepDistX = (x - xNearest) / tracingSteps;
				float stepDistY = (y - yNearest) / tracingSteps;
				float currPosX = x, currPosY = y;
				
				for ( int k = 0; k < tracingSteps; k++ ) {
				
					// if tracing point dist to GT is less or equal to 4.0f, stop tracing for negative patches. 
					if ( Math.sqrt((currPosX-xNearest)*(currPosX-xNearest) + (currPosY-yNearest)*(currPosY-yNearest)) <= 5.0 ) break;
					
					// VOIBase squareAAM = new VOIContour(true);
					newXPts[0] = currPosX - 32;
					newYPts[0] = currPosY - 32;
					newXPts[1] = currPosX + 32;
					newYPts[1] = currPosY - 32;
					newXPts[2] = currPosX + 32;
					newYPts[2] = currPosY + 32;
					newXPts[3] = currPosX - 32;
					newYPts[3] = currPosY + 32;
					// squareAAM.importArrays(newXPts, newYPts, newZPts, 4);
					// squareVOINeg.importCurve(squareAAM);
				    
					extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.NEG);
					patchNum++;
					currPosX += stepDistX;
					currPosY += stepDistY;
				}
			} 
			
			
		}
		
	}
	
	private void generatePatches() {
		
		VOI voiNew = srcImage.getVOIs().VOIAt(0);
		VOIBase vTemp = voiNew.getCurves().elementAt(0);
		float[] newXPts = null, newYPts = null, newZPts = null;
		int nPts = vTemp.size();
		float normLength = 0;
		float stepPct = (float) 20.0;
		int i, j;
		Vector3f interpPt = new Vector3f();
		
		Vector3f inNormPt = new Vector3f();
		Vector3f inNormHalfPt = new Vector3f();
		
		Vector3f outNormPt = new Vector3f();
		Vector3f outNormHalfPt = new Vector3f();
	
		Vector3f tangentDir = new Vector3f();
		Vector3f normDir = new Vector3f();
		Vector3f normStep = new Vector3f();

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		vTemp.exportArrays(xPts, yPts, zPts);

		// VOI lineVOI = new VOI( (short)srcImage.getVOIs().size(), "line", VOI.LINE, 0 );
		// srcImage.registerVOI(lineVOI);
        // lineVOI.setColor(Color.blue);		

		// VOI squareVOI = new VOI((short)srcImage.getVOIs().size(), "square", VOI.CONTOUR, 0);
		// srcImage.registerVOI(squareVOI);
		
		// VOI squareVOINeg = new VOI((short)srcImage.getVOIs().size(), "square", VOI.CONTOUR, 0);
		// srcImage.registerVOI(squareVOINeg);
		// squareVOINeg.setColor(Color.green);
		
		// VOI squareVOIUncertain = new VOI((short)srcImage.getVOIs().size(), "square", VOI.CONTOUR, 0);
		// srcImage.registerVOI(squareVOIUncertain);
		// squareVOIUncertain.setColor(Color.pink);

		int currPt, prevPt, nextPt;

		newXPts = new float[4];
		newYPts = new float[4];
		newZPts = new float[4];

		// System.err.println("number Pts = " + nPts);
		// if ( nPts != 128 ) return;
		
		// walk around the VOI points
		for (i = 0; i < nPts; i++) {
	    // for (i = 10; i < 11; i++) {
			currPt = i;
			prevPt = i - 1;
			nextPt = i + 1;

			if (currPt == 0) {
				prevPt = nPts - 1;
				nextPt = currPt + 1;
			} else if (currPt == nPts - 1) {
				prevPt = currPt - 1;
				nextPt = 0;
			}

			tangentDir.X = (xPts[currPt] - xPts[prevPt] + xPts[nextPt] - xPts[currPt]) / 2;
			tangentDir.Y = (yPts[currPt] - yPts[prevPt] + yPts[nextPt] - yPts[currPt]) / 2;

			interpPt.X = xPts[currPt];
			interpPt.Y = yPts[currPt];

			normLength = (float) Math.sqrt((tangentDir.X * tangentDir.X)
					+ (tangentDir.Y * tangentDir.Y));
			normDir.X = -tangentDir.Y / normLength;
			normDir.Y = tangentDir.X / normLength;

			normStep.X = stepPct * normDir.X;
			normStep.Y = stepPct * normDir.Y;

			outNormPt.X = -normStep.X + interpPt.X;
			outNormPt.Y = -normStep.Y + interpPt.Y;

			outNormHalfPt.X = -normStep.X / 2f + interpPt.X;
			outNormHalfPt.Y = -normStep.Y / 2f + interpPt.Y;
			
			inNormPt.X = normStep.X + interpPt.X;
			inNormPt.Y = normStep.Y + interpPt.Y;

			inNormHalfPt.X = normStep.X / 2f + interpPt.X;
			inNormHalfPt.Y = normStep.Y / 2f + interpPt.Y;
			
			// normal line
			// VOILine kLine = new VOILine();
			// kLine.add(new Vector3f(outNormPt.X, outNormPt.Y, 0f));
			// kLine.add(new Vector3f(inNormPt.X, inNormPt.Y, 0f));
			// lineVOI.importCurve(kLine);
			
			for (int k = 0; k < 19; k++ ) {
				
				VOIBase squareOuter = new VOIContour(true);
				newXPts[0] = outNormPt.X + k * normStep.X / 19f - 32;
				newYPts[0] = outNormPt.Y + k * normStep.Y / 19f - 32;
				newXPts[1] = outNormPt.X + k * normStep.X / 19f + 32;
				newYPts[1] = outNormPt.Y + k * normStep.Y / 19f - 32;
				newXPts[2] = outNormPt.X + k * normStep.X / 19f + 32;
				newYPts[2] = outNormPt.Y + k * normStep.Y / 19f + 32;
				newXPts[3] = outNormPt.X + k * normStep.X / 19f - 32;
				newYPts[3] = outNormPt.Y + k * normStep.Y / 19f + 32;
				// squareOuter.importArrays(newXPts, newYPts, newZPts, 4);
				// squareVOI.importCurve(squareOuter);
				
				if ( training ) {
					if ( k == 0 ) {
						extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.NEG);
						// squareOuter.importArrays(newXPts, newYPts, newZPts, 4);
						// squareVOINeg.importCurve(squareOuter);
					} else {
						// extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.UNDEFINED);
					    // squareOuter.importArrays(newXPts, newYPts, newZPts, 4);
					    // squareVOIUncertain.importCurve(squareOuter);
					} 
				} else {
					if ( k == 0 ) {
						extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.NEG,
							outNormPt.X, outNormPt.Y);
					} else {
						extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.UNDEFINED,
								outNormPt.X + k * normStep.X / 19f, outNormPt.Y + k * normStep.Y / 19f);
					}
				}
				patchNum++;
			}
			
			// center square
			// VOIBase squareCenter = new VOIContour(true);
			newXPts[0] = interpPt.X - 32;
			newYPts[0] = interpPt.Y - 32;
			newXPts[1] = interpPt.X + 32;
			newYPts[1] = interpPt.Y - 32;
			newXPts[2] = interpPt.X + 32;
			newYPts[2] = interpPt.Y + 32;
			newXPts[3] = interpPt.X - 32;
			newYPts[3] = interpPt.Y + 32;
			// squareCenter.importArrays(newXPts, newYPts, newZPts, 4);
			// squareVOI.importCurve(squareCenter);
			
			if ( training ) {
				extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.POS);
			} else {
				extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.POS,
							interpPt.X, interpPt.Y);
			}
			patchNum++;
			
			
			for (int k = 18; k >=0; k--) {
				VOIBase squareInner = new VOIContour(true);
				newXPts[0] = inNormPt.X - k * normStep.X / 19f - 32;
				newYPts[0] = inNormPt.Y - k * normStep.Y / 19f - 32;
				newXPts[1] = inNormPt.X - k * normStep.X / 19f + 32;
				newYPts[1] = inNormPt.Y - k * normStep.Y / 19f - 32;
				newXPts[2] = inNormPt.X - k * normStep.X / 19f + 32;
				newYPts[2] = inNormPt.Y - k * normStep.Y / 19f + 32;
				newXPts[3] = inNormPt.X - k * normStep.X / 19f - 32;
				newYPts[3] = inNormPt.Y - k * normStep.Y / 19f + 32;
			    // squareInner.importArrays(newXPts, newYPts, newZPts, 4);
				// squareVOI.importCurve(squareInner);
				if (training) {
					if ( k == 0 ) {
						extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.NEG);
						// squareInner.importArrays(newXPts, newYPts, newZPts, 4);
						// squareVOINeg.importCurve(squareInner);
						
					
					} else {
						// extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.UNDEFINED);
						// squareInner.importArrays(newXPts,  newYPts,  newZPts, 4);
						// squareVOIUncertain.importCurve(squareInner);
					} 
				} else {
					if ( k == 0 ) {
						extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.NEG,
							inNormPt.X, inNormPt.Y);
					} else {
						extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], ClassType.UNDEFINED,
								inNormPt.X - k * normStep.X / 19f, inNormPt.Y - k * normStep.Y / 19f);
					}
				}
				patchNum++;
			}
			
			
			/*
            Vector3f centerStep = new Vector3f();
            centerStep.X = normStep.X / 6f;
            centerStep.Y = normStep.Y / 6f;
            // int [] moveX = {-1,  0,  1, -1, 1, -1, 0, 1 };
            // int [] moveY = {-1, -1, -1,  0, 0,  1, 1, 1 };
            int [] moveX = {  0, -1, 1, 0 };
            int [] moveY = { -1,  0, 0, 1 };
			for (int k = 0; k < 4; k++ ) {
				// VOIBase squareAround = new VOIContour(true);
				newXPts[0] = interpPt.X + (centerStep.X * moveX[k]) - 16;
				newYPts[0] = interpPt.Y + (centerStep.Y * moveY[k]) - 16;
				newXPts[1] = interpPt.X + (centerStep.X * moveX[k]) + 16;
				newYPts[1] = interpPt.Y + (centerStep.Y * moveY[k]) - 16;
				newXPts[2] = interpPt.X + (centerStep.X * moveX[k]) + 16;
				newYPts[2] = interpPt.Y + (centerStep.Y * moveY[k]) + 16;
				newXPts[3] = interpPt.X + (centerStep.X * moveX[k]) - 16;
				newYPts[3] = interpPt.Y + (centerStep.Y * moveY[k]) + 16;
				// squareAround.importArrays(newXPts, newYPts, newZPts, 4);
				// squareVOI.importCurve(squareAround);
				if ( training ) {
					extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], true);
				} else {
					extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], true,
							interpPt.X, interpPt.Y);
				}
				patchNum++;
			}
			*/
			/*
			// outer sqaure
			// VOIBase squareOuter = new VOIContour(true);
			newXPts[0] = outNormPt.X - 16;
			newYPts[0] = outNormPt.Y - 16;
			newXPts[1] = outNormPt.X + 16;
			newYPts[1] = outNormPt.Y - 16;
			newXPts[2] = outNormPt.X + 16;
			newYPts[2] = outNormPt.Y + 16;
			newXPts[3] = outNormPt.X - 16;
			newYPts[3] = outNormPt.Y + 16;
			// squareOuter.importArrays(newXPts, newYPts, newZPts, 4);
			// squareVOI.importCurve(squareOuter);
			// cropImage((int)newXPts[0], (int)newXPts[2], (int)newYPts[0],
			// (int)newYPts[2]);
			if ( training ) {
				extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], false);
			} else {
				extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], false,
						outNormPt.X, outNormPt.Y);
			}
			// saveImage(false);
			// cropImage.disposeLocal();
			// cropImage = null;
			patchNum++;

			
			// inner square
			// VOIBase squareInner = new VOIContour(true);
			newXPts[0] = inNormPt.X - 16;
			newYPts[0] = inNormPt.Y - 16;
			newXPts[1] = inNormPt.X + 16;
			newYPts[1] = inNormPt.Y - 16;
			newXPts[2] = inNormPt.X + 16;
			newYPts[2] = inNormPt.Y + 16;
			newXPts[3] = inNormPt.X - 16;
			newYPts[3] = inNormPt.Y + 16;
			// squareInner.importArrays(newXPts, newYPts, newZPts, 4);
			// squareVOI.importCurve(squareInner);
			// cropImage((int)newXPts[0], (int)newXPts[2], (int)newYPts[0],
			// (int)newYPts[2]);
			if ( training ) {
				extractImage8bitsTrain((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], false);
			} else {
				extractImage8bitsTest((int) newXPts[0], (int) newXPts[2], (int) newYPts[0], (int) newYPts[2], false,
						inNormPt.X, inNormPt.Y);
			}
		
			// saveImage(false);
			// cropImage.disposeLocal();
			// cropImage = null;
			patchNum++;
			
			*/
		   
		}
		// new ViewJFrameImage(srcImage);
		// pause();

		interpPt = null;
		inNormPt = new Vector3f();
		outNormPt = new Vector3f();
		tangentDir = new Vector3f();
		normDir = new Vector3f();
		normStep = new Vector3f();

		// zero out the z dimension VOI
		xPts = null;
		yPts = null;
		zPts = null;

		newXPts = null;
		newYPts = null;
		newZPts = null;

	}
	
	public static void pause() {
		System.err.println("enter to continue: ");
		try {
			for ( int av = System.in.available(); av > 0; av-- ) {
				System.in.read();
			}
			System.in.read();	
		} catch ( IOException e ) {
			System.err.println("keyboard failed: " + e );
		}
	}
	
	public int numDigits(int value) {
		int numDigit = 0;
		while ( value >= 1 ) {
			numDigit++;
			value /= 10;
		}
		return numDigit;
	}
	
	private void extractImage8bitsTest(int xMin, int xMax, int yMin, int yMax, ClassType type, float centerX, float centerY) {
		// int size = 32*32;
		// int []buffer = new int[size];
		// int []ext = new int[2];
		// ext[0] = 32;
		// ext[1] = 32;
		// ModelImage result = new ModelImage(srcImage.getType(), ext, "extract");
		// if ( (xMax-xMin) > 64 ) xMax = xMin+64;
		// if ( (yMax-yMin) > 64 ) yMax = yMin+64;
		
		int []ext = srcImage.getExtents();
		int xDim = ext[0];
		int yDim = ext[1];
		
		if ( (xMax-xMin) > 64 || (xMax-xMin) < 64) xMax = xMin+64;
		if ( (yMax-yMin) > 64 || (yMax-yMin) < 64) yMax = yMin+64;
		
		// if ( xMin == 0 || xMax == 0 ) return;
		if ( xMin <= 0 || xMax <= 0 ) return;
		if ( yMin >= 512 || yMax >= 512 ) return;
		
		int x, y;
		// String sliceDir = savedImageDir;
		String imgName;
		File file = null;
		// savedImageDir = "C:\\DeepLearning\\"; 
		File dir = new File(savedImageDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}
		
		// padding 0 to the lexicographic order. 
		String sliceString, slicePadding = "";
		String patchString, patchPadding = "";
		
		int numDigitSlice = 5 - numDigits(sliceNum);
		int numDigitPatch = 5 - numDigits(patchNum);
		
		for ( int i = 0; i < numDigitSlice; i++ ) {
			slicePadding += "0";
		}
		for (int i = 0; i < numDigitPatch; i++ ) {
			patchPadding += "0";
		}
		
		sliceString = slicePadding + sliceNum;
		patchString = patchPadding + patchNum;
		
		if ( type == ClassType.POS ) {
			imgName = "img" + imageNum + "_slice" + sliceString + "_patch" + patchString + "_pos_" + ".png";
		} else if ( type == ClassType.NEG){
			imgName = "img" + imageNum + "_slice" + sliceString + "_patch" + patchString + "_neg_" + ".png";
		} else {  // type == ClassType.UNDEFINED
			imgName = "img" + imageNum + "_slice" + sliceString + "_patch" + patchString + "_undefined_" + ".png";
		}
		
		try {
			
			boolean alpha = false;
			boolean gray = true;
			boolean indexed = false;
			ImageInfo imi = new ImageInfo(64, 64, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			
			
			// OutputStream os = new FileOutputStream(savedImageDir + File.separator + imgName);
			file = new File(savedImageDir + File.separator + imgName);
			if ( !file.exists() ) {
				file.createNewFile();
			}
			
			OutputStream os = new FileOutputStream(file);
			
			PngWriter pngw = new PngWriter(os, imi);
			
			
			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					float intensity;
					if ( j < 0 || i < 0 || j > yDim || i > xDim ) {
							intensity = 0;
					} else {
							intensity = srcImage.getFloat(i, j); 
					}
					float r = 0;
					if (intensity >= minIntensity && intensity <= maxIntensity) {
						r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
					} else if (intensity > maxIntensity)
						r = 255;
					else if (intensity < minIntensity)
						r = 0;
					
					// buffer[y * 32 + x] = (int) r;
					
					ImageLineHelper.setPixelGray8(line, x, (int)r);
					
				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
			
		
		    pngw.end();
		    pngw.close();
		    pngw = null;
		    os.close();
		    os = null;
		    line = null;
		    imi = null;
		    
		    outStream.write(imgName + " " + centerX + " " + centerY + "\n");
		    
			// System.err.println("testing array");
			
		} catch ( Exception e ) {
			System.err.println("imageNum = " + imageNum + "sliceNum = "+ sliceNum + "patchNum = " + patchNum);
			System.err.println("image find worong: " + file.getAbsolutePath());
			e.printStackTrace();
			System.exit(1);
			
		}
	
		// try {
	    //   result.importData(0, buffer, true);
	    //    new ViewJFrameImage(result);
	    // } catch ( IOException e ) {
	    //	e.printStackTrace();
	    // }	
	}
	
	
	private void extractImage8bitsTrain(int xMin, int xMax, int yMin, int yMax, ClassType type) {
		// int size = 32*32;
		// int []buffer = new int[size];
		// int []ext = new int[2];
		// ext[0] = 32;																																																																																																																																																
		// ext[1] = 32;
		// ModelImage result = new ModelImage(srcImage.getType(), ext, "extract");
		
		if ( (xMax-xMin) > 64 || (xMax-xMin) < 64) xMax = xMin+64;
		if ( (yMax-yMin) > 64 || (yMax-yMin) < 64) yMax = yMin+64;
		
		int []ext = srcImage.getExtents();
		int xDim = ext[0];
		int yDim = ext[1];
		
	    // xMax = xMin+64;
		// yMax = yMin+64;
		
		if ( xMin <= 0 || xMax <= 0 ) return;
		if ( yMin >= 512 || yMax >= 512 ) return;
		
		int x, y;
		// String sliceDir = "C:\\TrainMorePatches8bits_10%_10%" + File.separator;
		// String sliceDir = "C:\\DeepLearning" + File.separator;
		String imgName;
		File file = null;
		File dir = new File(savedImageDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}
		
		if ( type == ClassType.POS ) {
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_pos_" + ".png";
		} else if ( type == ClassType.NEG ){
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_neg_" + ".png";
		} else {   // type == ClassType.UNDEFINED
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_undefined_" + ".png";
		}
		
		// if ( imageNum == 14 && sliceNum == 64 && patchNum == 394 ) {
		// 	System.err.println("debug");
		// }
 		
		try {
			
			boolean alpha = false;
			boolean gray = true;
			boolean indexed = false;
			ImageInfo imi = new ImageInfo(64, 64, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			
			file = new File(savedImageDir + File.separator + imgName);
			if ( !file.exists() ) {
			 	file.createNewFile();
			}
			
			
			OutputStream os = new FileOutputStream(file);
			
			PngWriter pngw = new PngWriter(os, imi);
			
			// System.err.println("xMin = " + xMin + "  xMax = " + xMax);
			
			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					float intensity;
					if ( j < 0 || i < 0 || j > yDim || i > xDim ) {
						intensity = 0;
					} else {
						// if ( imageNum == 14 && sliceNum == 64 && patchNum == 394 ) {
						// 	System.err.println("i = " + i + " j = " + j);
						// }
						intensity = srcImage.getFloat(i, j);
					}
					float r = 0;
					if (intensity >= minIntensity && intensity <= maxIntensity) {
						r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
					} else if (intensity > maxIntensity)
						r = 255;
					else if (intensity < minIntensity)
						r = 0;
					
					// buffer[y * 32 + x] = (int) r;
					
					ImageLineHelper.setPixelGray8(line, x, (int)r);
					
				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
		    pngw.end();
		    pngw.close();
		    pngw = null;
		    os.close();
		    os = null;
		    line = null;
		    imi = null;
			// System.err.println("testing array");
			
		} catch ( Exception e ) {
			// System.err.println("imageNum = " + imageNum + "sliceNum = "+ sliceNum + "patchNum = " + patchNum);
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();
			System.exit(1);
		}
	     
		// try {
	    //   result.importData(0, buffer, true);
	    //  new ViewJFrameImage(result);
	    // } catch ( IOException e ) {
	    //	e.printStackTrace();
	    // }	
	}
	
	/*
	private void extractImage8bitsImageIO(int xMin, int xMax, int yMin, int yMax, boolean isCenter) {
		// int []ext = new int[2];
		// ext[0] = 32;
		// ext[1] = 32;
		// int size = 32*32;
		// int []buffer = new int[size];
		int x, y;
		// ModelImage result = new ModelImage(srcImage.getType(), ext, "extract");
		
		BufferedImage tempImage = new BufferedImage(32, 32, BufferedImage.TYPE_BYTE_GRAY);
		WritableRaster raster = tempImage.getRaster();
		
		String sliceDir = "C:\\DeepLearning" + File.separator;
		String imgName;
		File dir = new File(sliceDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}
		
		if ( isCenter ) {
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_pos_" + ".png";
		} else {
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_neg_" + ".png";
		}
		
		try {
			
			File file = new File(sliceDir + File.separator + imgName);
			
			double max = srcImage.getMax();
			// System.err.println("image max = " + srcImage.getMax()  + "   image min = " + srcImage.getMin());
			for (int i = xMin; i < xMax; i++) {
				
				x = i - xMin;
				
				for (int j = yMin; j < yMax; j++) {
					
					y = j - yMin;

					float intensity = srcImage.getFloat(i, j);
					
					int r = (int) (Math.round(255.0f * intensity / max));
					
					// buffer[y * 32 + x] = (int) srcImage.getFloat(i, j);
				    raster.setSample(x, y, 0, (byte)(r & 0xff));
				}
				System.err.println();
			}

			System.err.println("testing array");
			
			ImageIO.write(tempImage, "png", file);
			raster = null;
			tempImage = null;
			// gray = null;
			// array = null;
			// byteBuffer = null;
			
			
		} catch ( Exception e ) {
			e.printStackTrace();
		}
	
		// try {
	    //    result.importData(0, buffer, true);
	    //   new ViewJFrameImage(result);
	    // } catch ( IOException e ) {
		//   	e.printStackTrace();
	    // }	
	}
	*/
	
	private void extractImage24bits(int xMin, int xMax, int yMin, int yMax, boolean isCenter) {
		int []ext = new int[2];
		ext[0] = 32;
		ext[1] = 32;
		int size = 32*32;
		int []buffer = new int[size];
		int x, y;
		ModelImage result = new ModelImage(srcImage.getType(), ext, "extract");
		
		BufferedImage tempImage = new BufferedImage(32, 32,BufferedImage.TYPE_INT_RGB);
		
		String sliceDir = "C:\\DeepLearning" + File.separator;
		String imgName;
		File dir = new File(sliceDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}
		
		if ( isCenter ) {
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_pos_" + ".png";
			// System.err.println(sliceDir + File.separator + imgName);
		} else {
			imgName = "img" + imageNum + "_slice" + sliceNum + "_patch" + patchNum + "_neg_" + ".png";
		}
		
		try {
			File file = new File(sliceDir + File.separator + imgName);
			
			double max = srcImage.getMax();
			System.err.println("image max = " + srcImage.getMax()  + "   image min = " + srcImage.getMin());
			for (int i = xMin; i < xMax; i++) {
				
				x = i - xMin;
				
				for (int j = yMin; j < yMax; j++) {
					
					y = j - yMin;

					float intensity = srcImage.getFloat(i, j);
					intensity = 100;
					int r =  (int) (Math.round(255.0f * intensity / max));
					int g = r; // (int) (( intensity / max ) * 255f);
					int b = r; // (int) (( intensity / max ) * 255f);
					
            		int newRGB = (r << 16) | (g << 8) | b;
            		// newRGB = 100;
				    buffer[y * 32 + x] = (int) srcImage.getFloat(i, j);
					
					tempImage.setRGB(x, y, newRGB);
				}
			}
			
			ImageIO.write(tempImage, "png", file);
			tempImage = null;
			
		} catch ( IOException e ) {
			e.printStackTrace();
		}
	
		try {
	       result.importData(0, buffer, true);
	       new ViewJFrameImage(result);
	    } catch ( IOException e ) {
		  	e.printStackTrace();
	    }	
	}
	
	private void cropImage(int xMin, int xMax, int yMin, int yMax) {

		int zDim;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
			
		zDim = 1;
			
		xBounds[0] = xMin;
		xBounds[1] = xMax;

		yBounds[0] = yMin;
		yBounds[1] = yMax;

		zBounds[0] = 0;
		zBounds[1] = zDim;

			try {
				int[] destExtents = new int[2];
				destExtents = new int[2];
				destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
				destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
			    // System.err.println("destExtents[0] = " + destExtents[0] + "  destExtents[1] = " + destExtents[1]);
			    
			    cropImage = new ModelImage(srcImage.getType(), destExtents, "crop");
			    
				int[] xCrop = new int[] { 0, 0 };
				int[] yCrop = new int[] { 0, 0 };
				int[] zCrop = new int[] { 0, 0 };
				if (destExtents.length > 0) {
					xCrop[0] = -1 * xBounds[0];
					xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
				}
				if (destExtents.length > 1) {
					yCrop[0] = -1 * yBounds[0];
					yCrop[1] = -1 * (yBounds[1] - destExtents[1] - 1);
				}
				if (destExtents.length > 2) {
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] - destExtents[2] - 1);
				} else // 3D to 2D
				{
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] + 1);
				}

				// System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = " + xCrop[1]);
				// System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = " + yCrop[1]);

			    cropAlgo = new AlgorithmAddMargins(srcImage, cropImage, xCrop, yCrop, zCrop);
				cropAlgo.run();
				/*
			    double inMin = cropImage.getMin();
			    double inMax = cropImage.getMax();
				
				resultImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "_ushort");
				resultImage.setType(ModelStorageBase.USHORT);
				resultImage.reallocate(ModelStorageBase.USHORT);
			
				AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(resultImage, cropImage, inMin, inMax, inMin, inMax, true);
				changeTypeAlgo.run();
				// new ViewJFrameImage(cropImage);
				// new ViewJFrameImage(resultImage);
                */
				
				destExtents = null;
				xCrop = null;
				yCrop = null;
				zCrop = null;
				
				xBounds = null;
				yBounds = null;
				zBounds = null;
				
			} catch (OutOfMemoryError e) {
				MipavUtil.displayError("Dialog Crop: unable to allocate enough memory");
				return;
			}
	

		// crop target image

	}
	
	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 30, false);
			// smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}
	
	public void smoothVOI128(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 128, false);
			// smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}
	
}