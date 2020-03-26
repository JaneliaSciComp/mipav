package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmRegLeastSquares. Selects image is match image, the image that gets transformed
 * until it is registered to the base image. Algorithms are executed in their own thread.
 *
 * @version  0.1 May 19, 1999
 * @author   Delia McGarry
 */
public class JDialogRegistrationLeastSquares extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2171665599919057675L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Base image - register match image to base image. */
    protected ModelImage baseImage;

    /** Combo box with image names for choosing base image. */
    private JComboBox comboBoxImage;

    /** Number of dimensions in match image. */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean fromOAR3D = false;

    /** DOCUMENT ME! */
    private boolean lsCompleted = false;

    /** Algorithm to run from this dialog. */
    private AlgorithmRegLeastSquares LSMatch = null;

    /** Match image - register match image to base image. */
    protected ModelImage matchImage;

    /** Result image - image returned from registration algorithm. */
    protected ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private TransMatrix resultMatrix = null;

    /** Used to lock and unlock images. */
    private String[] titles;

    /** Reference to userface. */
    private ViewUserInterface userInterface;

    /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;

    /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;
    
    private JLabel outOfBoundsLabel;
    
    private JComboBox outOfBoundsComboBox;
    
    private JLabel valueLabel;
    
    private JTextField valueText;
    
    private double imageMin;
    
    private double imageMax;
    
    private int dataType;
    
    /**
     * Tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image maximum
     */
    private int outOfBoundsIndex = 0;
    
    private float fillValue = 0.0f;
    
    private JLabel matrixLabel;
    
    private JComboBox matrixComboBox;
    
    private String matrixDirectory;
    
    private JLabel userDirectoryLabel;
    
    private JTextField userDirectoryText;
    
    private boolean haveNonPointVOI = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationLeastSquares() { }

    /**
     * Creates new registration dialog to get base image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationLeastSquares(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    /**
     * Creates a new JDialogRegistrationLeastSquares object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  _mi             DOCUMENT ME!
     * @param  _ri             DOCUMENT ME!
     */
    public JDialogRegistrationLeastSquares(Frame theParentFrame, ModelImage _mi, ModelImage _ri) {
        matchImage = _mi;
        baseImage = _ri;
        DIM = 3;
        userInterface = ViewUserInterface.getReference();
        setSeparateThread(false);
        fromOAR3D = true;
        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, set variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10040");
            MipavUtil.showWebHelp("Registration:_Landmark-Least_Squares");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegLeastSquares) {

            if (LSMatch.isCompleted() == true) {
                lsCompleted = true;
                matchImage.setMatrix(LSMatch.getTransformBtoA());
                Preferences.debug(matchImage.getMatrix().toString(),Preferences.DEBUG_ALGORITHM);
                LSMatch.getTransformBtoA().saveMatrix(matrixDirectory + matchImage.getImageName() +
                                                      "_To_" + baseImage.getImageName() + ".mtx");
                Preferences.debug("Saved " + matrixDirectory + matchImage.getImageName() + "_To_" +
                        baseImage.getImageName() + ".mtx\n",Preferences.DEBUG_FILEIO);
                LSMatch.calculateResiduals();
                xdimA = baseImage.getExtents()[0];
                ydimA = baseImage.getExtents()[1];

                String name = makeImageName(matchImage.getImageName(), "_register");

                if (DIM == 2) {
                    int[] extents = new int[] { xdimA, ydimA };
                    float[] resolutions = new float[] { xresA, yresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name);
                    resultImage.getFileInfo(0).setResolutions(resolutions);

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformBilinear(matchImage, resultImage, LSMatch.getTransformBtoA(), null, true,
                                                             fillValue);
                    } else {
                        AlgorithmTransform.transformBilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              xdimA, ydimA, xresA, yresA, fillValue);
                    }
                    if (haveNonPointVOI) {
                    	float imgBuffer[] = new float[matchImage.getExtents()[0]*matchImage.getExtents()[1]];
                        transform2DVOI(matchImage, resultImage, imgBuffer, LSMatch.getTransformBtoA());	
                    }

                } else if (DIM == 3) {

                    if (fromOAR3D) {
                        resultMatrix = LSMatch.getTransformBtoA();
                    }

                    zdimA = baseImage.getExtents()[2];

                    int[] extents = new int[] { xdimA, ydimA, zdimA };
                    float[] resolutions = new float[] { xresA, yresA, zresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name);

                    for (int i = 0; i < zdimA; i++) {
                        resultImage.getFileInfo(i).setResolutions(resolutions);
                    }

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformTrilinear(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              null, true, fillValue);
                    } else {
                        AlgorithmTransform.transformTrilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                               xdimA, ydimA, zdimA, xresA, yresA, zresA, fillValue);
                    }
                    if (haveNonPointVOI) {
                    	float imgBuffer[] = new float[matchImage.getExtents()[0]*matchImage.getExtents()[1]*matchImage.getExtents()[2]];
                        transform3DVOI(matchImage, resultImage, imgBuffer, LSMatch.getTransformBtoA());	
                    }
                }

                resultImage.calcMinMax();

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                matchImage.notifyImageDisplayListeners(null, true);

                if (resultImage != null) {

                    try {
                        resultImage.setImageName("LS Transformed image");
                        updateFileInfo(baseImage, resultImage);

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                    
                    resultImage.getMatrixHolder().replaceMatrices(baseImage.getMatrixHolder().getMatrices());

                } else {
                    MipavUtil.displayError("Result Image is null");
                }

                insertScriptLine();
            }
        }

        if (!fromOAR3D) {
            dispose();
        }
    }
    
    private void transform2DVOI(final ModelImage image, ModelImage destImage, final float[] imgBuffer, final TransMatrix trans) {

        int i, j;
        int X0pos, Y0pos;
        float X, Y;
        float temp1, temp2;
        float value;
        float imm, jmm;
        int roundX, roundY;
        int index;
        int index2;
        int indexC;
        int iXdim = image.getExtents()[0];
        int iYdim = image.getExtents()[1];
        int length = iXdim * iYdim;
        float iXres = image.getFileInfo()[0].getResolutions()[0];
        float iYres = image.getFileInfo()[0].getResolutions()[1];
        int index2Size;
        ModelImage maskImage;
        float fillValue = 0.0f;
        int oXdim = destImage.getExtents()[0];
        int oYdim = destImage.getExtents()[1];
        float oXres = destImage.getFileInfo()[0].getResolutions()[0];
        float oYres = destImage.getFileInfo()[0].getResolutions()[1];

        float T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask;
        VOIVector voiVector;


        TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = -1;
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		VOIBaseVector curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else if (presentVOI.getCurveType() == VOI.POINT) {
        		continue;
        	}
        	else {
        		index2Size = 1;
        	}
        	for (index2 = 0; index2 < index2Size; index2++) {
        		indexC++;
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		        
		
		        try {
		            maskImage.exportData(0, length, imgBuffer); // locks and releases lock
		            
		        } catch (final IOException error) {
		            MipavUtil.displayError("maskImage.exportData: Image(s) locked");
		
		            return;
		        }
		
		        for (i = 0; (i < oXdim); i++) {
		
		            imm = i * oXres;
		            temp1 = (imm * T00) + T02;
		            temp2 = (imm * T10) + T12;
		
		            for (j = 0; (j < oYdim); j++) {
		
		                // transform i,j
		
		                jmm = j * oYres;
		                value = fillValue; // if transformed out of bounds.
		                X = (temp1 + (jmm * T01)) / iXres;
		                roundX = (int) (X + 0.5f);
		
		                if ( (X >= -0.5f) && (X < iXdim)) {
		                    Y = (temp2 + (jmm * T11)) / iYres;
		                    roundY = (int) (Y + 0.5f);
		
		                    if ( (Y >= -0.5f) && (Y < iYdim)) {
		                        X0pos = Math.min(roundX, iXdim - 1);
		                        Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
		                        value = imgBuffer[Y0pos + X0pos];
		                    } // end if Y in bounds
		                } // end if X in bounds
		
		                tmpMask.set(i, j, value);
		            } // end for j
		        } // end for i
		
		        
		        // ******* Make algorithm for VOI extraction.
		        tmpMask.calcMinMax();
		
		        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
		
		        VOIExtAlgo.run();
		        VOIExtAlgo.finalize();
		        VOIExtAlgo = null;
		        destImage.addVOIs(tmpMask.getVOIs());
		        tmpMask.resetVOIs();
		        for (j = 0; j < oYdim; j++) {
	        		for (i = 0; i < oXdim; i++) {
	        			tmpMask.set(i, j, fillValue);
	        		}
	        	}
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        tmpMask.disposeLocal();
        tmpMask = null;
        maskImage.disposeLocal();
        maskImage = null;
       
    }
    
    private void transform3DVOI(final ModelImage image, ModelImage destImage, final float[] imgBuffer, final TransMatrix trans) {
    	int i, j, k, z;
        int X0pos, Y0pos, Z0pos;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        int index;
        int index2;
        int indexC;
        int iXdim = image.getExtents()[0];
        int iYdim = image.getExtents()[1];
        int iZdim = image.getExtents()[2];
        int sliceSize = iXdim * iYdim;
        int length = sliceSize * iZdim;
        float iXres = image.getFileInfo()[0].getResolutions()[0];
        float iYres = image.getFileInfo()[0].getResolutions()[1];
        float iZres = image.getFileInfo()[0].getResolutions()[2];
        int index2Size;
        ModelImage maskImage;
        float fillValue = 0.0f;
        int oXdim = destImage.getExtents()[0];
        int oYdim = destImage.getExtents()[1];
        int oZdim = destImage.getExtents()[2];
        float oXres = destImage.getFileInfo()[0].getResolutions()[0];
        float oYres = destImage.getFileInfo()[0].getResolutions()[1];
        float oZres = destImage.getFileInfo()[0].getResolutions()[2];
        VOIBaseVector curves = null;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        int zFound[] = new int[iZdim];
        boolean duplicateZ = false;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        ModelImage tmpMask = null;
        VOIVector voiVector;

        TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;
        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (z = 0; z < oZdim; z++) {
        	for (j = 0; j < oYdim; j++) {
        		for (i = 0; i < oXdim; i++) {
        			tmpMask.set(i, j, z, fillValue);
        		}
        	}
        }
      
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else if (presentVOI.getCurveType() == VOI.POINT) {
        		continue;
        	}
        	else {
        		index2Size = 1;
        	}
            for (i = 0; i < iZdim; i++) {
            	zFound[i] = 0;
            }
        	for (index2 = 0; index2 < index2Size; index2++) {
        		if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		    curves.get(index2).getBounds(xBounds, yBounds, zBounds);	
        		}
        		else {
        			presentVOI.getBounds(xBounds, yBounds, zBounds);
        		}
        		duplicateZ = false;
        		for (i = zBounds[0]; i <= zBounds[1]; i++) {
        			zFound[i]++;
        			if (zFound[i] >= 2) {
        				duplicateZ = true;
        			}
        		}
        		if (duplicateZ) {
        			indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, fillValue);
			        		}
			        	}
			        }
			        index2--;
			        continue;
		        }
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
	            try {
	                maskImage.exportData(0, length, imgBuffer); // locks and releases lock
	            } catch (final IOException error) {
	                MipavUtil.displayError("maskImage.exportData: Image(s) locked");

	                return;
	            }
		        
		        
		        for (k = 0; (k < oZdim); k++) {

		            kmm = k * oZres;
		            k1 = (kmm * T02) + T03;
		            k2 = (kmm * T12) + T13;
		            k3 = (kmm * T22) + T23;

		            for (j = 0; (j < oYdim); j++) {

		                jmm = j * oYres;
		                j1 = (jmm * T01) + k1;
		                j2 = (jmm * T11) + k2;
		                j3 = (jmm * T21) + k3;

		                for (i = 0; (i < oXdim); i++) {

		                    // transform i,j,k

		                    imm = i * oXres;
		                    value = fillValue; // if voxel is transformed out of bounds
		                    X = (j1 + (imm * T00)) * invXRes;

		                    if ( (X >= -0.5) && (X < iXdim)) {
		                        Y = (j2 + (imm * T10)) * invYRes;

		                        if ( (Y >= -0.5) && (Y < iYdim)) {
		                            Z = (j3 + (imm * T20)) * invZRes;

		                            if ( (Z >= -0.5) && (Z < iZdim)) {
		                                X0pos = Math.min((int) (X + 0.5f), iXdim - 1);
		                                Y0pos = Math.min((int) (Y + 0.5f), iYdim - 1) * iXdim;
		                                Z0pos = Math.min((int) (Z + 0.5f), iZdim - 1) * sliceSize;
		                                value = imgBuffer[Z0pos + Y0pos + X0pos];
		                            } // end if Z in bounds
		                        } // end if Y in bounds
		                    } // end if X in bounds

		                    if (value != fillValue) {
		                        tmpMask.set(i, j, k, value);
		                    }
		                } // end for i
		            } // end for j
		        } // end for k
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, fillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;

        
    }

    /**
     * Accessor that returns whether or not the algorithm successfully completed.
     *
     * @return  boolean
     */
    public boolean getLSCompleted() {
        return lsCompleted;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Gets the result matrix (only used by OAR3D).
     *
     * @return  TransMatrix
     */
    public TransMatrix getResultMatrix() {
        return resultMatrix;
    }

    /**
     * Sets arrays appropriately and calls registration algorithm, running it in it's own thread.
     */
    protected void callAlgorithm() {
        int nPtsA = 0; // = baseImage.getVOIs().size();
        int nPtsB = 0; // = matchImage.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i;
        int j;
        int num;
        Vector<VOIBase> curves;
        VOIVector voiVector;
        VOI presentVOI;
        int nDims;

        try {

            if (baseImage.getVOIs().size() == 0) {
                MipavUtil.displayError("Select points before clicking OK");

                return;
            }
            
            if (baseImage.getNDims() != matchImage.getNDims()) {
            	MipavUtil.displayError("baseImage and matchImage must have the same number of dimensions");
            }
        	nDims = baseImage.getNDims();
        	voiVector = baseImage.getVOIs();
            for (i = 0; i < voiVector.size(); i++) {
                presentVOI = baseImage.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	curves = presentVOI.getCurves();
                	nPtsA += curves.size();
                }
            }
  
            Preferences.debug("nPtsA = " + nPtsA + "\n",Preferences.DEBUG_ALGORITHM);
            ptA = new Vector3f[nPtsA];
            for (i = 0, num = 0; i < voiVector.size(); i++) {
                presentVOI = baseImage.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	tmpptA = presentVOI.exportAllPoints();
                	for (j = 0; j < tmpptA.length; j++) {
                		ptA[num++] = tmpptA[j];
                	}
                }
            }

            voiVector = matchImage.getVOIs();
            for (i = 0; i < voiVector.size(); i++) {
                presentVOI = matchImage.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	curves = presentVOI.getCurves();
                	nPtsB += curves.size();
                }
                else {
                	haveNonPointVOI = true;
                }
            }

            if (nPtsA != nPtsB) {
                MipavUtil.displayError("Both images must have the same number of points");

                return;
            }

            Preferences.debug("nPtsB = " + nPtsB + "\n",Preferences.DEBUG_ALGORITHM);
            ptB = new Vector3f[nPtsB];
            for (i = 0, num = 0; i < voiVector.size(); i++) {
                presentVOI = matchImage.getVOIs().VOIAt(i);
                if (presentVOI.getCurveType() == VOI.POINT) {
                	tmpptB = presentVOI.exportAllPoints();
                	for (j = 0; j < tmpptB.length; j++) {
                		ptB[num++] = tmpptB[j];
                	}
                }
            }
        
            

            if ((nPtsA < (nDims+1)) || (nPtsB < (nDims+1))) {
                MipavUtil.displayError("Must select at least " + (nDims + 1) + " points.");

                return;
            }

            Vector3f[] ptAmm = new Vector3f[nPtsA];
            Vector3f[] ptBmm = new Vector3f[nPtsB];
            zresA = 1;
            xresA = baseImage.getFileInfo(0).getResolutions()[0];
            yresA = baseImage.getFileInfo(0).getResolutions()[1];

            if (baseImage.getNDims() == 3) {
                zresA = baseImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsA; i++) {
                ptAmm[i] = new Vector3f((ptA[i].X * xresA), (ptA[i].Y * yresA),
                                        (ptA[i].Z * zresA));
                Preferences.debug(ptAmm[i].X + ", " + ptAmm[i].Y + ", " + ptAmm[i].Z + "\n",Preferences.DEBUG_ALGORITHM);
            }

            xresB = matchImage.getFileInfo(0).getResolutions()[0];
            yresB = matchImage.getFileInfo(0).getResolutions()[1];

            if (matchImage.getNDims() == 3) {
                zresB = matchImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsB; i++) {
                ptBmm[i] = new Vector3f((ptB[i].X * xresB), (ptB[i].Y * yresB),
                                        (ptB[i].Z * zresB));
                Preferences.debug(ptBmm[i].X + ", " + ptBmm[i].Y + ", " + ptBmm[i].Z + "\n",Preferences.DEBUG_ALGORITHM);
            }

            LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Register Least Squares: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        LSMatch.addListener(this);

        createProgressBar(baseImage.getImageName(), LSMatch);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        // Start the thread as a low priority because we wish to still have
        // user interface work fast
        // if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false){
        // MipavUtil.displayError("A thread is already running on this object", "Error");

        if (isRunInSeparateThread()) {

            if (LSMatch.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            LSMatch.run();
        }


    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }
    
    /**
     * tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image max 
     * @param outOfBoundsIndex
     */
    public void setOutOfBoundsIndex(int outOfBoundsIndex) {
        this.outOfBoundsIndex = outOfBoundsIndex;
    }
    
    /**
     * Accessor to set intensity value for out of bounds data
     * @param fillValue
     */
    public void setFillValue(float fillValue) {
        this.fillValue = fillValue;
    }
    
    /**
     * Accessor to set directory in which the matrix file is stored
     * @param matrixDirectory
     */
    public void setMatrixDirectory(String matrixDirectory) {
    	this.matrixDirectory = matrixDirectory;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
        baseImage = scriptParameters.retrieveImage("reference_image");

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = matchImage.getParentFrame();
        setOutOfBoundsIndex(scriptParameters.getParams().getInt("out_of_bounds_index"));
        switch(outOfBoundsIndex) {
            case 0: 
                setFillValue((float)imageMin);
                break;
            case 1: 
                if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                    setFillValue(Float.NaN);
                }
                else {
                    setFillValue(0.0f);
                }
                break;
            case 2:
                setFillValue(scriptParameters.getParams().getFloat("fill_value"));
                break;
            case 3:
                setFillValue((float)imageMax);
                break;
        }
        setMatrixDirectory(scriptParameters.getParams().getString("matrix_directory"));
    }
    

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(baseImage, "reference_image");

        scriptParameters.storeImageInRecorder(getResultImage());
        scriptParameters.getParams().put(ParameterFactory.newParameter("out_of_bounds_index", outOfBoundsIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("matrix_directory", matrixDirectory));
    }

    /**
     * Initializes GuserInterface components and displays dialog.
     */
    protected void init() {
        setForeground(Color.black);
        setTitle("Least Squares Registration");

        JPanel imagePanel = buildImagePanel();
        
        JPanel extentsPanel = buildExtentsPanel();

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(extentsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    protected JPanel buildImagePanel() {
    	matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
    	String matchName = matchImage.getImageName();

        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImgComboBox(matchImage);

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(comboBoxImage);
        
        return imagePanel;
    }
    
    protected JPanel buildExtentsPanel() {
    	JPanel extentsPanel = new JPanel(new GridBagLayout());
        outOfBoundsLabel = new JLabel("Out of bounds data:");
        outOfBoundsLabel.setForeground(Color.black);
        outOfBoundsLabel.setFont(serif12);
        outOfBoundsLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox = new JComboBox();
        outOfBoundsComboBox.setFont(serif12);
        outOfBoundsComboBox.setBackground(Color.white);
        outOfBoundsComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox.addItem("Image minimum");
        outOfBoundsComboBox.addItem("If float NaN, else 0");
        outOfBoundsComboBox.addItem("User defined");
        outOfBoundsComboBox.addItem("Image maximum");
        outOfBoundsComboBox.setSelectedIndex(0);
        outOfBoundsComboBox.addItemListener(this);
        
        valueLabel = new JLabel("Out of bounds intensity value:");
        valueLabel.setForeground(Color.black);
        valueLabel.setFont(serif12);
        valueLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        valueText = new JTextField(String.valueOf(imageMin));
        valueText.setFont(serif12);
        valueText.setEnabled(false);
        
        matrixLabel = new JLabel("Matrix file directory");
        matrixLabel.setForeground(Color.black);
        matrixLabel.setFont(serif12);
        matrixLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        matrixComboBox = new JComboBox();
        matrixComboBox.setFont(serif12);
        matrixComboBox.setBackground(Color.white);
        matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        baseImage = userInterface.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        
        if (baseImage != null) {
            matrixComboBox.addItem(baseImage.getImageDirectory());	
        }
        if ((matchImage.getImageDirectory() != null) && 
        	(!baseImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
        	matrixComboBox.addItem(matchImage.getImageDirectory());
        }
        if ((userInterface.getDefaultDirectory() != null) && 
        	(!userInterface.getDefaultDirectory().equals(baseImage.getImageDirectory())) &&
        	(!userInterface.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
        	matrixComboBox.addItem(userInterface.getDefaultDirectory());
        }
        matrixComboBox.addItem("User specified matrix directory");
        matrixComboBox.setSelectedIndex(0);
        
        userDirectoryLabel = new JLabel("User specified matrix directory");
        userDirectoryLabel.setForeground(Color.black);
        userDirectoryLabel.setFont(serif12);
        userDirectoryLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        userDirectoryText = new JTextField();
        userDirectoryText.setFont(serif12);
        userDirectoryText.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        Insets insets = new Insets(2, 5, 2, 5);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        extentsPanel.add(outOfBoundsLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        extentsPanel.add(outOfBoundsComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        extentsPanel.add(valueLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        extentsPanel.add(valueText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        extentsPanel.add(matrixLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        extentsPanel.add(matrixComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        extentsPanel.add(userDirectoryLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        extentsPanel.add(userDirectoryText, gbc);
        
        return extentsPanel;
    }
    
    /**
     * Builds a list of images. Returns combobox.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildImgComboBox(ModelImage image) {
        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();

            if (!name.equals(image.getImageName())) {
                ModelImage img = userInterface.getRegisteredImageByName(name);

                if ((image.getNDims() == img.getNDims()) && (image.isColorImage() == img.isColorImage()) &&
                        (userInterface.getFrameContainingImage(img) != null)) {
                    comboBox.addItem(name);
                }
            }
        }
        comboBox.addItemListener(this);

        return comboBox;
    }
    
    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        if (event.getSource() == outOfBoundsComboBox) {
            switch (outOfBoundsComboBox.getSelectedIndex()) {
                case 0: // image minimum
                    valueText.setText(String.valueOf(imageMin));
                    valueText.setEnabled(false);
                    break;
                case 1: // If float NaN, else 0
                    if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        valueText.setText(String.valueOf(Float.NaN)); 
                    }
                    else {
                        valueText.setText(String.valueOf(0));
                    }
                    valueText.setEnabled(false);
                    break;
                case 2: // User defined;
                    valueText.setEnabled(true);
                    break;
                case 3: // Image maximum
                    valueText.setText(String.valueOf(imageMax));
                    valueText.setEnabled(false);
                    break;
            } // switch (outOfBoundsComboBox.getSelectedIndex())
        } // if (event.getSource() == outOfBoundsComboBox)
        else if (event.getSource() == comboBoxImage) {
        	baseImage = userInterface.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        	
        	matrixComboBox.removeAllItems();
            
            if (baseImage != null) {
                matrixComboBox.addItem(baseImage.getImageDirectory());	
            }
            
            if ((matchImage.getImageDirectory() != null) && 
                	(!baseImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
                	matrixComboBox.addItem(matchImage.getImageDirectory());
            }
            
            if ((userInterface.getDefaultDirectory() != null) && 
            	(!userInterface.getDefaultDirectory().equals(baseImage.getImageDirectory())) &&
            	(!userInterface.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
            	matrixComboBox.addItem(userInterface.getDefaultDirectory());
            }
            matrixComboBox.addItem("User specified matrix directory");
            matrixComboBox.setSelectedIndex(0);
        }
    }


    /**
     * Sets the variables needed for calling the algorithm.
     *
     * @return  <code>true</code> if successful in setting variables.
     */
    private boolean setVariables() {

        // assign baseImage to image selected in comboBox
        String selectedName = (String) comboBoxImage.getSelectedItem();

        baseImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
            
        }
        
        fillValue = Float.valueOf(valueText.getText()).floatValue();
        outOfBoundsIndex = outOfBoundsComboBox.getSelectedIndex();
        if (outOfBoundsIndex == 2) {
            // user defined value
            boolean success = testType(dataType, fillValue);
            if (!success) {
                MipavUtil.displayError("User defined value is out of the data type range");
                valueText.requestFocus();
                valueText.selectAll();
                return false;
            }
        }
        
        matrixDirectory = (String)matrixComboBox.getSelectedItem();
        if (matrixDirectory != null) {
	        if (matrixDirectory.equals("User specified matrix directory")) {
	            matrixDirectory = userDirectoryText.getText();	
	        }
        }

        return true;
    }
    
    /**
     * Determine if the value is in the image type range and
     * within the float range since AlgorithmTransform does
     * not use double buffers.
     *
     * @param   type    image type
     * @param   value   value tested
     *
     * @return  true if value is within acceptable range
     */
    private boolean testType(int type, float value) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ((value < 0) || (value > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ((value < -128) || (value > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ((value < -32768) || (value > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ((value < Integer.MIN_VALUE) || (value > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ((value < 0) || (value > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ((value < Long.MIN_VALUE) || (value > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            // Float buffers are used in the AlgorithmTransform routines
            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

}
