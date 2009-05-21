package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix2f;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;


/**
 * Algorithm that can match: the orientation (based on the images saved Transform Matrix), the origin (based on the
 * image's saved origin, assumed as of Nov, 2004 to be ordered according to the GE's Left-Posterior-Superior ordering
 * and not according to the image's X-Y-Z directions), the resolution, and the dimensions of a pair of images so that
 * they can be easily compared.
 *
 * @author  Zohara A Cohen, Ph.D.
 */
public class AlgorithmMatchImages extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    String nameA, nameB;

    /** Orientation matrices and ordering indices. */
    private AlgorithmTransform algoTransform;

    /** DOCUMENT ME! */
    private int[] axisOrientA;

    /** DOCUMENT ME! */
    private int[] axisOrientB;

    /** boolean variable that determine what matchings are done. */
    private boolean changeUnits = false;

    /** DOCUMENT ME! */
    private int[] dimA, dimB;

    /** DOCUMENT ME! */
    private boolean doDimensions = true;

    /** DOCUMENT ME! */
    private boolean doOrients = true;

    /** DOCUMENT ME! */
    private boolean doOrigins = true;

    /** DOCUMENT ME! */
    //private double[] endImg_A, endImg_B;

    /** DOCUMENT ME! */
    private double[] endLPS_A, endLPS_B;

    /** DOCUMENT ME! */
    private float eps = 1.0e-6f;

    /** DOCUMENT ME! */
    private FileInfoBase fileInfo;

    /** DOCUMENT ME! */
    private int nDims;

    /** DOCUMENT ME! */
    private boolean newA, newB; // Indicates whether new images were created.

    /** DOCUMENT ME! */
    private double[] oDiffD;

    /** DOCUMENT ME! */
    private double[] origImg_A, origImg_B;

    /** Image origin and end location. */
    private double[] origLPS_A, origLPS_B;

    /** DOCUMENT ME! */
    private double padValue = 0.0;

    /** DOCUMENT ME! */
    private int[] reorderA2B;

    /** DOCUMENT ME! */
    private int[] reorderB2A;

    /** DOCUMENT ME! */
    private double[] resA, resB;

    /** DOCUMENT ME! */
    private boolean resByRef = false;

    /** DOCUMENT ME! */
    private ModelImage resultImgA, resultImgB; // Result images

    /** DOCUMENT ME! */
    private boolean[] reverse;

    /** DOCUMENT ME! */
    private boolean sameOrient;

    /** DOCUMENT ME! */
    private int[] sign2LPS_A, sign2LPS_B;

    /** Image info. */
    private ModelImage sourceImgA, sourceImgB; // Source images

    /** DOCUMENT ME! */
    private boolean stopped = false;

    /** DOCUMENT ME! */
    private int[] unitsOfMeasureA, unitsOfMeasureB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithm.
     *
     * @param  sourceImgA_    image A
     * @param  sourceImgB_    image B
     * @param  doOrigins_     DOCUMENT ME!
     * @param  doDimensions_  DOCUMENT ME!
     * @param  resByRef_      DOCUMENT ME!
     */
    public AlgorithmMatchImages(ModelImage sourceImgA_, ModelImage sourceImgB_, boolean doOrigins_,
                                boolean doDimensions_, boolean resByRef_) {
        super(null, sourceImgA_);
        this.sourceImgA = sourceImgA_;
        this.sourceImgB = sourceImgB_;
        this.doOrigins = doOrigins_;
        this.doDimensions = doDimensions_;
        this.resByRef = resByRef_;
        nDims = sourceImgA.getNDims();

        try {
            dimA = new int[nDims];
            resA = new double[nDims];
            dimB = new int[nDims];
            resB = new double[nDims];

            for (int i = 0; i < nDims; i++) {
                resA[i] = (double) sourceImgA.getFileInfo(0).getResolutions()[i];
                dimA[i] = sourceImgA.getExtents()[i];
                resB[i] = (double) sourceImgB.getFileInfo(0).getResolutions()[i];
                dimB[i] = sourceImgB.getExtents()[i];
            }

            oDiffD = new double[nDims];
            origLPS_A = new double[nDims];
            origLPS_B = new double[nDims];
            origImg_A = new double[nDims];
            origImg_B = new double[nDims];
            endLPS_A = new double[nDims];
            endLPS_B = new double[nDims];
            //endImg_A = new double[nDims];
            //endImg_B = new double[nDims];
            reverse = new boolean[nDims];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm reports: Out of memory");
            setCompleted(false);

            return;
        }

        if (sourceImgA.getNDims() != sourceImgB.getNDims()) {
            displayError("Images don't have the number of dimensions.");
            setCompleted(false);

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void checkUnits() {

        /**
         * Checks if units of measure from two images match.
         */
        int i;
        int lastSlice;

        try {
            unitsOfMeasureA = new int[nDims];
            unitsOfMeasureB = new int[nDims];

            for (i = 0; i < nDims; i++) {
                unitsOfMeasureA[i] = sourceImgA.getFileInfo(0).getUnitsOfMeasure()[i];
                unitsOfMeasureB[i] = sourceImgB.getFileInfo(0).getUnitsOfMeasure()[i];
            }
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmRegPatientPos.checkUnits() reports: Out of memory");
            setCompleted(false);
            stopped = true;

            return;
        }

        changeUnits = false;

        for (i = 0; i < nDims; i++) {

            if (unitsOfMeasureA[i] != unitsOfMeasureB[i]) {

                // Check if both are units of length
                if (((unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.METERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS) ||
                         (unitsOfMeasureA[i] == FileInfoBase.INCHES) || (unitsOfMeasureA[i] == FileInfoBase.MILES)) &&
                        ((unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.METERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) ||
                             (unitsOfMeasureB[i] == FileInfoBase.INCHES) || (unitsOfMeasureB[i] ==
                                                                                 FileInfoBase.MILES))) {
                    changeUnits = true;

                    // Change units of B into units of A
                    if (unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 10.0 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e8 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e10 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e13 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e8 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e13 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.ANGSTROMS;
                    } // if (unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 0.1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e12 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e12 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.NANOMETERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 1.0e-4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e9 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MICROMETERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 1.0e-7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 1.0e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e6 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MILLIMETERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 1.0e-8 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 1.0e-7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e-4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e-1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e2 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e5 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e5 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.CENTIMETERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.METERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 1.0e-10 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 1.0e-9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e-2 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e-2 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093e3 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.METERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.METERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 1.0e-13 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 1.0e-12 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 1.0e-9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 1.0e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 1.0e-5 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 2.54e-5 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 1.6093 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.KILOMETERS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.INCHES) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 3.937e-9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 3.937e-8 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 3.937e-5 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 3.937e-2 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 3.937e-1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 39.37 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 3.937e4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
                            resB[i] = 63360.0 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.INCHES;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.INCHES)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MILES) {

                        if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
                            resB[i] = 6.214e-14 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
                            resB[i] = 6.214e-13 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
                            resB[i] = 6.214e-10 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
                            resB[i] = 6.214e-7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
                            resB[i] = 6.214e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
                            resB[i] = 6.214e-4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
                            resB[i] = 6.214e-1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
                            resB[i] = 1.57828e-5 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MILES;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MILES)
                } // if both unitsOfMeasure are length units
                else if (((unitsOfMeasureA[i] == FileInfoBase.NANOSEC) ||
                              (unitsOfMeasureA[i] == FileInfoBase.MICROSEC) ||
                              (unitsOfMeasureA[i] == FileInfoBase.MILLISEC) ||
                              (unitsOfMeasureA[i] == FileInfoBase.SECONDS) ||
                              (unitsOfMeasureA[i] == FileInfoBase.MINUTES) ||
                              (unitsOfMeasureA[i] == FileInfoBase.HOURS)) &&
                             ((unitsOfMeasureB[i] == FileInfoBase.NANOSEC) ||
                                  (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) ||
                                  (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) ||
                                  (unitsOfMeasureB[i] == FileInfoBase.SECONDS) ||
                                  (unitsOfMeasureB[i] == FileInfoBase.MINUTES) ||
                                  (unitsOfMeasureB[i] == FileInfoBase.HOURS))) {
                    changeUnits = true;

                    if (unitsOfMeasureA[i] == FileInfoBase.NANOSEC) {

                        if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
                            resB[i] = 1.0e6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
                            resB[i] = 1.0e9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
                            resB[i] = 6.0e10 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
                            resB[i] = 3.6e12 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.NANOSEC;
                    } // if (unitsOfMeasureA[i] == FileInfoBase.NANOSEC)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MICROSEC) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
                            resB[i] = 1.0e6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
                            resB[i] = 6.0e7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
                            resB[i] = 3.6e9 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MICROSEC;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MICROSEC)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MILLISEC) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
                            resB[i] = 1.0e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
                            resB[i] = 1.0e3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
                            resB[i] = 6.0e4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
                            resB[i] = 3.6e6 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MILLISEC;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MILLISEC)
                    else if (unitsOfMeasureA[i] == FileInfoBase.SECONDS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
                            resB[i] = 1.0e-9 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
                            resB[i] = 1.0e-6 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
                            resB[i] = 1.0e-3 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
                            resB[i] = 6.0e1 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
                            resB[i] = 3.6e3 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.SECONDS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.SECONDS)
                    else if (unitsOfMeasureA[i] == FileInfoBase.MINUTES) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
                            resB[i] = 1.66666666e-11 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
                            resB[i] = 1.66666666e-8 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
                            resB[i] = 1.66666666e-5 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
                            resB[i] = 1.66666666e-2 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
                            resB[i] = 60.0 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.MINUTES;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.MINUTES)
                    else if (unitsOfMeasureA[i] == FileInfoBase.HOURS) {

                        if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
                            resB[i] = 2.77777777e-13 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
                            resB[i] = 2.77777777e-10 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
                            resB[i] = 2.77777777e-7 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
                            resB[i] = 2.77777777e-4 * resB[i];
                        } else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
                            resB[i] = 1.66666666e-2 * resB[i];
                        }

                        unitsOfMeasureB[i] = FileInfoBase.HOURS;
                    } // else if (unitsOfMeasureA[i] == FileInfoBase.HOURS)
                } // if both unitsOfMeasure are time units
            } // if (unitsOfMeasureA[i] != unitsOfMeasureB[i])
        } // for (int i=0; i<3; i++)
        // If units need to be changed, change them here.

        if (changeUnits) {
            float[] resolutionB = new float[nDims];

            for (i = 0; i < nDims; i++) {
                resolutionB[i] = (float) resB[i];
            }

            if (nDims == 3) {
                lastSlice = dimB[2];
            } else {
                lastSlice = 1;
            }

            for (i = 0; i < lastSlice; i++) {
                fileInfo = sourceImgB.getFileInfo()[i];
                fileInfo.setResolutions(resolutionB);
                fileInfo.setUnitsOfMeasure(unitsOfMeasureB);
                sourceImgB.setFileInfo(fileInfo, i);
            } // for (int i = 0; i < lastSlice; i++)
        } // if (changeUnits)
    }

    /**
     * Get rid of space hogs.
     */
    public void disposeLocal() {

        sourceImgA = null;
        sourceImgB = null;
    }

    /**
     * Exits from run method.
     */
    public void doStop() {
        Preferences.debug("AlgorithmMatchImages stopped by another algorithm that it called.");
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }


    /**
     * Returns first of new images.
     *
     * @return  resultImgA
     */
    public ModelImage getResultA() {
        resultImgA.calcMinMax();

        return this.resultImgA;
    }

    /**
     * Returns second of new images.
     *
     * @return  resultImgB
     */
    public ModelImage getResultB() {
        resultImgB.calcMinMax();

        return this.resultImgB;
    }

    /**
     * Tells if new imageA was created.
     *
     * @return  newA
     */
    public boolean isNewA() {
        return this.newA;
    }

    /**
     * Tells if new imageB was created.
     *
     * @return  newB
     */
    public boolean isNewB() {
        return this.newB;
    }

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        /**
         *  Runs the program
         */

        // Create result images.
        String tag = new String("_matched2_");
        nameA = JDialogBase.makeImageName(sourceImgA.getImageName(), tag.concat(sourceImgB.getImageName()));
        nameB = JDialogBase.makeImageName(sourceImgB.getImageName(), tag.concat(sourceImgA.getImageName()));
        newA = false;
        newB = false;

        

        fireProgressStateChanged("Retrieving data ...");

        // Get data
        getData();

        fireProgressStateChanged(10, null, "Matching colors ...");

        // Match image colors
        matchColors();

        if (doOrients) {

            // The only way this can be false is if it's explicitly set to be.  JDialogMatchImages
            // do not given an option not to match orientations.
            // Match orientations.
            if (!sameOrient) {
                Preferences.debug("\n Matching orientations...\n");
                fireProgressStateChanged(25, null, "Matching orientation ...");
                matchOrients();
            } else {
                Preferences.debug("Orientations already match.\n");
            }

            // Stop, if necessary
            if (stopped) {
                doStop();

                return;
            }
        }

        fireProgressStateChanged(40, null, " Matching units ...");

        // Match resolutions
        checkUnits();

        boolean doResols = false;

        for (int i = 0; i < nDims; i++) {

            if (resB[i] != resA[i]) {

                if ((resB[i] >= (resA[i] - eps)) && (resB[i] <= (resA[i] + eps))) {
                    resB[i] = resA[i];
                } else {
                    doResols = true;
                }
            }
        }

        if (doResols) {
            Preferences.debug("\nMatching resolutions...\n");
            fireProgressStateChanged(60, null, " Matching units ...");
            matchResolutions();
        } else {
            Preferences.debug("Resolutions already match.\n");
        }

        // Stop, if necessary
        if (stopped) {
            doStop();

            return;
        }

        // Match origins.
        if (doOrigins) {
            fireProgressStateChanged(80, null, " Matching origins ...");
            Preferences.debug("\nMatching origins...\n");
            matchOrigins();
        }

        // Stop, if necessary
        if (stopped) {
            doStop();

            return;
        }

        // Match dimensions.
        if (doDimensions) {
            doDimensions = false;

            for (int i = 0; i < nDims; i++) {

                if (dimA[i] != dimB[i]) {
                    doDimensions = true;
                }
            }
        }

        if (doDimensions) {
            fireProgressStateChanged(90, null, " Matching dimensions ...");
            Preferences.debug("\nMatching dimensions...\n");
            matchDimensions();
        }

        fireProgressStateChanged(100);

        setCompleted(true);
    }

    /**
     * Set value for doOrients. Not set in constructor because for most applications it will be true.
     *
     * @param  doOr  DOCUMENT ME!
     */
    public void setOrients(boolean doOr) {
        doOrients = doOr;
    }

    /**
     * Returns orgthogonal vector.
     *
     * @param  pad  vector
     */
    /*double[] vectorCrossProduct(double[] a, double[] b) {
     * double[] c = new double[3]; //cross product c = axb = <a2b3-a3b2,a3b1-a1b3,a1b2-a2b1> c[0] = a[1] * b[2] - a[2] *
     * b[1]; c[1] = a[2] * b[0] - a[0] * b[2]; c[2] = a[0] * b[1] - a[1] * b[0]; return c;}*/

    /**
     * Set value for image padding during transformation. Not set in constructor because for most applications it will
     * be 0.
     *
     * @param  pad  DOCUMENT ME!
     */
    public void setPadValue(int pad) {
        padValue = (double) pad;
    } 

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     *
     * @param   mat  4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x = Left, +y = Posterior, +z
     *               = Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation
     *               of (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the
     *               input matrix Errors: The codes returned will be zero.
     *
     * @return  codes
     */
    public static int[] getAxisOrientation(TransMatrix mat) {
        int[] axisOrientation = new int[3];
        //double[][] array;
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        Matrix3f Q;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        Matrix3f P;
        double detP;
        Matrix3f M = new Matrix3f();

        //array = mat.getMatrix(0, 2, 0, 2).getArray();

        xi = mat.Get(0, 0);
        xj = mat.Get(0, 1);
        xk = mat.Get(0, 2);
        yi = mat.Get(1, 0);
        yj = mat.Get(1, 1);
        yk = mat.Get(1, 2);
        zi = mat.Get(2, 0);
        zj = mat.Get(2, 1);
        zk = mat.Get(2, 2);

        int izero = 0;
        int jzero = 0;
        int kzero = 0;
        int xzero = 0;
        int yzero = 0;
        int zzero = 0;

        if (xi == 0.0) {
            izero++;
            xzero++;
        }

        if (yi == 0.0) {
            izero++;
            yzero++;
        }

        if (zi == 0.0) {
            izero++;
            zzero++;
        }

        if (xj == 0.0) {
            jzero++;
            xzero++;
        }

        if (yj == 0.0) {
            jzero++;
            yzero++;
        }

        if (zj == 0.0) {
            jzero++;
            zzero++;
        }

        if (xk == 0.0) {
            kzero++;
            xzero++;
        }

        if (yk == 0.0) {
            kzero++;
            yzero++;
        }

        if (zk == 0.0) {
            kzero++;
            zzero++;
        }

        if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2)) {

            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            }

            return axisOrientation;
        } // if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2))

        // Normalize column vectors to get unit vectors along each ijk-axis

        // Normalize i axis
        val = Math.sqrt((xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;
        zj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj) + (zi * zj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to i
        val = (xi * xk) + (yi * yk) + (zi * zk); // dot product between i and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to i");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to j
        val = (xj * xk) + (yj * yk) + (zj * zk); // dot product between j and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to j");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        if (((Math.abs(xi) > 0.9) || (Math.abs(yi) > 0.9) || (Math.abs(zi) > 0.9)) &&
                ((Math.abs(xj) > 0.9) || (Math.abs(yj) > 0.9) || (Math.abs(zj) > 0.9)) &&
                ((Math.abs(xk) > 0.9) || (Math.abs(yk) > 0.9) || (Math.abs(zk) > 0.9))) {

            if (Math.abs(xi) < 0.9) {
                xi = 0;
            }

            if (Math.abs(yi) < 0.9) {
                yi = 0;
            }

            if (Math.abs(zi) < 0.9) {
                zi = 0;
            }

            if (Math.abs(xj) < 0.9) {
                xj = 0;
            }

            if (Math.abs(yj) < 0.9) {
                yj = 0;
            }

            if (Math.abs(zj) < 0.9) {
                zj = 0;
            }

            if (Math.abs(xk) < 0.9) {
                xk = 0;
            }

            if (Math.abs(yk) < 0.9) {
                yk = 0;
            }

            if (Math.abs(zk) < 0.9) {
                zk = 0;
            }

            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            }

            return axisOrientation;
        }

        mat.set(0, 0, xi);
        mat.set(0, 1, xj);
        mat.set(0, 2, xk);
        mat.set(1, 0, yi);
        mat.set(1, 1, yj);
        mat.set(1, 2, yk);
        mat.set(2, 0, zi);
        mat.set(2, 1, zj);
        mat.set(2, 2, zk);

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new Matrix3f((float)xi, (float)xj, (float)xk, 
        		(float)yi, (float)yj, (float)yk, 
        		(float)zi, (float)zj, (float)zk);
        detQ = Q.Determinant();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        // Despite the formidable looking 6 nested loops, there are
        // only 3*3*3*2*2*2 = 216 passes, which will run very quickly
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        rbest = 1;
        ibest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { // i = column number to use for row #1

            for (j = 1; j <= 3; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { // k = column number to use for row #3

                    if ((i == k) || (j == k)) {
                        continue;
                    }

                    mat.set(0, 0, 0.0);
                    mat.set(0, 1, 0.0);
                    mat.set(0, 2, 0.0);
                    mat.set(1, 0, 0.0);
                    mat.set(1, 1, 0.0);
                    mat.set(1, 2, 0.0);
                    mat.set(2, 0, 0.0);
                    mat.set(2, 1, 0.0);
                    mat.set(2, 2, 0.0);
                    P = new Matrix3f(); // zero matrix

                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.Set(0, i - 1, p);
                                P.Set(1, j - 1, q);
                                P.Set(2, k - 1, r);
                                detP = P.Determinant();

                                // sign of permutation doesn't match sign of Q
                                if ((detP * detQ) <= 0.0) {
                                    continue;
                                }

                                M.Mult(P, Q);

                                // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                                // we want largest trace(M) == smallest angle == M nearest to I
                                val = M.Get(0, 0) + M.Get(1, 1) + M.Get(2, 2); // trace

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
        // to the (i,j,k) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes.  If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis.  The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * @return the 2 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j)
     * axes.
     *
     * @param   mat  3x3 matrix that transforms (i,j) indexes to x,y coordinates where +x = Left, +y = Posterior Only
     *               the upper-left 2x2 corner of the matrix is used This routine finds the permutation of (x,y) which
     *               has the smallest angle to the (i,j) axes directions, which are columns of the input matrix Errors:
     *               The codes returned will be zero.
     *
     */
    private int[] getAxisOrientation2D(TransMatrix mat) {
        int[] axisOrientation = new int[2];
        //double[][] array;
        double xi, xj, yi, yj, val;
        Matrix2f Q;
        double detQ;
        double vbest;
        int ibest, jbest, pbest, qbest;
        int i, j, p, q;
        Matrix2f P;
        double detP;
        Matrix2f M = new Matrix2f();

        //array = mat.getMatrix(0, 1, 0, 1).getArray();

        xi = mat.Get(0, 0);
        xj = mat.Get(0, 1);
        yi = mat.Get(1, 0);
        yj = mat.Get(1, 1);

        int izero = 0;
        int jzero = 0;
        int xzero = 0;
        int yzero = 0;

        if (xi == 0.0) {
            izero++;
            xzero++;
        }

        if (yi == 0.0) {
            izero++;
            yzero++;
        }

        if (xj == 0.0) {
            jzero++;
            xzero++;
        }

        if (yj == 0.0) {
            jzero++;
            yzero++;
        }

        if ((izero == 2) && (jzero == 2) && (xzero == 2) && (yzero == 2)) {

            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            }

            return axisOrientation;
        } // if ((izero == 2) && (jzero == 2) && (xzero == 2) && (yzero == 2))

        // Normalize column vectors to get unit vectors along each ij-axis

        // Normalize i axis
        val = Math.sqrt((xi * xi) + (yi * yi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;

        // Normalize j axis
        val = Math.sqrt((xj * xj) + (yj * yj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            val = Math.sqrt((xj * xj) + (yj * yj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
        }


        if (((Math.abs(xi) > 0.9) || (Math.abs(yi) > 0.9)) && ((Math.abs(xj) > 0.9) || (Math.abs(yj) > 0.9))) {

            if (Math.abs(xi) < 0.9) {
                xi = 0;
            }

            if (Math.abs(yi) < 0.9) {
                yi = 0;
            }

            if (Math.abs(xj) < 0.9) {
                xj = 0;
            }

            if (Math.abs(yj) < 0.9) {
                yj = 0;
            }


            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            }

            return axisOrientation;
        }

        mat.set(0, 0, xi);
        mat.set(0, 1, xj);
        mat.set(1, 0, yi);
        mat.set(1, 1, yj);

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new Matrix2f((float)xi, (float)xj, (float)yi, (float)yj );
        detQ = Q.Determinant();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        ibest = 1;
        jbest = 2;

        for (i = 1; i <= 2; i++) { // i = column number to use for row #1

            for (j = 1; j <= 2; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                mat.set(0, 0, 0.0);
                mat.set(0, 1, 0.0);
                mat.set(1, 0, 0.0);
                mat.set(1, 1, 0.0);
                P = new Matrix2f();

                for (p = -1; p <= 1; p += 2) { // p,q are -1 or +1 and go into rows #1,2

                    for (q = -1; q <= 1; q += 2) {
                        P.Set(0, i - 1, p);
                        P.Set(1, j - 1, q);
                        detP = P.Determinant();

                        // sign of permutation doesn't match sign of Q
                        if ((detP * detQ) <= 0.0) {
                            continue;
                        }

                        M.Mult(P, Q);

                        // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                        // we want largest trace(M) == smallest angle == M nearest to I
                        val = M.Get(0, 0) + M.Get(1, 1); // trace

                        if (val > vbest) {
                            vbest = val;
                            ibest = i;
                            jbest = j;
                            pbest = p;
                            qbest = q;
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y) coordinates
        // to the (i,j) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes.  If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis.  The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * DOCUMENT ME!
     */
    private void getData() {

        /**
         *   Get origin, transformation data, and field of view.  This has to be done regardless of which other options
         * the user chose.
         */
        double[] fovA, fovB;
        int i, j;
        double temp;

        try {
            reorderB2A = new int[nDims];
            reorderA2B = new int[nDims];
            sign2LPS_A = new int[nDims];
            sign2LPS_B = new int[nDims];
            fovA = new double[nDims];
            fovB = new double[nDims];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmMatchImages.getData() reports: Out of memory");
            setCompleted(false);
            stopped = true;

            return;
        }

        Preferences.debug("\n PRELIMINARY DATA: \n");

        if (sourceImgA.getNDims() > 2) {
            axisOrientA = getAxisOrientation(sourceImgA.getMatrix());
            Preferences.debug("axisOrientA = " + axisOrientA[0] + ", " + axisOrientA[1] + ", " + axisOrientA[2] + "\n");
        } else {
            axisOrientA = getAxisOrientation2D(sourceImgA.getMatrix());
            Preferences.debug("axisOrientA = " + axisOrientA[0] + ", " + axisOrientA[1] + "\n");
        }

        if (sourceImgB.getNDims() > 2) {
            axisOrientB = getAxisOrientation(sourceImgB.getMatrix());
            Preferences.debug("axisOrientB = " + axisOrientB[0] + ", " + axisOrientB[1] + ", " + axisOrientB[2] + "\n");
        } else {
            axisOrientB = getAxisOrientation2D(sourceImgB.getMatrix());
            Preferences.debug("axisOrientB = " + axisOrientB[0] + ", " + axisOrientB[1] + "\n");
        }

        sameOrient = true;

        for (i = 0; i < nDims; i++) {

            if (axisOrientA[i] != axisOrientB[i]) {
                sameOrient = false;
            }
        }

        for (i = 0; i < nDims; i++) {

            if ((axisOrientB[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrientB[i] == FileInfoBase.ORI_L2R_TYPE)) {

                for (j = 0; j < nDims; j++) {

                    if ((axisOrientA[j] == FileInfoBase.ORI_R2L_TYPE) ||
                            (axisOrientA[j] == FileInfoBase.ORI_L2R_TYPE)) {
                        reorderA2B[j] = i;
                        reorderB2A[i] = j;

                        if (axisOrientB[i] != axisOrientA[j]) {
                            reverse[i] = true;
                        } else {
                            reverse[i] = false;
                        }
                    }
                }
            } else if ((axisOrientB[i] == FileInfoBase.ORI_A2P_TYPE) || (axisOrientB[i] == FileInfoBase.ORI_P2A_TYPE)) {

                for (j = 0; j < nDims; j++) {

                    if ((axisOrientA[j] == FileInfoBase.ORI_A2P_TYPE) ||
                            (axisOrientA[j] == FileInfoBase.ORI_P2A_TYPE)) {
                        reorderA2B[j] = i;
                        reorderB2A[i] = j;

                        if (axisOrientB[i] != axisOrientA[j]) {
                            reverse[i] = true;
                        } else {
                            reverse[i] = false;
                        }
                    }
                }
            } else if ((axisOrientB[i] == FileInfoBase.ORI_I2S_TYPE) || (axisOrientB[i] == FileInfoBase.ORI_S2I_TYPE)) {

                for (j = 0; j < nDims; j++) {

                    if ((axisOrientA[j] == FileInfoBase.ORI_I2S_TYPE) ||
                            (axisOrientA[j] == FileInfoBase.ORI_S2I_TYPE)) {
                        reorderA2B[j] = i;
                        reorderB2A[i] = j;

                        if (axisOrientB[i] != axisOrientA[j]) {
                            reverse[i] = true;
                        } else {
                            reverse[i] = false;
                        }
                    }
                }
            }

            if ((axisOrientA[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrientA[i] == FileInfoBase.ORI_A2P_TYPE) ||
                    (axisOrientA[i] == FileInfoBase.ORI_I2S_TYPE)) {
                sign2LPS_A[i] = 1;
            } else {
                sign2LPS_A[i] = -1;
            }

            if ((axisOrientB[i] == FileInfoBase.ORI_R2L_TYPE) || (axisOrientB[i] == FileInfoBase.ORI_A2P_TYPE) ||
                    (axisOrientB[i] == FileInfoBase.ORI_I2S_TYPE)) {
                sign2LPS_B[i] = 1;
            } else {
                sign2LPS_B[i] = -1;
            }
        } // for (i = 0; i < nDims; i++)

        if (nDims == 3) {
            Preferences.debug("Indices to reorder ImgB to ImgA: " + reorderB2A[0] + ", " + reorderB2A[1] + ", " +
                              reorderB2A[2] + "\n");
            Preferences.debug("Original ImageA signs for LPS: " + sign2LPS_A[0] + ", " + sign2LPS_A[1] + ", " +
                              sign2LPS_A[2] + "\n");
            Preferences.debug("Original ImageB signs for LPS: " + sign2LPS_B[0] + ", " + sign2LPS_B[1] + ", " +
                              sign2LPS_B[2] + "\n");
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug("Indices to reorder ImgB to ImgA: " + reorderB2A[0] + ", " + reorderB2A[1] + "\n");
            Preferences.debug("Original ImageA signs for LPS: " + sign2LPS_A[0] + ", " + sign2LPS_A[1] + "\n");
            Preferences.debug("Original ImageB signs for LPS: " + sign2LPS_B[0] + ", " + sign2LPS_B[1] + "\n");
        } // else nDims == 2

        // Get image origin before transformation.
        for (i = 0; i < nDims; i++) {
            origLPS_A[i] = (double) sourceImgA.getFileInfo(0).getOrigin(i);
            origLPS_B[i] = (double) sourceImgB.getFileInfo(0).getOrigin(i);
        }

        if (nDims == 3) {
            Preferences.debug("Original ImageA startLocation: " + (float) origLPS_A[0] + ", " + (float) origLPS_A[1] +
                              ", " + (float) origLPS_A[2] + "\n");
            Preferences.debug("Original ImageB startLocation: " + (float) origLPS_B[0] + ", " + (float) origLPS_B[1] +
                              ", " + (float) origLPS_B[2] + "\n");
        } else { // nDims == 2
            Preferences.debug("Original ImageA startLocation: " + (float) origLPS_A[0] + ", " + (float) origLPS_A[1] +
                              "\n");
            Preferences.debug("Original ImageB startLocation: " + (float) origLPS_B[0] + ", " + (float) origLPS_B[1] +
                              "\n");
        } // else nDims == 2

        for (i = 0; i < nDims; i++) {
            fovA[i] = resA[i] * (dimA[i] - 1); // field of view in this dimension
            fovB[i] = resB[i] * (dimB[i] - 1);
        }

        for (i = 0; i < nDims; i++) {

            if (origLPS_A[i] < 0) {
                endLPS_A[i] = origLPS_A[i] + fovA[i];
            } else {
                endLPS_A[i] = origLPS_A[i] - fovA[i];
            }

            if (origLPS_B[i] < 0) {
                endLPS_B[i] = origLPS_B[i] + fovB[i];
            } else {
                endLPS_B[i] = origLPS_B[i] - fovB[i];
            }

            if (reverse[i]) {
                temp = origLPS_B[i];
                origLPS_B[i] = endLPS_B[i];
                endLPS_B[i] = temp;
            }
        }

        for (i = 0; i < nDims; i++) {

            switch (axisOrientA[i]) {

                case FileInfoBase.ORI_R2L_TYPE:
                case FileInfoBase.ORI_L2R_TYPE:
                    origImg_A[0] = origLPS_A[i];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                case FileInfoBase.ORI_P2A_TYPE:
                    origImg_A[1] = origLPS_A[i];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                case FileInfoBase.ORI_S2I_TYPE:
                    origImg_A[2] = origLPS_A[i];
                    break;
            } // switch(axisOrientA[i])

            switch (axisOrientB[i]) {

                case FileInfoBase.ORI_R2L_TYPE:
                case FileInfoBase.ORI_L2R_TYPE:
                    origImg_B[0] = origLPS_B[i];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                case FileInfoBase.ORI_P2A_TYPE:
                    origImg_B[1] = origLPS_B[i];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                case FileInfoBase.ORI_S2I_TYPE:
                    origImg_B[2] = origLPS_B[i];
                    break;
            } // switch(axisOrientB[i])
        } // for (i = 0; i < nDims; i++)

        if (nDims == 3) {
            Preferences.debug("ImageA origins in image order: " + (float) origImg_A[0] + ", " + (float) origImg_A[1] +
                              ", " + (float) origImg_A[2] + "\n");
            Preferences.debug("ImageB origins in Image order: " + (float) origImg_B[0] + ", " + (float) origImg_B[1] +
                              ", " + (float) origImg_B[2] + "\n");
        } // if (nDims == 3
        else { // nDims == 2
            Preferences.debug("ImageA origins in image order: " + (float) origImg_A[0] + ", " + (float) origImg_A[1] +
                              "\n");
            Preferences.debug("ImageB origins in ImageA order: " + (float) origImg_B[0] + ", " + (float) origImg_B[1] +
                              "\n");

        } // else nDims == 2


    }

    /**
     * DOCUMENT ME!
     */
    private void matchColors() {

        /**
         *   Match color types.  If imageA is grayscale, make imageB grayscale.  If imageA is RGB...
         */

        // Check to see if the color types match up
        if (sourceImgA.isColorImage() && !sourceImgB.isColorImage()) {

            // If resultImgA is color and resultImgB isn't, change resultImgB to a color image.
            // Run algorithm AlgorithmRGBConcat
            if (!newB) {
                resultImgB = (ModelImage) sourceImgB.clone();
                newB = true;
            }

            resultImgB.setImageName("imageB_rgb");

            AlgorithmRGBConcat rgbAlgo = new AlgorithmRGBConcat(resultImgB, resultImgB, resultImgB, ModelStorageBase.ARGB,
                                                                true, true);
            rgbAlgo.setRunningInSeparateThread(runningInSeparateThread);
            rgbAlgo.run();
            resultImgB = rgbAlgo.getImageR();

            // Clean up
            rgbAlgo.finalize();
            rgbAlgo = null;
        } else if (!sourceImgA.isColorImage() && sourceImgB.isColorImage()) {

            // If resultImgA is not color and resultImgB is, change resultImgB to a grayscale image.
            float redValue = 1.0f / 3.0f;
            float greenValue = 1.0f / 3.0f;
            float blueValue = 1.0f / 3.0f;
            float threshold = 1.0f;

            // Run algorithm AlgorithmRGBtoGray
            if (!newB) {
                resultImgB = (ModelImage) sourceImgB.clone();
                newB = true;
            }

            resultImgB.setImageName("imageB_gray");

            AlgorithmRGBtoGray rgbAlgo = new AlgorithmRGBtoGray(resultImgB, redValue, greenValue, blueValue, true,
                                                                threshold, true);
            rgbAlgo.setRunningInSeparateThread(runningInSeparateThread);
            rgbAlgo.run();
            resultImgB = rgbAlgo.getSrcImage();

            rgbAlgo.finalize();
            rgbAlgo = null;

        }
    }

    /**
     * DOCUMENT ME!
     */
    private void matchDimensions() {

        /**
         *   Append pixels to end (right, bottom, back) of whichever image has smaller dimension.  Only gets called when
         * matchDimensions is true but matchOrigins is not.  If matchOrigins  is also true, this will get done within
         * that algorithm.  Doesn't affect resolution.
         */

        int[] appendA = new int[nDims];
        int[] appendB = new int[nDims];
        AlgorithmAddMargins algoMarginsA, algoMarginsB;
        boolean newDimA = false;
        boolean newDimB = false;

        // Find difference
        for (int i = 0; i < nDims; i++) {

            if (dimA[i] > dimB[i]) {
                appendB[i] = dimA[i] - dimB[i];
                dimB[i] = dimA[i];
                newDimB = true;
            } else if (dimB[i] > dimA[i]) {
                appendA[i] = dimB[i] - dimA[i];
                dimA[i] = dimB[i];
                newDimA = true;
            }
        }

        if (newDimA) {

            if (!newA) {
                resultImgA = (ModelImage) sourceImgA.clone();
                newA = true;
            }

            resultImgA.setImageName("tempA");
            Vector mats = resultImgA.getMatrixHolder().getMatrices();

            if (nDims == 3) {
                algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, 0, appendA[0], 0, appendA[1], 0,
                                                       appendA[2]);
            } // if (nDims == 3)
            else { // nDims == 2
                algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, 0, appendA[0], 0, appendA[1]);
            } // else nDims == 2

            algoMarginsA.setRunningInSeparateThread(runningInSeparateThread);
            algoMarginsA.run();

            if (algoMarginsA.isCompleted() == false) {
                Preferences.debug("algoMarginsA or algoMarginsB in matchDimensions failed.");
                algoMarginsA.finalize();
                algoMarginsA = null;
                setCompleted(false);
                stopped = true;

                return;
            }

            resultImgA.disposeLocal();
            resultImgA = algoMarginsA.getSrcImage();
            resultImgA.setImageName(nameA);
            algoMarginsA.finalize();
            algoMarginsA = null;

            if (nDims == 3) {
                Preferences.debug("New ImageA dimensions (matchDimensions): " + dimA[0] + ", " + dimA[1] + ", " +
                                  dimA[2] + "\n");
            } // if (nDims == 3)
            else { // nDims == 2
                Preferences.debug("New ImageA dimensions (matchDimensions): " + dimA[0] + ", " + dimA[1] + "\n");
            } // else nDims == 2

            // update file info
            resultImgA.getMatrixHolder().replaceMatrices(mats);
            updateFileInfo(resultImgA, dimA, resA, origLPS_A);
        }

        if (newDimB) {

            if (!newB) {
                resultImgB = (ModelImage) sourceImgB.clone();
                newB = true;
            }

            // BEN: change

            resultImgB.setImageName("tempB");
            Vector mats = resultImgB.getMatrixHolder().getMatrices();

            if (nDims == 3) {
                algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, 0, appendB[0], 0, appendB[1], 0,
                                                       appendB[2]);
            } // if (nDims == 3)
            else { // nDims == 2
                algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, 0, appendB[0], 0, appendB[1]);
            } // else nDims == 2

            algoMarginsB.setRunningInSeparateThread(runningInSeparateThread);
            algoMarginsB.run();

            if (algoMarginsB.isCompleted() == false) {
                Preferences.debug("algoMarginsA or algoMarginsB in matchDimensions failed.");
                algoMarginsB.finalize();
                algoMarginsB = null;
                setCompleted(false);
                stopped = true;

                return;
            }

            if (nDims == 3) {
                Preferences.debug("New ImageB dimensions (matchDimensions): " + dimB[0] + ", " + dimB[1] + ", " +
                                  dimB[2] + "\n");
            } // if (nDims == 3)
            else { // nDims == 2
                Preferences.debug("New ImageB dimensions (matchDimensions): " + dimB[0] + ", " + dimB[1] + "\n");
            } // else nDims == 2

            resultImgB.disposeLocal();
            resultImgB = algoMarginsB.getSrcImage();
            resultImgB.setImageName(nameB);
            algoMarginsB.finalize();
            algoMarginsB = null;

            // update file info
            resultImgB.getMatrixHolder().replaceMatrices(mats);
            updateFileInfo(resultImgB, dimB, resB, origLPS_B);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void matchOrients() {

        /**
         *   Reorient ImageB so that directions will coincide with ImageA.
         */
        int tID;
        int[] oldDims = new int[nDims];

        if (!newB) {
            resultImgB = (ModelImage) sourceImgB.clone();
            newB = true;
        }

        oldDims = sourceImgB.getExtents();

        int sliceSize = oldDims[0] * oldDims[1];
        int x, y, z;
        int[] newXY;
        int[] newXYZ;
        int[] newDimB;
        int newSlice;
        float[] buffer;
        float[] buffer2;
        int length;
        int i;
        int c;
        int cDim = 1;
        int resultType;
        FileInfoBase[] fileInfo;
        int[] units;
        float[] res;
        double[] tempRes;

        length = oldDims[0];

        for (i = 1; i < nDims; i++) {
            length *= oldDims[i];
        }

        if (resultImgB.isColorImage()) {
            length *= 4;
            cDim = 4;
        }

        buffer = new float[length];
        buffer2 = new float[length];

        try {
            resultImgB.exportData(0, length, buffer);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on resultImgB.exportData");
            setCompleted(false);
            stopped = true;

            return;
        }

        resultType = resultImgB.getType();

        if (nDims == 3) {
            newXYZ = new int[3];
            newDimB = new int[3];
            newDimB[reorderB2A[0]] = dimB[0];
            newDimB[reorderB2A[1]] = dimB[1];
            newDimB[reorderB2A[2]] = dimB[2];
            newSlice = newDimB[0] * newDimB[1];
            Preferences.debug("oldDims = " + oldDims[0] + ", " + oldDims[1] + ", " + oldDims[2] + "\n");
            Preferences.debug("newDimB = " + newDimB[0] + ", " + newDimB[1] + ", " + newDimB[2] + "\n");

            for (z = 0; z < oldDims[2]; z++) {

                if (reverse[2]) {
                    newXYZ[reorderB2A[2]] = oldDims[2] - 1 - z;
                } else {
                    newXYZ[reorderB2A[2]] = z;
                }

                for (y = 0; y < oldDims[1]; y++) {

                    if (reverse[1]) {
                        newXYZ[reorderB2A[1]] = oldDims[1] - 1 - y;
                    } else {
                        newXYZ[reorderB2A[1]] = y;
                    }

                    for (x = 0; x < oldDims[0]; x++) {

                        if (reverse[0]) {
                            newXYZ[reorderB2A[0]] = (oldDims[0] - 1 - x);
                        } else {
                            newXYZ[reorderB2A[0]] = x;
                        }

                        for (c = 0; c < cDim; c++) {
                            buffer2[c + (cDim * (newXYZ[0] + (newXYZ[1] * newDimB[0]) + (newXYZ[2] * newSlice)))] = buffer[c +
                                                                                                                           (cDim *
                                                                                                                                (x +
                                                                                                                                     (y *
                                                                                                                                          oldDims[0]) +
                                                                                                                                     (z *
                                                                                                                                          sliceSize)))];
                        }
                    }
                }
            }
        } // if (nDims == 3)
        else { // nDims == 2
            newXY = new int[2];
            newDimB = new int[2];
            newDimB[reorderB2A[0]] = dimB[0];
            newDimB[reorderB2A[1]] = dimB[1];

            for (y = 0; y < oldDims[1]; y++) {

                if (reverse[1]) {
                    newXY[reorderB2A[1]] = oldDims[1] - 1 - y;
                } else {
                    newXY[reorderB2A[1]] = y;
                }

                for (x = 0; x < oldDims[0]; x++) {

                    if (reverse[0]) {
                        newXY[reorderB2A[0]] = oldDims[0] - 1 - x;
                    } else {
                        newXY[reorderB2A[0]] = x;
                    }

                    for (c = 0; c < cDim; c++) {
                        buffer2[c + (cDim * (newXY[0] + (newXY[1] * newDimB[0])))] = buffer[c +
                                                                                            (cDim *
                                                                                                 (x + (y * oldDims[0])))];
                    }
                }
            }
        } // else nDims == 2

        resultImgB.disposeLocal();
        resultImgB = new ModelImage(resultType, newDimB, nameB);

        try {
            resultImgB.importData(0, buffer2, true);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on resultImgB.importData");
            setCompleted(false);
            stopped = true;

            return;
        }

        resultImgB.getMatrixHolder().replaceMatrices(sourceImgA.getMatrixHolder().getMatrices());

        // Get updated resolutions and dimensions.
        res = new float[nDims];
        tempRes = new double[nDims];
        units = new int[nDims];

        for (i = 0; i < nDims; i++) {
            tempRes[reorderB2A[i]] = resB[i];
        }

        for (i = 0; i < nDims; i++) {
            resB[i] = tempRes[i];
            res[i] = (float) resB[i];
            dimB[i] = resultImgB.getExtents()[i];
            units[reorderB2A[i]] = sourceImgB.getFileInfo()[0].getUnitsOfMeasure()[i];
            axisOrientB[i] = axisOrientA[i];
        }

        if (nDims == 2) {
            fileInfo = new FileInfoBase[1];
            fileInfo[0] = new FileInfoImageXML(null, null, FileUtility.XML);
            fileInfo[0].setExtents(dimB);
            fileInfo[0].setResolutions(res);
            fileInfo[0].setUnitsOfMeasure(units);
            fileInfo[0].setDataType(resultType);
        } else {
            fileInfo = new FileInfoBase[dimB[2]];

            for (i = 0; i < dimB[2]; i++) {
                fileInfo[i] = new FileInfoImageXML(null, null, FileUtility.XML);
                fileInfo[i].setExtents(dimB);
                fileInfo[i].setResolutions(res);
                fileInfo[i].setUnitsOfMeasure(units);
                fileInfo[i].setDataType(resultType);
                fileInfo[i].setAxisOrientation(axisOrientB);
            }
        }

        resultImgB.setFileInfo(fileInfo);

        if (nDims == 3) {
            Preferences.debug("ImageB resolution in matchOrients after transform: " + (float) resB[0] + ", " +
                              (float) resB[1] + ", " + (float) resB[2] + "\n");
            Preferences.debug("ImageB dimensions in matchOrient after transform: " + dimB[0] + ", " + dimB[1] + ", " +
                              dimB[2] + "\n");
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug("ImageB resolution in matchOrients after transform: " + (float) resB[0] + ", " +
                              (float) resB[1] + "\n");
            Preferences.debug("ImageB dimensions in matchOrient after transform: " + dimB[0] + ", " + dimB[1] + "\n");
        } // else nDims == 2


        for (i = 0; i < nDims; i++) {

            switch (axisOrientB[i]) {

                case FileInfoBase.ORI_R2L_TYPE:
                case FileInfoBase.ORI_L2R_TYPE:
                    origLPS_B[i] = origImg_B[0];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                case FileInfoBase.ORI_P2A_TYPE:
                    origLPS_B[i] = origImg_B[1];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                case FileInfoBase.ORI_S2I_TYPE:
                    origLPS_B[i] = origImg_B[2];
                    break;
            } // switch(axisOrientB[i])
        } // for (i = 0; i < nDims; i++)

        if (nDims == 3) {
            Preferences.debug("Final ImageB origins in LPS order: " + (float) origLPS_B[0] + ", " +
                              (float) origLPS_B[1] + ", " + (float) origLPS_B[2] + "\n");
            ViewUserInterface.getReference().setDataText("Image B origins (in LPS order) after matching orientation: " +
                                                         (float) origLPS_B[0] + ", " + (float) origLPS_B[1] + ", " +
                                                         (float) origLPS_B[2] + "\n");
            resultImgB.setImageOrientation(sourceImgA.getImageOrientation());
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug("Final ImageB origins in LPS order: " + (float) origLPS_B[0] + ", " +
                              (float) origLPS_B[1] + "\n");
            ViewUserInterface.getReference().setDataText("Image B origins (in LPS order) after matching orientation: " +
                                                         (float) origLPS_B[0] + ", " + (float) origLPS_B[1] + "\n");
        } // else nDims == 2

        // Update file info for result images.
        updateFileInfo(resultImgB, dimB, resB, origLPS_B);
    }

    /**
     * DOCUMENT ME!
     */
    private void matchOrigins() {

        /**
         *   Add margins to images A and B so that their origins match.
         */

        int addA[], addB[], tID;
        double[] eps;
        TransMatrix tMatBkp;
        AlgorithmAddMargins algoMarginsA, algoMarginsB;
        boolean newOriginA = false;
        boolean newOriginB = false;
        int i;

        try {
            addA = new int[nDims];
            addB = new int[nDims];
            eps = new double[nDims];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmMatchImages in matchOrigins reports: Out of memory");
            setCompleted(false);
            stopped = true;

            return;
        }

        // If all the start locations for Image A are 0.0, assume that start locations
        // aren't known and shouldn't be used in the registration.
        // Likewise for Image B.  Set the diff to zero.
        if (((nDims == 3) &&
                 (((origLPS_A[0] == 0.0) && (origLPS_A[1] == 0.0) && (origLPS_A[2] == 0.0)) ||
                      ((origLPS_B[0] == 0.0) && (origLPS_B[1] == 0.0) && (origLPS_B[2] == 0.0)))) ||
                ((nDims == 2) &&
                     (((origLPS_A[0] == 0.0) && (origLPS_A[1] == 0.0)) ||
                          ((origLPS_B[0] == 0.0) && (origLPS_B[1] == 0.0))))) {
            Preferences.debug("Start locations for either Image A or Image B are all zeros.  Start location won't be used " +
                              " in registration.\n");
        } else {

            // Convert origins to current image coordinate systems.
            // origImg_A = originLPS2Img(origLPS_A, resultImgA);
            // origImg_B = originLPS2Img(origLPS_B, resultImgB);
            doOrigins = false;

            // Find differences in image coordinate system and add appropriately.
            for (i = 0; i < nDims; i++) {
                oDiffD[i] = sign2LPS_A[i] * (origImg_B[i] - origImg_A[i]);
                eps[i] = resA[i];

                if (Math.abs(oDiffD[i]) > eps[i]) {
                    doOrigins = true;

                    if (oDiffD[i] > 0) {

                        // add pixels to the start (left, top, front) of imageB
                        addB[i] = (int) Math.round(oDiffD[i] / resB[i]);

                        // convert to pixels from mm
                        origImg_B[i] = origImg_B[i] - (sign2LPS_A[i] * addB[i] * resB[i]);

                        if (!newB) {
                            resultImgB = (ModelImage) sourceImgB.clone();
                            newB = true;
                        }

                        newOriginB = true;
                        // The reason to set the origin this way and not simply set it equal to the origin from imageA
                        // is that the rounding to get the number of pixels may make the new origin for imageB not
                        // exactly equal to that of imageA.
                    } else {

                        // add pixels to the start (left, top, front) of imageA
                        addA[i] = -1 * (int) Math.round(oDiffD[i] / resA[i]);

                        // convert to pixels from mm
                        origImg_A[i] = origImg_A[i] - (sign2LPS_A[i] * addA[i] * resA[i]);

                        if (!newA) {
                            resultImgA = (ModelImage) sourceImgA.clone();
                            newA = true;
                        }

                        newOriginA = true;
                    }
                }
            }

            if (doOrigins) {
                Preferences.debug("Origins will be matched.\n");
            }

            // Call AlgorithmAddMargins.
            if (newOriginA) {

                if (nDims == 3) {
                    Preferences.debug("Adding margins to imageA: " + addA[0] + ", " + addA[1] + ", " + addA[2] + "\n");
                } // if (nDims == 3)
                else { // nDims == 2
                    Preferences.debug("Adding margins to imageA: " + addA[0] + ", " + addA[1] + "\n");
                } // else nDims == 2

                // update resolution and dimension variables
                for (i = 0; i < nDims; i++) {
                    dimA[i] = dimA[i] + addA[i];
                }

                if (nDims == 3) {
                    Preferences.debug("New ImageA dimensions (matchOrigins): " + dimA[0] + ", " + dimA[1] + ", " +
                                      dimA[2] + "\n");
                } // if (nDims == 3)
                else { // else nDims == 2
                    Preferences.debug("New ImageA dimensions (matchOrigins): " + dimA[0] + ", " + dimA[1] + "\n");
                } // else nDims == 2

                // resultImgA.setImageName("tempA");
                
                Vector mats = resultImgA.getMatrixHolder().getMatrices();
                
                if (nDims == 3) {
                    algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, addA[0], 0, addA[1], 0, addA[2], 0);
                } // if (nDims == 3)
                else { // nDims == 2
                    algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, addA[0], 0, addA[1], 0);
                } // else nDims == 2

                algoMarginsA.setRunningInSeparateThread(runningInSeparateThread);

                algoMarginsA.run();

                if (algoMarginsA.isCompleted() == false) {
                    Preferences.debug("algoMarginsA in matchOrigins failed.");
                    algoMarginsA.finalize();
                    algoMarginsA = null;
                    setCompleted(false);
                    stopped = true;

                    return;
                }

                resultImgA.disposeLocal();
                resultImgA = algoMarginsA.getSrcImage();
                resultImgA.setImageName(nameA);
                algoMarginsA.finalize();
                algoMarginsA = null;

                resultImgA.getMatrixHolder().replaceMatrices(mats);

                for (i = 0; i < nDims; i++) {

                    switch (axisOrientA[i]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                        case FileInfoBase.ORI_L2R_TYPE:
                            origLPS_A[i] = origImg_A[0];
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                        case FileInfoBase.ORI_P2A_TYPE:
                            origLPS_A[i] = origImg_A[1];
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                        case FileInfoBase.ORI_S2I_TYPE:
                            origLPS_A[i] = origImg_A[2];
                            break;
                    } // switch(axisOrientA[i])
                }

                updateFileInfo(resultImgA, dimA, resA, origLPS_A);
            }

            if (newOriginB) {

                if (nDims == 3) {
                    Preferences.debug("Adding margins to imageB: " + addB[0] + ", " + addB[1] + ", " + addB[2] + "\n");
                } // if (nDims == 3)
                else { // nDims == 2
                    Preferences.debug("Adding margins to imageB: " + addB[0] + ", " + addB[1] + "\n");
                } // else nDims == 2

                // update resolution and dimension variables
                for (i = 0; i < nDims; i++) {
                    dimB[i] = dimB[i] + addB[i];
                }

                if (nDims == 3) {
                    Preferences.debug("New ImageB dimensions (matchOrigins): " + dimB[0] + ", " + dimB[1] + ", " +
                                      dimB[2] + "\n");
                } // if (nDims == 3)
                else { // nDims == 2
                    Preferences.debug("New ImageB dimensions (matchOrigins): " + dimB[0] + ", " + dimB[1] + "\n");
                } // else nDims == 2

                resultImgB.setImageName("tempB");
                
                Vector mats = resultImgB.getMatrixHolder().getMatrices();

                if (nDims == 3) {
                    algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, addB[0], 0, addB[1], 0, addB[2], 0);
                } // if (nDims == 3)
                else { // nDims == 2
                    algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, addB[0], 0, addB[1], 0);
                } // else nDims == 2

                algoMarginsB.setRunningInSeparateThread(runningInSeparateThread);
                algoMarginsB.run();

                if (algoMarginsB.isCompleted() == false) {
                    Preferences.debug("algoMarginsB in matchOrigins failed.");
                    algoMarginsB.finalize();
                    algoMarginsB = null;
                    setCompleted(false);
                    stopped = true;

                    return;
                }

                resultImgB.disposeLocal();
                resultImgB = algoMarginsB.getSrcImage();
                resultImgB.setImageName(nameB);
                algoMarginsB.finalize();
                algoMarginsB = null;

                resultImgB.getMatrixHolder().replaceMatrices(mats);

                for (i = 0; i < nDims; i++) {

                    switch (axisOrientB[i]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                        case FileInfoBase.ORI_L2R_TYPE:
                            origLPS_B[i] = origImg_B[0];
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                        case FileInfoBase.ORI_P2A_TYPE:
                            origLPS_B[i] = origImg_B[1];
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                        case FileInfoBase.ORI_S2I_TYPE:
                            origLPS_B[i] = origImg_B[2];
                            break;
                    } // switch(axisOrientB[i])
                }

                updateFileInfo(resultImgB, dimB, resB, origLPS_B);
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void matchResolutions() {

        /**
         *   Match resolutions - resample both images so that they have the higher of the two  resolutions (i.e. lower
         * pixel size) in each direction.  If resByRef is set to true, then resolution will match reference image (image
         * A),  regardless of which resolution is higher.
         */

        double fovA, fovB;
        int tIDA, tIDB;
        boolean newResA = false;
        boolean newResB = false;
        int i;
        int units2D[] = null;

        for (i = 0; i < nDims; i++) {
            fovA = resA[i] * dimA[i]; // field of view in this dimension
            fovB = resB[i] * dimB[i];

            if (resA[i] != resB[i]) {

                // can overwrite the resolution and dimension variables b/c no longer need originals.
                if ((resA[i] < resB[i]) || resByRef) {
                    resB[i] = resA[i]; // lower pixel size in this dimension
                    newResB = true;
                    dimB[i] = (int) Math.round(fovB / resB[i]);
                } else {
                    resA[i] = resB[i];
                    newResA = true;
                    dimA[i] = (int) Math.round(fovA / resA[i]);
                }
            }
        }

        TransMatrix identMatrix = new TransMatrix(nDims + 1); // constructor sets matrix to identity
        TransMatrix tMatBkp;

        if (newResA) {
            newA = true;

            // resultImgA definitely has not been created at this point.
            resultImgA = (ModelImage) sourceImgA.clone();
            resultImgA.setImageName("tempA");

            if (nDims == 3) {
                Preferences.debug("Creating new version of Image A with resolutions: " + (float) resA[0] + ", " +
                                  (float) resA[1] + ", " + (float) resA[2] + " and dimensions: " + dimA[0] + ", " +
                                  dimA[1] + ", " + dimA[2] + "\n");

                // Call AlgorithmTransform with padding.
                algoTransform = new AlgorithmTransform(resultImgA, identMatrix, AlgorithmTransform.TRILINEAR,
                                                       (float) resA[0], (float) resA[1], (float) resA[2], dimA[0],
                                                       dimA[1], dimA[2],
                                                       false, false, true);
            } // if (nDims == 3)
            else { // nDims == 2
                Preferences.debug("Creating new version of Image A with resolutions: " + (float) resA[0] + ", " +
                                  (float) resA[1] + " and dimensions: " + dimA[0] + ", " + dimA[1] + "\n");

                // Call AlgorithmTransform with padding.
                units2D = new int[2];
                units2D[0] = sourceImgA.getUnitsOfMeasure(0);
                units2D[1] = sourceImgA.getUnitsOfMeasure(1);
                algoTransform = new AlgorithmTransform(resultImgA, identMatrix, AlgorithmTransform.BILINEAR,
                                                       (float) resA[0], (float) resA[1], dimA[0], dimA[1], 
                                                       units2D,
                                                       false, false,
                                                       true);
            }

            algoTransform.setRunningInSeparateThread(runningInSeparateThread);
            algoTransform.setFillValue((float) padValue);
            algoTransform.run();

            if (algoTransform.isCompleted() == false) {
                Preferences.debug("algoTransform in matchResolutions failed - imageA.");
                algoTransform.finalize();
                algoTransform = null;
                setCompleted(false);
                stopped = true;

                return;
            }

            resultImgA.disposeLocal();
            resultImgA = algoTransform.getTransformedImage();
            algoTransform.finalize();
            algoTransform = null;

            // Update file info for result images.
            resultImgA.setImageName(nameA);
            resultImgA.getMatrixHolder().replaceMatrices(sourceImgA.getMatrixHolder().getMatrices());

            for (i = 0; i < nDims; i++) {
                origLPS_A[i] = (double) resultImgA.getFileInfo(0).getOrigin(i);
            }

            updateFileInfo(resultImgA, dimA, resA, origLPS_A);
        } else {
            Preferences.debug("No need to resample imageA.  Already at minimum resolution.\n");
        }

        if (newResB) {

            if (!newB) {
                resultImgB = (ModelImage) sourceImgB.clone();
                newB = true;
            }

            Vector mats = resultImgB.getMatrixHolder().getMatrices();
            resultImgB.setImageName("tempB");

            if (nDims == 3) {
                Preferences.debug("Creating new version of Image B with resolutions: " + (float) resB[0] + ", " +
                                  (float) resB[1] + ", " + (float) resB[2] + " and dimensions: " + dimB[0] + ", " +
                                  dimB[1] + ", " + dimB[2] + "\n");

                // Call AlgorithmTransform with padding.
                algoTransform = new AlgorithmTransform(resultImgB, identMatrix, AlgorithmTransform.TRILINEAR,
                                                       (float) resB[0], (float) resB[1], (float) resB[2], dimB[0],
                                                       dimB[1], dimB[2], 
                                                       false, false, true);
            } // if (nDims == 3)
            else { // nDims == 2
                Preferences.debug("Creating new version of Image B with resolutions: " + (float) resB[0] + ", " +
                                  (float) resB[1] + " and dimensions: " + dimB[0] + ", " + dimB[1] + "\n");

                // Call AlgorithmTransform with padding.
                units2D = new int[2];
                units2D[0] = sourceImgB.getUnitsOfMeasure(0);
                units2D[1] = sourceImgB.getUnitsOfMeasure(1);
                algoTransform = new AlgorithmTransform(resultImgB, identMatrix, AlgorithmTransform.BILINEAR,
                                                       (float) resB[0], (float) resB[1], dimB[0], dimB[1], 
                                                       units2D,
                                                       false, false,
                                                       true);
            } // else nDims == 2

            algoTransform.setRunningInSeparateThread(runningInSeparateThread);
            algoTransform.setFillValue((float) padValue);
            algoTransform.run();

            if (algoTransform.isCompleted() == false) {
                Preferences.debug("algoTransform in matchResolutions failed - imageB.");
                algoTransform.finalize();
                algoTransform = null;
                setCompleted(false);
                stopped = true;

                return;
            }

            resultImgB.disposeLocal();
            resultImgB = algoTransform.getTransformedImage();
            algoTransform.finalize();
            algoTransform = null;

            // Update file info for result images.
            resultImgB.setImageName(nameB);
            resultImgB.getMatrixHolder().replaceMatrices(mats);

            for (i = 0; i < nDims; i++) {
                origLPS_B[i] = (double) resultImgB.getFileInfo(0).getOrigin(i);
            }

            updateFileInfo(resultImgB, dimB, resB, origLPS_B);
        } else {
            Preferences.debug("No need to resample imageB.  Already at minimum resolution.\n\n");
        }
    }

    /**
     * Update fileInfoBase.
     *
     * @param  image  DOCUMENT ME!
     * @param  dim    DOCUMENT ME!
     * @param  res    DOCUMENT ME!
     * @param  start  DOCUMENT ME!
     */
    private void updateFileInfo(ModelImage image, int[] dim, double[] res, double[] start) {
        int[] tempI3 = new int[nDims];
        float[] tempF3 = new float[nDims];
        float slicePos;
        int lastSlice;
        FileInfoBase fileInfoB;

        if (nDims == 3) {
            lastSlice = dim[2];
            slicePos = (float) start[2];
        } else {
            lastSlice = 1;
            slicePos = 0.0f;
        }

        // Set file properties of result image.
        for (int i = 0; i < nDims; i++) {
            tempI3[i] = dim[i];
        }

        for (int i = 0; i < nDims; i++) {
            tempF3[i] = (float) res[i];
        }

        image.setExtents(tempI3);

        for (int i = 0; i < lastSlice; i++) {
            fileInfoB = (FileInfoBase) image.getFileInfo(i);
            fileInfoB.setResolutions(tempF3);

            if (nDims == 3) {
                fileInfoB.setSliceThickness(tempF3[2]);
            }

            fileInfoB.setOrigin((float) start[0], 0);
            fileInfoB.setOrigin((float) start[1], 1);
            fileInfoB.setOrigin(slicePos, 2);

            if (nDims == 3) {
                slicePos += (float) (sign2LPS_A[2] * res[2]);
            } // if (nDims == 3)
        }
    }

    /**
     * Update fileInfoBase.
     *
     * @param  image  DOCUMENT ME!
     * @param  dim    DOCUMENT ME!
     * @param  res    DOCUMENT ME!
     * @param  start  DOCUMENT ME!
     * @param  tID    DOCUMENT ME!
     */
    /*
    private void updateFileInfo(ModelImage image, int[] dim, double[] res, double[] start, int tID) {
        int[] tempI3 = new int[nDims];
        float[] tempF3 = new float[nDims];
        float slicePos;
        int lastSlice;
        FileInfoBase fileInfoB;

        if (nDims == 3) {
            lastSlice = dim[2];
            slicePos = (float) start[2];
        } else {
            lastSlice = 1;
            slicePos = 0.0f;
        }

        // Set file properties of result image.
        for (int i = 0; i < nDims; i++) {
            tempI3[i] = dim[i];
        }

        for (int i = 0; i < nDims; i++) {
            tempF3[i] = (float) res[i];
        }

        image.setExtents(tempI3);

        for (int i = 0; i < lastSlice; i++) {
            fileInfoB = (FileInfoBase) image.getFileInfo(i);
            fileInfoB.setResolutions(tempF3);

            if (nDims == 3) {
                fileInfoB.setSliceThickness(tempF3[2]);
            }

            fileInfoB.setOrigin((float) start[0], 0);
            fileInfoB.setOrigin((float) start[1], 1);
            fileInfoB.setOrigin(slicePos, 2);

            if (nDims == 3) {
                slicePos += (float) (sign2LPS_A[2] * res[2]);
            } // if (nDims == 3)
        }
    }
    */
}
