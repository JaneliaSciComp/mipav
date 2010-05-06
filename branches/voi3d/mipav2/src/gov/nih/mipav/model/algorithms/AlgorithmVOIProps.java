package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.MipavCoordinateSystems;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.swing.*;


/**
 * This class calculates a properties of an image defined by a VOI. Attributes include: volume, area, number of pixels,
 * center of mass, average pixel intensity, standard deviation of intensity, eccentricity, ,principalAxis,
 * coefficient of skewness, and coefficient of kurtosis.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/algorithms/AlgorithmVOIProps.java $ $Revision: 44 $ $Date:
 *           12/19/05 4:51p $</p>
 */

public class AlgorithmVOIProps extends AlgorithmBase implements VOIStatisticList {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int PROCESS_PER_VOI = 0;

    /** DOCUMENT ME! */
    public static final int PROCESS_PER_SLICE_AND_CONTOUR = 1;

    /** DOCUMENT ME! */
    public static final int PROCESS_PER_SLICE = 2;

    /** DOCUMENT ME! */
    public static final int PROCESS_PER_CONTOUR = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The VOI on which to perform the calculations. */
    private VOI activeVOI;

    /** Formatting for float values into strings. */
    private DecimalFormat nf;

    /** How the VOI calculations should be performed (entire, contour, slice). */
    private int processType = PROCESS_PER_VOI;

    /** Vector to hold all properties calculated within the algorithm for later access. */
    private Vector<VOIStatisticalProperties> propertyList;

    /** Whether or not to exclude a range of values. */
    private int rangeFlag;

    /** Whether or not to calculate largest slice distance, true by default */
    private boolean sliceDistanceFlag;

    /** Whether or not to calculate largest distance (only 3D), true by default */
    private boolean distanceFlag;

    /** Vector of all VOIs that will have calculations performed. */
    private ViewVOIVector selectedVOIset;

    /** Whether or not to show totals for each calculation. */
    private boolean showTotals = false;

    /** Boolean for if the algorithm should ONLY check active contours */
    private boolean doOnlyActiveContours = false;

    /**The top-level group of threads used for calculating. */
    private ThreadGroup calcGroup = new ThreadGroup("CalcVOI");

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor. sets the source image of the algorithm, and presets the algorithm to calculate properties of 3D
     * images as a volume of interest, rather than by slice.
     *
     * @param  srcImg  image model that contain the VOI
     * @param  voiSet  The VOIs that will be calculated
     */
    public AlgorithmVOIProps(ModelImage srcImg, ViewVOIVector voiSet) {
        this(srcImg, PROCESS_PER_VOI, voiSet);
    }

    /**
     * constructor.
     *
     * @param  srcImg       image model that contain the VOI
     * @param  processType  perform the property calculations for each slice, rather than for whole volume of interest
     * @param  voiSet     The VOIs that will be calculated
     */
    public AlgorithmVOIProps(ModelImage srcImg, int processType, ViewVOIVector voiSet) {
        this(srcImg, processType, 0, voiSet);
    }

    /**
     * constructor. note that if there are no VOIs to act on, this constructor returns quietly.
     *
     * @param  srcImg     image model that contain the VOI
     * @param  pType      list of items to perform the statistics operations on
     * @param  rangeFlag  Whether the range of values specified by the statistics generator should be ignored
     * @param  voiSet     The VOIs that will be calculated
     */
    public AlgorithmVOIProps(ModelImage srcImg, int pType, int rangeFlag, ViewVOIVector voiSet) {
        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        this.rangeFlag = rangeFlag;
        this.sliceDistanceFlag = true;
        this.distanceFlag = true;
        this.srcImage = srcImg;
        this.processType = pType;

        if (srcImage.getNDims() == 2) {
            pType = PROCESS_PER_SLICE;
        }

        selectedVOIset = voiSet;

        if (selectedVOIset.size() == 0) {
            return;
        }

        // initial storage ....
        // this represents 1 vector for each contour/
        // can be more selected contours!!!!!!!!
        initialiseDataHolders(selectedVOIset.size());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Gets the area of the VOI; return area defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getArea() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.areaDescription)).floatValue();
    } // {return area;}

    /**
     * Gets the average intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgInten() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.avgIntensity)).floatValue();
    } // {return avgInten;}

    /**
     * Gets the average intensity of the Blue channel of VOI return average intensity of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.avgIntensity +
        "Blue")).floatValue();
    } // {return avgIntenB;}

    /**
     * Gets the average intensity of the Green channel of VOI return average intensity of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.avgIntensity +
        "Green")).floatValue();
    } // {return avgIntenG;}

    /**
     * Gets the average intensity of the Red channel of VOI return average intensity of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.avgIntensity +
        "Red")).floatValue();
    } // {return avgIntenR;}



    /**
     * Gets the median
     *
     * @return  DOCUMENT ME!
     */
    public float getMedian() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.median)).floatValue();
    }

    /**
     * Gets the median of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.median +
        "Blue")).floatValue();
    }

    /**
     * Gets the median of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.median +
        "Green")).floatValue();
    } 

    /**
     * Gets the median of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.median +
        "Red")).floatValue();
    } 





    /**
     * Gets the mode
     *
     * @return  DOCUMENT ME!
     */
    public float getMode() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.mode)).floatValue();
    }

    /**
     * Gets the mode of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.mode +
        "Blue")).floatValue();
    }

    /**
     * Gets the mode of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.mode +
        "Green")).floatValue();
    } 

    /**
     * Gets the mode of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.mode +
        "Red")).floatValue();
    } 





    /**
     * Gets the mode
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCount() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.modeCount)).intValue();
    }

    /**
     * Gets the mode of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountB() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.modeCount +
        "Blue")).intValue();
    }

    /**
     * Gets the mode of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountG() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.modeCount +
        "Green")).intValue();
    } 

    /**
     * Gets the mode of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountR() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.modeCount +
        "Red")).intValue();
    }






    /**
     * Gets the the geometric center of the VOI ; return geometric center defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs
    public String getGeometricCenter() {
        VOIStatisticalProperties p = (VOIStatisticalProperties) propertyList.firstElement(); 
        return (p).getProperty(VOIStatisticalProperties.geometricCenterDescription);
    } // {return gcPt;}

    /**
     * Gets the the center of mass of the VOI ; return center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs
    public String getCenterOfMass() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.massCenterDescription);
    } // {return cenMassPt;}

    /**
     * Gets the the red center of mass of the VOI ; return red center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs, etc.
    public String getCenterOfMassR() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.massCenterDescription + "Red");
    } // {return cenMassPtR;}

    /**
     * Gets the the green center of mass of the VOI ; return green center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterOfMassG() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.massCenterDescription + "Green");
    } // {return cenMassPtG;}

    /**
     * Gets the the blue center of mass of the VOI ; return blue center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterOfMassB() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.massCenterDescription + "Blue");
    } // {return cenMassPtB;}

    /**
     * Gets the eccentricity of the VOI: 1 = line, 0 = circle; return eccentricity of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getEccentricity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.eccentricityDescription)).floatValue();
    } // {return eccentricity;}

    /**
     * Gets the major axis of VOI (only valid for 2D object); return major axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMajorAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.majorAxisDescription)).floatValue();
    } // {return majorAxis;}

    /**
     * Gets the maximum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.maxIntensity)).floatValue();
    } // {return maxIntensity;}

    /**
     * Gets the maximum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityBlue() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.maxIntensity +
        "Blue")).floatValue();
    } // {return maxIntenBlue;}

    /**
     * Gets the maximum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityGreen() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.maxIntensity +
        "Green")).floatValue();
    } // {return maxIntenGreen;}

    /**
     * Gets the maximum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityRed() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.maxIntensity +
        "Red")).floatValue();
    } // {return maxIntenRed;}

    /**
     * Gets the greatest distance between any two point of the VOI return distance.
     *
     * @return  DOCUMENT ME!
     */
    public double getMaxWidth() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.maxWidthDescription)).floatValue();
    } // {return maxDistance;}

    /**
     * Gets the minimum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.minIntensity)).floatValue();
    } // {return minIntensity;}

    /**
     * Gets the minimum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityBlue() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.minIntensity +
        "Blue")).floatValue();
    } // {return minIntenBlue;}

    /**
     * Gets the minimum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityGreen() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.minIntensity +
        "Green")).floatValue();
    } // {return minIntenGreen;}

    /**
     * Gets the minimum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityRed() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.minIntensity +
        "Red")).floatValue();
    } // {return minIntenRed;}

    /**
     * Gets the minor axis of VOI (only valid for 2D object); return minor axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinorAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.minorAxisDescription)).floatValue();
    } // {return minorAxis;}

    /**
     * Gets the the number of pixels return number of pixels defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getNVoxels() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.quantityDescription)).intValue();
    } // {return nVox;}

    /**
     * Gets the perimeter of the VOI (in terms of res).
     *
     * @return  String perimeter string
     */
    public String getPerimeter() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.perimeterDescription);
    } // {return perimeter;}

    /**
     * Gets the largest line segment totally contained within a VOI slice (in terms of res).
     * If this unexpectedly returns zero, make sure you have not inadvertently set sliceDistanceFlag to false.
     *
     * @return  String largest slice distance string
     */
    public String getLargestSliceDistance() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.largestSliceDistanceDescription);
    } // {return largestSliceDistance;}

    /**
     * Gets the largest line segment totally contained within a 3D VOI (in terms of res).
     * If this unexpectedly returns zero, make sure you have not inadvertently set distanceFlag to false.
     *
     * @return  String largest distance string
     */
    public String getLargestDistance() {
        return propertyList.firstElement().getProperty(VOIStatisticalProperties.largestDistanceDescription);
    } // {return largestDistance;}

    /**
     * Gets the principle axis of VOI (only valid for 2D object); return pricipal axis angle of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getPrincipalAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.axisDescription)).floatValue();
    } // {return principalAxis;}


    /**
     * Reports if algorithm is performing calcs per slice, per contour, or for entire VOI.
     *
     * @return  processType (int for process type)
     */
    public int getProcessType() {
        return processType;
    }

    /**
     * Gets the standard deviation of image intensities return standard deviation of image intensities defined by the
     * VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDev() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.deviationDescription)).floatValue();
    } // {return stdDev;}

    /**
     * Gets the get standard deviation of image intensities (blue channel) return standard deviation of image
     * intensities defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.deviationDescription +
        "Blue")).floatValue();
    } // {return stdDevB;}

    /**
     * Gets the standard deviation of image intensities (green channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.deviationDescription +
        "Green")).floatValue();
    } // {return stdDevG;}


    /**
     * Gets the standard deviation of image intensities (red channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.deviationDescription +
        "Red")).floatValue();
    } // {return stdDevR;}

    /**
     * Gets the sum of image intensities defined by the VOI
     * @return
     */
    public float getSumIntensities() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.sumIntensities)).floatValue();
    }

    /**
     * Gets the sum of red channel image intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.sumIntensities + "Red")).floatValue();
    }

    /**
     * Gets the sum of green channel image intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.sumIntensities + "Green")).floatValue();
    }

    /**
     * Gets the sum of blue channel mage intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.sumIntensities + "Blue")).floatValue();
    }




    /**
     * DOCUMENT ME!
     *
     * @param   aVOI  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOIStatisticalProperties getVOIProperties(VOI aVOI) {
        return (VOIStatisticalProperties) propertyList.elementAt(selectedVOIset.indexOf(aVOI));
    }

    /**
     * Gets the volume of the VOI; return volume defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getVolume() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.volumeDescription)).floatValue();
    } // {return volume;}

    /**
     * Gets the coefficient of skewness of the pixel values in the VOI
     * @return
     */
    public float getSkewness() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.skewnessDescription)).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the red pixel values in the VOI
     * @return
     */
    public float getSkewnessR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.skewnessDescription + "Red")).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the green pixel values in the VOI
     * @return
     */
    public float getSkewnessG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.skewnessDescription + "Green")).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the blue pixel values in the VOI
     * @return
     */
    public float getSkewnessB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.skewnessDescription + "Blue")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the pixel values in the VOI
     * @return
     */
    public float getKurtosis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.kurtosisDescription)).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the red pixel values in the VOI
     * @return
     */
    public float getKurtosisR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.kurtosisDescription + "Red")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the green pixel values in the VOI
     * @return
     */
    public float getKurtosisG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.kurtosisDescription + "Green")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the blue pixel values in the VOI
     * @return
     */
    public float getKurtosisB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticalProperties.kurtosisDescription + "Blue")).floatValue();
    }

    /**
     * Accessor that indicates if the source image is a color image.
     *
     * @return  <code>true</code> if the image is a color image.
     */
    public boolean isColor() {
        return srcImage.isColorImage();
    }

    /**
     * Creates the list of labels to use in the checkboxes.
     *
     * @return  DOCUMENT ME!
     */
    public String[] makeStatisticListDescriptions() {
        return statisticDescription;
    }

    // the following is to preserve old functionality

    /**
     * Begins execution of the software.
     */
    public void runAlgorithm() {
        int threadsCreated = 0;
        try {

            // stats for 2D images are by defined to work only on the VOIs in the slice
            // find the VOI to calculate for (need not be active!!)
            // System.out.println("algoVOIprops n VOIs = " + selectedVOIset.size());
            for (int i = 0; i < selectedVOIset.size(); i++) {
                activeVOI = (VOI) selectedVOIset.elementAt(i);
                if (!doOnlyActiveContours) {
                    activeVOI.setAllActive(false);
                    if (srcImage.getNDims() == 2) {
                        Calc2D calcVOI = new Calc2D(activeVOI);
                        Thread t = new Thread(calcGroup, calcVOI, activeVOI.getName());
                        t.start();
                        threadsCreated++;
                    } else if (srcImage.getNDims() > 2) {
                        Calc34D calcVOI = new Calc34D(activeVOI);
                        Thread t = new Thread(calcGroup, calcVOI, activeVOI.getName());
                        t.start();
                        threadsCreated++;
                    }
                } else {
                    //create active contour subset to pass in
                    VOI tempVOI = (VOI)activeVOI.clone();
                    for (int j = 0; j < tempVOI.getCurvesTemp().length; j++) {
                        for (int k = tempVOI.getCurvesTemp()[j].size() - 1; k >= 0 ; k--) {
                            if (!tempVOI.getCurvesTemp()[j].elementAt(k).isActive()) {
                                tempVOI.getCurvesTemp()[j].remove(k);
                                System.err.println("removed curve at: " + k + " on slice: " + j);
                            }
                        }
                    }
                    tempVOI.setAllActive(false);

                    selectedVOIset.remove(i);
                    selectedVOIset.insertElementAt(tempVOI, i);
                    if (srcImage.getNDims() == 2) {
                        Calc2D calcVOI = new Calc2D(tempVOI);
                        Thread t = new Thread(calcGroup, calcVOI, tempVOI.getName());
                        t.start();
                        threadsCreated++;
                    } else if (srcImage.getNDims() > 2) {
                        Calc34D calcVOI = new Calc34D(tempVOI);
                        Thread t = new Thread(calcGroup, calcVOI, tempVOI.getName());
                        t.start();
                        threadsCreated++;
                    }
                }

            }
        } catch (NullPointerException npe) {

            // to handle the srcImage == null problem
            // if (noisyProcess) {
            if (srcImage == null) {
                displayError("Source Image is null");
            }

            MipavUtil.displayError("Null pointer refers to:\n" + npe.getMessage());
            npe.printStackTrace();
            // }
        }

        Thread[] activeGroup = new Thread[threadsCreated];
        //extra threads cannot be silently ignored, so maximum is set
        calcGroup.enumerate(activeGroup);
        for(int i=0; i<activeGroup.length; i++) {
            if(activeGroup[i] != null && activeGroup[i].isAlive()) {
                try {
                    Preferences.debug("Waiting for "+activeGroup[i].getName()+" to finish.\n");
                    activeGroup[i].join();
                    Preferences.debug("Still waiting for "+activeGroup[i].getName()+" to finish.\n");
                } catch(InterruptedException e) {
                    System.err.println("Thread waiting process did not complete normally.");
                    System.err.println("Please restart calculation process.");
                }
            }
        }

        setCompleted(true);
    }

    /**
     * Sets the String float formatter to trim to numDecimal number of decimals.
     *
     * @param  numDecimal  int number of decimals
     * @param  doForce     boolean force numDecimal or allow zero
     */
    public void setPrecisionDisplay(int numDecimal, boolean doForce) {
        nf.setMaximumFractionDigits(numDecimal);

        if (doForce) {
            nf.setMinimumFractionDigits(numDecimal);
        } else {
            nf.setMinimumFractionDigits(0);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aVOI  DOCUMENT ME!
     */
    public void setSelectedVOI(VOI aVOI) {
        activeVOI = aVOI;
    }

    /**
     * Sets the flag for calculating totals ONLY for active contours
     * @param doActive
     */
    public void setDoOnlyActiveContours(boolean doActive) {
        this.doOnlyActiveContours = doActive;
    }

    /**
     * Sets whether the largest slice distance is calculated.  Defaults
     * to true, set to false if VOI will not complete.
     * @param sliceDistanceFlag
     */
    public void setSliceDistanceFlag(boolean sliceDistanceFlag) {
        this.sliceDistanceFlag = sliceDistanceFlag;
    }

    /**
     * Sets whether the largest distance is calculated. Defaults to true,
     * set to false if VOI will not complete.
     * @param distanceFlag
     */
    public void setDistanceFlag(boolean distanceFlag) {
        this.distanceFlag = distanceFlag;
    }

    /**
     * tells the algorithm to total the property calculations.
     *
     * @param  totals  DOCUMENT ME!
     */
    public void setShowTotals(boolean totals) {
        showTotals = totals;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  list  DOCUMENT ME!
     */
    public void setVOIList(ListModel list) {

        // convert model of a list to a vector of VOI elements.
        selectedVOIset = new ViewVOIVector(list.getSize());

        for (int i = 0; i < list.getSize(); i++) {
            selectedVOIset.addElement((VOI)list.getElementAt(i));
        }

        if (selectedVOIset.size() != 0) {
            initialiseDataHolders(selectedVOIset.size());
        }

    }

    /**
     * sets the selected VOIset and the data set data storage sizes based on the number of curves in the Vector.
     *
     * @param  vvv  DOCUMENT ME!
     */
    public void setVOIList(ViewVOIVector vvv) {
        selectedVOIset = vvv;

        if (vvv.size() != 0) {
            initialiseDataHolders(vvv.size());
        }
    }

    private class Calc2D implements Runnable {

        /** The VOI being used just for this calculation (may be a single curve of a single slice or entire VOI **/
        private VOI calcSelectedVOI;

        public Calc2D(VOI selectedVOI) {
            this.calcSelectedVOI = selectedVOI;
        }

        public void run() {
            double minIntensity = Double.MAX_VALUE, totalMinIntensity = Double.MAX_VALUE;
            double maxIntensity = -Double.MAX_VALUE, totalMaxIntensity = -Double.MAX_VALUE;
            double minIntenRed = Double.MAX_VALUE, totalMinIntenRed = Double.MAX_VALUE;
            double maxIntenRed = -Double.MAX_VALUE, totalMaxIntenRed = -Double.MAX_VALUE;
            double minIntenGreen = Double.MAX_VALUE, totalMinIntenGreen = Double.MAX_VALUE;
            double maxIntenGreen = -Double.MAX_VALUE, totalMaxIntenGreen = -Double.MAX_VALUE;
            double minIntenBlue = Double.MAX_VALUE, totalMinIntenBlue = Double.MAX_VALUE;
            double maxIntenBlue = -Double.MAX_VALUE, totalMaxIntenBlue = -Double.MAX_VALUE;
            double avgInten = 0;
            double avgIntenR = 0;
            double avgIntenG = 0;
            double avgIntenB = 0;
            double stdDev = 0, stdDevR = 0, stdDevG = 0, stdDevB = 0;
            double skewness = 0, skewnessR = 0, skewnessG = 0, skewnessB = 0;
            double kurtosis = 0, kurtosisR = 0, kurtosisG = 0, kurtosisB = 0;
            double R2, R3, R4, G2, G3, G4, B2, B3, B4, s2, s3, s4;
            double diff, diffR, diffG, diffB;
            double sum = 0, sumR = 0, sumG = 0, sumB = 0;
            double sum2 = 0, sumR2 = 0, sumG2 = 0, sumB2 = 0, area = 0;
            double sum3 = 0, sumR3 = 0, sumG3 = 0, sumB3 = 0;
            double sum4 = 0, sumR4 = 0, sumG4 = 0, sumB4 = 0;
            double moment2, moment2R, moment2G, moment2B;
            double moment3, moment3R, moment3G, moment3B;
            double moment4, moment4R, moment4G, moment4B;
            double totalSum = 0, totalSumR = 0, totalSumG = 0, totalSumB = 0, totalArea = 0;
            double totalSum2 = 0, totalSumR2 = 0, totalSumG2 = 0, totalSumB2 = 0;
            double totalSum3 = 0, totalSumR3 = 0, totalSumG3 = 0, totalSumB3 = 0;
            double totalSum4 = 0, totalSumR4 = 0, totalSumG4 = 0, totalSumB4 = 0;
            double totalAxis = 0, totalEcc = 0;
            double totalMajorAxis = 0;
            double totalMinorAxis = 0;
            int nVox = 0, totalNVox = 0;
            Vector3f totalC = new Vector3f(0, 0, 0); // geometric center
            float[] imgBuffer;
            float[] tmpPAxis = null;
            float[] tmpEcc = null;
            float[] tmpMajorAxis = null;
            float[] tmpMinorAxis = null;
            double totalPerimeter = 0;
            double largestSliceDistance = 0;
            Vector3f gCenter;
            String comStr;
            int x;
            int y;
            int z;
            double xMass, yMass, zMass, xMassR, yMassR, zMassR, xMassG, yMassG, zMassG, xMassB, yMassB, zMassB;
            double xCOM, yCOM, zCOM, xCOMR, yCOMR, zCOMR, xCOMG, yCOMG, zCOMG, xCOMB, yCOMB, zCOMB;
            double totalXMass = 0, totalYMass = 0, totalZMass = 0;
            double totalXMassR = 0, totalYMassR = 0, totalZMassR = 0;
            double totalXMassG = 0, totalYMassG = 0, totalZMassG = 0;
            double totalXMassB = 0, totalYMassB = 0, totalZMassB = 0;
            int xUnits, yUnits, zUnits;
            String xStr, yStr, zStr;
            String unit2DStr = null;
            String unit3DStr = null;
            String unitStr = null;
            Vector3f centerPt;

            float totalMode = 0;
            float totalMaxCount = 0;
            float totalModeR = 0;
            float totalMaxCountR = 0;
            float totalModeG = 0;
            float totalMaxCountG = 0;
            float totalModeB = 0;
            float totalMaxCountB = 0;

            float[] totalBuff = null;
            float[] totalBuffR = null;
            float[] totalBuffG = null;
            float[] totalBuffB = null;

            double totalMedian = 0;
            double totalMedianR = 0;
            double totalMedianG = 0;
            double totalMedianB = 0;


            int length;

            Vector[] contours;
            BitSet mask;
            VOIStatisticalProperties statProperty = getVOIProperties(calcSelectedVOI);

            try {
                int bufferFactor = 1;

                if (srcImage.isColorImage()) {
                    bufferFactor = 4;
                }

                length = bufferFactor * srcImage.getSliceSize();
                imgBuffer = new float[length];
                srcImage.exportData(0, length, imgBuffer); // locks and releases lock
                mask = new BitSet(length);
                tmpPAxis = new float[1];
                tmpEcc = new float[1];
                tmpMajorAxis = new float[1];
                tmpMinorAxis = new float[1];
            } catch (IOException error) {
                displayError("Algorithm VOI Properties: Image(s) locked");
                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                displayError("Algorithm VOI Properties: Out of Memory");
                setCompleted(false);

                return;
            }

            FileInfoBase[] fileInfo = srcImage.getFileInfo();

            float ignoreMin = calcSelectedVOI.getMinimumIgnore();
            float ignoreMax = calcSelectedVOI.getMaximumIgnore();
            float perimeter = 0f;
            double largestContourDistance = 0;

            contours = calcSelectedVOI.getCurvesTemp();

            xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            if (xUnits != FileInfoBase.UNKNOWN_MEASURE) {
                xStr = "X " + FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);    
            }
            else {
                xStr = "X ";
            }
            yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            if (yUnits != FileInfoBase.UNKNOWN_MEASURE) {
                yStr = "Y " + FileInfoBase.getUnitsOfMeasureAbbrevStr(yUnits);    
            }
            else {
                yStr = "Y ";
            }
            unit2DStr = xStr + "\t" + yStr;
            if (srcImage.getNDims() > 2) {
                zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
                if (zUnits != FileInfoBase.UNKNOWN_MEASURE) {
                    zStr = "Z " + FileInfoBase.getUnitsOfMeasureAbbrevStr(zUnits);  
                    if ((srcImage.getFileInfo(0).getOrigin()[0] != 0) || (srcImage.getFileInfo(0).getOrigin()[1] != 0) ||
                            (srcImage.getFileInfo(0).getOrigin()[2] != 0)) {
                        zStr = zStr + "\tR-L:\tA-P:\tI-S:";
                    }
                }
                else {
                    zStr = "Z ";
                }
                unit3DStr = unit2DStr + "\t" + zStr;
            }

            if (processType == PROCESS_PER_SLICE_AND_CONTOUR) {
                // since we're in a 2D image, contours.length should = 1

                for (int q = 0; q < contours.length; q++) {

                    // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                    for (int r = 0; r < contours[q].size(); r++) {
                        sum = 0;
                        sumR = 0;
                        sumG = 0;
                        sumB = 0;
                        nVox = 0;
                        /*
                        ((VOIContour) (contours[q].elementAt(r))).secondOrderAttributes(srcImage.getExtents()[0],
                                                                                        srcImage.getExtents()[1],
                                                                                        srcImage.getFileInfo(0).getResolutions()[0],
                                                                                        srcImage.getFileInfo(0).getResolutions()[1],
                                                                                        srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                                                                        srcImage.getFileInfo(0).getUnitsOfMeasure()[1],
                                                                                        tmpPAxis, tmpEcc, tmpMajorAxis,
                                                                                        tmpMinorAxis);
                         */

                        ((VOIContour) (contours[q].elementAt(r))).secondOrderAttributes(srcImage,
                                tmpPAxis, tmpEcc, tmpMajorAxis,
                                tmpMinorAxis);
                        statProperty.setProperty(VOIStatisticList.axisDescription + "0;" + r, nf.format(tmpPAxis[0]));
                        statProperty.setProperty(VOIStatisticList.eccentricityDescription + "0;" + r, nf.format(tmpEcc[0]));
                        statProperty.setProperty(VOIStatisticList.majorAxisDescription + "0;" + r,
                                nf.format(tmpMajorAxis[0]));
                        statProperty.setProperty(VOIStatisticList.minorAxisDescription + "0;" + r,
                                nf.format(tmpMinorAxis[0]));

                        gCenter = ((VOIContour) (contours[q].elementAt(r))).getGeometricCenter();
                        z = (int)Math.round(gCenter.Z);
                        gCenter.X *= srcImage.getFileInfo(0).getResolutions()[0];
                        gCenter.Y *= srcImage.getFileInfo(0).getResolutions()[1];
                        unitStr = unit2DStr + "\tZ";

                        if (srcImage.getNDims() > 2) {
                            gCenter.Z *= srcImage.getFileInfo(0).getResolutions()[2];
                            unitStr = unit3DStr;
                        }


                        comStr = unitStr + "\n\t\t" + nf.format(gCenter.X) + "\t" + nf.format(gCenter.Y) + "\t" +
                        nf.format(gCenter.Z);
                        comStr = addScannerLabels(comStr, gCenter);
                        statProperty.setProperty(VOIStatisticList.geometricCenterDescription + "0;" + r, comStr);


                        totalEcc += tmpEcc[0];
                        totalAxis += tmpPAxis[0];
                        totalMajorAxis += tmpMajorAxis[0];
                        totalMinorAxis += tmpMinorAxis[0];
                        perimeter = (float)((VOIContour) (contours[q].elementAt(r))).getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());
                        totalPerimeter += perimeter;

                        if(sliceDistanceFlag) {
                            largestContourDistance = ((VOIContour)(contours[q].elementAt(r))).calcLargestSliceDistance(srcImage.getFileInfo(0).getResolutions()[0],
                                    srcImage.getFileInfo(0).getResolutions()[1]);
                            largestSliceDistance = Math.max(largestContourDistance, largestSliceDistance);
                        }

                        totalC = calcSelectedVOI.getGeometricCenter();

                        ((VOIContour) (contours[q].elementAt(r))).setActive(true);

                        // mask.clear(); only works for Java1.4
                        for (int m = 0; m < mask.size(); m++) {
                            mask.clear(m);
                        }

                        calcSelectedVOI.createActiveContourBinaryMask(mask, srcImage.getExtents()[0], srcImage.getExtents()[1]);
                        ((VOIContour) (contours[q].elementAt(r))).setActive(false);

                        if (srcImage.isColorImage()) {
                            minIntenRed = Double.MAX_VALUE;
                            maxIntenRed = -Double.MAX_VALUE;
                            minIntenGreen = Double.MAX_VALUE;
                            maxIntenGreen = -Double.MAX_VALUE;
                            minIntenBlue = Double.MAX_VALUE;
                            maxIntenBlue = -Double.MAX_VALUE;

                            for (int i = 0; i < length; i += 4) {

                                if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                                    sumR += imgBuffer[i + 1];
                                    sumG += imgBuffer[i + 2];
                                    sumB += imgBuffer[i + 3];
                                    nVox++;

                                    if (imgBuffer[i + 1] < minIntenRed) {
                                        minIntenRed = imgBuffer[i + 1];
                                    }

                                    if (imgBuffer[i + 1] > maxIntenRed) {
                                        maxIntenRed = imgBuffer[i + 1];
                                    }

                                    if (imgBuffer[i + 2] < minIntenGreen) {
                                        minIntenGreen = imgBuffer[i + 2];
                                    }

                                    if (imgBuffer[i + 2] > maxIntenGreen) {
                                        maxIntenGreen = imgBuffer[i + 2];
                                    }

                                    if (imgBuffer[i + 3] < minIntenBlue) {
                                        minIntenBlue = imgBuffer[i + 3];
                                    }

                                    if (imgBuffer[i + 3] > maxIntenBlue) {
                                        maxIntenBlue = imgBuffer[i + 3];
                                    }
                                }
                            }


                            //second pass...
                            float[] buffR = new float[nVox];
                            float[] buffG = new float[nVox];
                            float[] buffB = new float[nVox];
                            int k = 0;
                            for (int i = 0; i < length; i += 4) {
                                if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                                    buffR[k] = imgBuffer[i + 1];
                                    buffG[k] = imgBuffer[i + 2];
                                    buffB[k] = imgBuffer[i + 3];
                                    k++; 
                                }
                            }

                            //red

                            Arrays.sort(buffR);
                            float[] sortedR = buffR;
                            if(totalBuffR == null) {
                                totalBuffR = sortedR;
                            }else {
                                float[] tempR = totalBuffR;
                                int tempLengthR = tempR.length;
                                int sortedLengthR =  sortedR.length;
                                int newLengthR = tempLengthR + sortedLengthR;
                                totalBuffR = new float[newLengthR];
                                int counterR = 0;
                                for(int i=0;i<tempR.length;i++) {
                                    totalBuffR[counterR] = tempR[i];
                                    counterR++;
                                }
                                for(int i=counterR;i<sortedR.length;i++) {
                                    totalBuffR[counterR] = sortedR[i];
                                    counterR++;
                                }
                            }

                            int cntR = sortedR.length;
                            double medianR;
                            float tempR = 0;
                            int countR = 0;
                            float modeR = 0;
                            float maxCountR = 0;
                            for(int i=0;i<sortedR.length;i++) {
                                if(i==0) {
                                    tempR = sortedR[i];
                                    countR = 1;
                                    modeR = tempR;
                                    maxCountR = 1;
                                }else {
                                    if(sortedR[i] == tempR) {
                                        countR++;
                                        if(countR > maxCountR) {
                                            maxCountR = countR;
                                            modeR = tempR;
                                        }

                                    }else {
                                        tempR = sortedR[i];
                                        countR = 1;
                                    }
                                } 
                            }
                            if (cntR%2 == 1) {
                                medianR = sortedR[cntR/2] ;   
                            }
                            else {
                                medianR = (sortedR[cntR/2] + sortedR[(cntR/2) - 1])/2.0;
                            }


                            //green
                            Arrays.sort(buffG);
                            float[] sortedG = buffG;
                            if(totalBuffG == null) {
                                totalBuffG = sortedG;
                            }else {
                                float[] tempG = totalBuffG;
                                int tempLengthG = tempG.length;
                                int sortedLengthG =  sortedG.length;
                                int newLengthG = tempLengthG + sortedLengthG;
                                totalBuffG = new float[newLengthG];
                                int counterG = 0;
                                for(int i=0;i<tempG.length;i++) {
                                    totalBuffG[counterG] = tempG[i];
                                    counterG++;
                                }
                                for(int i=counterG;i<sortedG.length;i++) {
                                    totalBuffG[counterG] = sortedG[i];
                                    counterG++;
                                }
                            }
                            int cntG = sortedG.length;
                            double medianG;
                            float tempG = 0;
                            int countG = 0;
                            float modeG = 0;
                            float maxCountG = 0;
                            for(int i=0;i<sortedG.length;i++) {
                                if(i==0) {
                                    tempG = sortedG[i];
                                    countG = 1;
                                    modeG = tempG;
                                    maxCountG = 1;
                                }else {
                                    if(sortedG[i] == tempG) {
                                        countG++;
                                        if(countG > maxCountG) {
                                            maxCountG = countG;
                                            modeG = tempG;
                                        }

                                    }else {
                                        tempG = sortedG[i];
                                        countG = 1;
                                    }
                                } 
                            }
                            if (cntG%2 == 1) {
                                medianG = sortedG[cntG/2] ;   
                            }
                            else {
                                medianG = (sortedG[cntG/2] + sortedG[(cntG/2) - 1])/2.0;
                            }



                            //blue
                            Arrays.sort(buffB);
                            float[] sortedB = buffB;

                            if(totalBuffB == null) {
                                totalBuffB = sortedB;
                            }else {
                                float[] tempB = totalBuffB;
                                int tempLengthB = tempB.length;
                                int sortedLengthB =  sortedB.length;
                                int newLengthB = tempLengthB + sortedLengthB;
                                totalBuffB = new float[newLengthB];
                                int counterB = 0;
                                for(int i=0;i<tempB.length;i++) {
                                    totalBuffB[counterB] = tempB[i];
                                    counterB++;
                                }
                                for(int i=counterB;i<sortedB.length;i++) {
                                    totalBuffB[counterB] = sortedB[i];
                                    counterB++;
                                }
                            }

                            int cntB = sortedB.length;
                            double medianB;
                            float tempB = 0;
                            int countB = 0;
                            float modeB = 0;
                            float maxCountB = 0;
                            for(int i=0;i<sortedB.length;i++) {
                                if(i==0) {
                                    tempB = sortedB[i];
                                    countB = 1;
                                    modeB = tempB;
                                    maxCountB = 1;
                                }else {
                                    if(sortedB[i] == tempB) {
                                        countB++;
                                        if(countB > maxCountB) {
                                            maxCountB = countB;
                                            modeB = tempB;
                                        }

                                    }else {
                                        tempB = sortedB[i];
                                        countB = 1;
                                    }
                                } 
                            }
                            if (cntB%2 == 1) {
                                medianB = sortedB[cntB/2] ;   
                            }
                            else {
                                medianB = (sortedB[cntB/2] + sortedB[(cntB/2) - 1])/2.0;
                            }



                            if(maxCountR > totalMaxCountR) {
                                totalModeR = modeR;
                                totalMaxCountR = maxCountR;
                            } 

                            if(maxCountG > totalMaxCountG) {
                                totalModeG = modeG;
                                totalMaxCountG = maxCountG;
                            }    

                            if(maxCountB > totalMaxCountB) {
                                totalModeB = modeB;
                                totalMaxCountB = maxCountB;
                            }   










                            avgIntenR = sumR / nVox;
                            avgIntenG = sumG / nVox;
                            avgIntenB = sumB / nVox;

                            totalSumR += sumR;
                            totalSumG += sumG;
                            totalSumB += sumB;
                            totalNVox += nVox;

                            if (minIntenRed < totalMinIntenRed) {
                                totalMinIntenRed = minIntenRed;
                            }

                            if (minIntenGreen < totalMinIntenRed) {
                                totalMinIntenGreen = minIntenGreen;
                            }

                            if (minIntenBlue < totalMinIntenRed) {
                                totalMinIntenBlue = minIntenBlue;
                            }

                            if (maxIntenRed > totalMaxIntenRed) {
                                totalMaxIntenRed = maxIntenRed;
                            }

                            if (maxIntenGreen > totalMaxIntenGreen) {
                                totalMaxIntenGreen = maxIntenGreen;
                            }

                            if (maxIntenBlue > totalMaxIntenBlue) {
                                totalMaxIntenBlue = maxIntenBlue;
                            }

                            statProperty.setProperty(VOIStatisticList.minIntensity + "Red" + "0;" + r,
                                    nf.format(minIntenRed));
                            statProperty.setProperty(VOIStatisticList.maxIntensity + "Red" + "0;" + r,
                                    nf.format(maxIntenRed));
                            statProperty.setProperty(VOIStatisticList.minIntensity + "Green" + "0;" + r,
                                    nf.format(minIntenGreen));
                            statProperty.setProperty(VOIStatisticList.maxIntensity + "Green" + "0;" + r,
                                    nf.format(maxIntenGreen));
                            statProperty.setProperty(VOIStatisticList.minIntensity + "Blue" + "0;" + r,
                                    nf.format(minIntenBlue));
                            statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue" + "0;" + r,
                                    nf.format(maxIntenBlue));
                            statProperty.setProperty(VOIStatisticList.avgIntensity + "Red" + "0;" + r,
                                    nf.format(avgIntenR));
                            statProperty.setProperty(VOIStatisticList.avgIntensity + "Green" + "0;" + r,
                                    nf.format(avgIntenG));
                            statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue" + "0;" + r,
                                    nf.format(avgIntenB));
                            statProperty.setProperty(VOIStatisticList.quantityDescription + "0;" + r, nf.format(nVox));
                            statProperty.setProperty(VOIStatisticList.sumIntensities + "Red" + "0;" + r, nf.format(sumR));
                            statProperty.setProperty(VOIStatisticList.sumIntensities + "Green" + "0;" + r, nf.format(sumG));
                            statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue" + "0;" + r, nf.format(sumB));

                            statProperty.setProperty(VOIStatisticList.median + "Red" + "0;" + r, nf.format(medianR));
                            statProperty.setProperty(VOIStatisticList.median + "Green" + "0;" + r, nf.format(medianG));
                            statProperty.setProperty(VOIStatisticList.median + "Blue" + "0;" + r, nf.format(medianB));

                            statProperty.setProperty(VOIStatisticList.mode + "Red" + "0;" + r, nf.format(modeR));
                            statProperty.setProperty(VOIStatisticList.mode + "Green" + "0;" + r, nf.format(modeG));
                            statProperty.setProperty(VOIStatisticList.mode + "Blue" + "0;" + r, nf.format(modeB));

                            statProperty.setProperty(VOIStatisticList.modeCount + "Red" + "0;" + r, nf.format(maxCountR));
                            statProperty.setProperty(VOIStatisticList.modeCount + "Green" + "0;" + r, nf.format(maxCountG));
                            statProperty.setProperty(VOIStatisticList.modeCount + "Blue" + "0;" + r, nf.format(maxCountB));

                        } else {
                            minIntensity = Double.MAX_VALUE;
                            maxIntensity = -Double.MAX_VALUE;


                            for (int i = 0; i < length; i++) {

                                if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                                    sum += imgBuffer[i];
                                    nVox++;

                                    if (imgBuffer[i] < minIntensity) {
                                        minIntensity = imgBuffer[i];
                                    }

                                    if (imgBuffer[i] > maxIntensity) {
                                        maxIntensity = imgBuffer[i];
                                    }
                                }
                            }


                            //second pass...
                            float[] buff = new float[nVox];
                            int k = 0;
                            for (int i = 0; i < length; i++) {
                                if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                                    buff[k] = imgBuffer[i];
                                    k++; 
                                }
                            }
                            Arrays.sort(buff);
                            float[] sorted = buff;

                            if(totalBuff == null) {
                                totalBuff = sorted;
                            }else {
                                float[] temp = totalBuff;
                                int tempLength = temp.length;
                                int sortedLength =  sorted.length;
                                int newLength = tempLength + sortedLength;
                                totalBuff = new float[newLength];
                                int counter = 0;
                                for(int i=0;i<temp.length;i++) {
                                    totalBuff[counter] = temp[i];
                                    counter++;
                                }
                                for(int i=counter;i<sorted.length;i++) {
                                    totalBuff[counter] = sorted[i];
                                    counter++;
                                }
                            }

                            int cnt = sorted.length;
                            double median;
                            float temp = 0;
                            int count = 0;
                            float mode = 0;
                            float maxCount = 0;
                            for(int i=0;i<sorted.length;i++) {
                                if(i==0) {
                                    temp = sorted[i];
                                    count = 1;
                                    mode = temp;
                                    maxCount = 1;
                                }else {
                                    if(sorted[i] == temp) {
                                        count++;
                                        if(count > maxCount) {
                                            maxCount = count;
                                            mode = temp;
                                        }

                                    }else {
                                        temp = sorted[i];
                                        count = 1;
                                    }
                                } 
                            }
                            if (cnt%2 == 1) {
                                median = sorted[cnt/2] ;   
                            }
                            else {
                                median = (sorted[cnt/2] + sorted[(cnt/2) - 1])/2.0;
                            }

                            if(maxCount > totalMaxCount) {
                                totalMode = mode;
                                totalMaxCount = maxCount;

                            }

                            avgInten = sum / nVox;

                            totalSum += sum;
                            totalNVox += nVox;

                            if (minIntensity < totalMinIntensity) {
                                totalMinIntensity = minIntensity;
                            }

                            if (maxIntensity > totalMaxIntensity) {
                                totalMaxIntensity = maxIntensity;
                            }

                            statProperty.setProperty(VOIStatisticList.minIntensity + "0;" + r, nf.format(minIntensity));
                            statProperty.setProperty(VOIStatisticList.maxIntensity + "0;" + r, nf.format(maxIntensity));
                            statProperty.setProperty(VOIStatisticList.avgIntensity + "0;" + r, nf.format(avgInten));
                            statProperty.setProperty(VOIStatisticList.quantityDescription + "0;" + r, nf.format(nVox));
                            statProperty.setProperty(VOIStatisticList.sumIntensities + "0;" + r, nf.format(sum));
                            statProperty.setProperty(VOIStatisticList.median + "0;" + r, nf.format(median));
                            statProperty.setProperty(VOIStatisticList.mode + "0;" + r, nf.format(mode));
                            statProperty.setProperty(VOIStatisticList.modeCount + "0;" + r, nf.format(maxCount));

                        }

                        area = nVox * (fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1]);
                        statProperty.setProperty(VOIStatisticList.areaDescription + "0;" + r, nf.format(area));
                        statProperty.setProperty(VOIStatisticList.volumeDescription + "0;" + r, nf.format(area));

                        // add perimeter
                        statProperty.setProperty(VOIStatisticList.perimeterDescription + "0;" + r, nf.format(perimeter));
                        statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + "0;" + r,
                                nf.format(largestContourDistance));

                        totalArea += area;

                        // calculate standard deviation, coefficient of skewness, and coefficient of kurtosis
                        sum2 = sumR2 = sumG2 = sumB2 = sum3 = sumR3 = sumG3 = sumB3 = sum4 = sumR4 = sumG4 = sumB4 = 0;
                        // Calculate centers of mass
                        xMass = yMass = zMass = 0;
                        xMassR = yMassR = zMassR = xMassG = yMassG = zMassG = xMassB = yMassB = zMassB = 0;

                        int cnt = 0;

                        if (srcImage.isColorImage()) {

                            for (int i = 0; i < length; i += 4) {

                                if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                        !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                                    x = (i/4) % srcImage.getExtents()[0];
                                    y = (i/4) / srcImage.getExtents()[0];
                                    xMassR += x * imgBuffer[i + 1];
                                    yMassR += y * imgBuffer[i + 1];
                                    zMassR += z * imgBuffer[i + 1];
                                    diffR = imgBuffer[i + 1] - avgIntenR;
                                    R2 = diffR * diffR;
                                    sumR2 += R2;
                                    R3 = R2 * diffR;
                                    sumR3 += R3;
                                    R4 = R3 * diffR;
                                    sumR4 += R4;
                                    xMassG += x * imgBuffer[i + 2];
                                    yMassG += y * imgBuffer[i + 2];
                                    zMassG += z * imgBuffer[i + 2];
                                    diffG = imgBuffer[i + 2] - avgIntenG;
                                    G2 = diffG * diffG;
                                    sumG2 += G2;
                                    G3 = G2 * diffG;
                                    sumG3 += G3;
                                    G4 = G3 * diffG;
                                    sumG4 += G4;
                                    xMassB += x * imgBuffer[i + 3];
                                    yMassB += y * imgBuffer[i + 3];
                                    zMassB += z * imgBuffer[i + 3];
                                    diffB = imgBuffer[i + 3] - avgIntenB;
                                    B2 = diffB * diffB;
                                    sumB2 += B2;
                                    B3 = B2 * diffB;
                                    sumB3 += B3;
                                    B4 = B3 * diffB;
                                    sumB4 += B4;
                                    cnt++;
                                }
                            }

                            stdDevR = (float) Math.sqrt(sumR2 / (cnt-1));
                            stdDevG = (float) Math.sqrt(sumG2 / (cnt-1));
                            stdDevB = (float) Math.sqrt(sumB2 / (cnt-1));
                            statProperty.setProperty(VOIStatisticList.deviationDescription + "Red" + "0;" + r,
                                    nf.format(stdDevR));
                            statProperty.setProperty(VOIStatisticList.deviationDescription + "Green" + "0;" + r,
                                    nf.format(stdDevG));
                            statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue" + "0;" + r,
                                    nf.format(stdDevB));
                            // moments around the mean
                            moment2R = sumR2/cnt;
                            moment2G = sumG2/cnt;
                            moment2B = sumB2/cnt;
                            moment3R = sumR3/cnt;
                            moment3G = sumG3/cnt;
                            moment3B = sumB3/cnt;
                            moment4R = sumR4/cnt;
                            moment4G = sumG4/cnt;
                            moment4B = sumB4/cnt;
                            skewnessR = (float)(moment3R/Math.pow(moment2R, 1.5));
                            skewnessG = (float)(moment3G/Math.pow(moment2G, 1.5));
                            skewnessB = (float)(moment3B/Math.pow(moment2B, 1.5));
                            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red" + "0;" + r,
                                    nf.format(skewnessR));
                            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green" + "0;" + r,
                                    nf.format(skewnessG));
                            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue" + "0;" + r,
                                    nf.format(skewnessB));
                            kurtosisR = moment4R/(moment2R * moment2R);
                            kurtosisG = moment4G/(moment2G * moment2G);
                            kurtosisB = moment4B/(moment2B * moment2B);
                            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red" + "0;" + r,
                                    nf.format(kurtosisR));
                            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green" + "0;" + r,
                                    nf.format(kurtosisG));
                            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue" + "0;" + r,
                                    nf.format(kurtosisB));
                            // Centers of mass
                            xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/sumR;
                            yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/sumR;
                            zCOMR = zMassR/sumR;
                            unitStr = unit2DStr + "\tZ";
                            if (srcImage.getNDims() > 2) {
                                zCOMR *= srcImage.getFileInfo(0).getResolutions()[2];
                                unitStr = unit3DStr;
                            }

                            comStr = unitStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" +
                            nf.format(zCOMR);
                            centerPt = new Vector3f();
                            centerPt.X = (float)xCOMR;
                            centerPt.Y = (float)yCOMR;
                            centerPt.Z = (float)zCOMR;
                            comStr = addScannerLabels(comStr, centerPt);
                            statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red" + "0;" + r, comStr);

                            xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/sumG;
                            yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/sumG;
                            zCOMG= zMassG/sumG;
                            if (srcImage.getNDims() > 2) {
                                zCOMG *= srcImage.getFileInfo(0).getResolutions()[2];
                            }

                            comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" +
                            nf.format(zCOMG);
                            centerPt = new Vector3f();
                            centerPt.X = (float)xCOMG;
                            centerPt.Y = (float)yCOMG;
                            centerPt.Z = (float)zCOMG;
                            comStr = addScannerLabels(comStr, centerPt);
                            statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green" + "0;" + r, comStr);

                            xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/sumB;
                            yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/sumB;
                            zCOMB = zMassB/sumB;
                            if (srcImage.getNDims() > 2) {
                                zCOMB *= srcImage.getFileInfo(0).getResolutions()[2];
                            }

                            comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" +
                            nf.format(zCOMB);
                            centerPt = new Vector3f();
                            centerPt.X = (float)xCOMB;
                            centerPt.Y = (float)yCOMB;
                            centerPt.Z = (float)zCOMB;
                            comStr = addScannerLabels(comStr, centerPt);
                            statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue" + "0;" + r, comStr);
                            totalSumR2 += sumR2;
                            totalSumG2 += sumG2;
                            totalSumB2 += sumB2;
                            totalSumR3 += sumR3;
                            totalSumG3 += sumG3;
                            totalSumB3 += sumB3;
                            totalSumR4 += sumR4;
                            totalSumG4 += sumG4;
                            totalSumB4 += sumB4;
                            totalXMassR += xMassR;
                            totalYMassR += yMassR;
                            totalZMassR += zMassR;
                            totalXMassG += xMassG;
                            totalYMassG += yMassG;
                            totalZMassG += zMassG;
                            totalXMassB += xMassB;
                            totalYMassB += yMassB;
                            totalZMassB += zMassB;
                        } else {

                            for (int i = 0; i < length; i++) {

                                if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                                    x = i % srcImage.getExtents()[0];
                                    y = i / srcImage.getExtents()[0];
                                    xMass += x * imgBuffer[i];
                                    yMass += y * imgBuffer[i];
                                    zMass += z * imgBuffer[i];
                                    diff = imgBuffer[i] - avgInten;
                                    s2 = diff * diff;
                                    sum2 += s2;
                                    s3 = s2 * diff;
                                    sum3 += s3;
                                    s4 = s3 * diff;
                                    sum4 += s4;
                                    cnt++;
                                }
                            }

                            stdDev = (float) Math.sqrt(sum2 / (cnt-1));
                            statProperty.setProperty(VOIStatisticList.deviationDescription + "0;" + r, nf.format(stdDev));
                            moment2 = sum2/cnt;
                            moment3 = sum3/cnt;
                            moment4 = sum4/cnt;
                            skewness = (float)(moment3/Math.pow(moment2, 1.5));
                            statProperty.setProperty(VOIStatisticList.skewnessDescription + "0;" + r, nf.format(skewness));
                            kurtosis = moment4/(moment2 * moment2);
                            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "0;" + r, nf.format(kurtosis));
                            // Center of mass
                            xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/sum;
                            yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/sum;
                            zCOM = zMass/sum;
                            unitStr = unit2DStr + "\tZ";
                            if (srcImage.getNDims() > 2) {
                                zCOM *= srcImage.getFileInfo(0).getResolutions()[2];
                                unitStr = unit3DStr;
                            }

                            comStr = unitStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" +
                            nf.format(zCOM);
                            centerPt = new Vector3f();
                            centerPt.X = (float)xCOM;
                            centerPt.Y = (float)yCOM;
                            centerPt.Z = (float)zCOM;
                            comStr = addScannerLabels(comStr, centerPt);
                            statProperty.setProperty(VOIStatisticList.massCenterDescription + "0;" + r, comStr);
                            totalSum2 += sum2;
                            totalSum3 += sum3;
                            totalSum4 += sum4;
                            totalXMass += xMass;
                            totalYMass += yMass;
                            totalZMass += zMass;
                        }
                    }

                }



                if (srcImage.isColorImage()) {
                    Arrays.sort(totalBuffR);
                    float[] totalSortedR = totalBuffR;
                    int totalCntR = totalSortedR.length;
                    if (totalCntR%2 == 1) {
                        totalMedianR = totalSortedR[totalCntR/2] ;   
                    }
                    else {
                        totalMedianR = (totalSortedR[totalCntR/2] + totalSortedR[(totalCntR/2) - 1])/2.0;
                    }
                    Arrays.sort(totalBuffG);
                    float[] totalSortedG = totalBuffG;
                    int totalCntG = totalSortedG.length;
                    if (totalCntG%2 == 1) {
                        totalMedianG = totalSortedG[totalCntG/2] ;   
                    }
                    else {
                        totalMedianG = (totalSortedG[totalCntG/2] + totalSortedG[(totalCntG/2) - 1])/2.0;
                    }
                    Arrays.sort(totalBuffB);
                    float[] totalSortedB = totalBuffB;
                    int totalCntB = totalSortedB.length;
                    if (totalCntB%2 == 1) {
                        totalMedianB = totalSortedB[totalCntB/2] ;   
                    }
                    else {
                        totalMedianB = (totalSortedB[totalCntB/2] + totalSortedB[(totalCntB/2) - 1])/2.0;
                    }
                }else {
                    Arrays.sort(totalBuff);
                    float[] totalSorted = totalBuff;
                    int totalCnt = totalSorted.length;
                    if (totalCnt%2 == 1) {
                        totalMedian = totalSorted[totalCnt/2] ;   
                    }
                    else {
                        totalMedian = (totalSorted[totalCnt/2] + totalSorted[(totalCnt/2) - 1])/2.0;
                    }
                }




                if (showTotals == true) {
                    statProperty.setProperty(VOIStatisticList.axisDescription + "Total", nf.format(totalAxis));
                    statProperty.setProperty(VOIStatisticList.eccentricityDescription + "Total", nf.format(totalEcc));
                    statProperty.setProperty(VOIStatisticList.majorAxisDescription + "Total", nf.format(totalMajorAxis));
                    statProperty.setProperty(VOIStatisticList.minorAxisDescription + "Total", nf.format(totalMinorAxis));

                    totalC.X *= srcImage.getFileInfo(0).getResolutions()[0];
                    totalC.Y *= srcImage.getFileInfo(0).getResolutions()[1];

                    comStr = unit2DStr + "\n\t\t" + nf.format(totalC.X) + "\t" + nf.format(totalC.Y);

                    statProperty.setProperty(VOIStatisticList.geometricCenterDescription + "Total", comStr);
                    statProperty.setProperty(VOIStatisticList.areaDescription + "Total", nf.format(totalArea));
                    statProperty.setProperty(VOIStatisticList.volumeDescription + "Total", nf.format(totalArea));
                    statProperty.setProperty(VOIStatisticList.quantityDescription + "Total", nf.format(totalNVox));
                    statProperty.setProperty(VOIStatisticList.perimeterDescription + "Total", nf.format(totalPerimeter));
                    statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + "Total", nf.format(largestSliceDistance));

                    if (srcImage.isColorImage()) {
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Red" + "Total",
                                nf.format((float) Math.sqrt(totalSumR2 / (totalNVox-1))));
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Green" + "Total",
                                nf.format((float) Math.sqrt(totalSumG2 / (totalNVox-1))));
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue" + "Total",
                                nf.format((float) Math.sqrt(totalSumB2 / (totalNVox-1))));
                        moment2R = totalSumR2/totalNVox;
                        moment2G = totalSumG2/totalNVox;
                        moment2B = totalSumB2/totalNVox;
                        moment3R = totalSumR3/totalNVox;
                        moment3G = totalSumG3/totalNVox;
                        moment3B = totalSumB3/totalNVox;
                        moment4R = totalSumR4/totalNVox;
                        moment4G = totalSumG4/totalNVox;
                        moment4B = totalSumB4/totalNVox;
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red" + "Total",
                                nf.format((float) (moment3R/Math.pow(moment2R, 1.5))));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green" + "Total",
                                nf.format((float) (moment3G/Math.pow(moment2G, 1.5))));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue" + "Total",
                                nf.format((float) (moment3B/Math.pow(moment2B, 1.5))));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red" + "Total",
                                nf.format(moment4R/(moment2R * moment2R)));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green" + "Total",
                                nf.format(moment4G/(moment2G * moment2G)));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue" + "Total",
                                nf.format(moment4B/(moment2B * moment2B)));
                        statProperty.setProperty(VOIStatisticList.minIntensity + "Red" + "Total",
                                nf.format(totalMinIntenRed));
                        statProperty.setProperty(VOIStatisticList.maxIntensity + "Red" + "Total",
                                nf.format(totalMaxIntenRed));
                        statProperty.setProperty(VOIStatisticList.minIntensity + "Green" + "Total",
                                nf.format(totalMinIntenGreen));
                        statProperty.setProperty(VOIStatisticList.maxIntensity + "Green" + "Total",
                                nf.format(totalMaxIntenGreen));
                        statProperty.setProperty(VOIStatisticList.minIntensity + "Blue" + "Total",
                                nf.format(totalMinIntenBlue));
                        statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue" + "Total",
                                nf.format(totalMaxIntenBlue));
                        statProperty.setProperty(VOIStatisticList.avgIntensity + "Red" + "Total",
                                nf.format(totalSumR / totalNVox));
                        statProperty.setProperty(VOIStatisticList.avgIntensity + "Green" + "Total",
                                nf.format(totalSumG / totalNVox));
                        statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue" + "Total",
                                nf.format(totalSumB / totalNVox));
                        statProperty.setProperty(VOIStatisticList.sumIntensities + "Red" + "Total", nf.format(totalSumR));
                        statProperty.setProperty(VOIStatisticList.sumIntensities + "Green" + "Total", nf.format(totalSumG));
                        statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue" + "Total", nf.format(totalSumB));
                        statProperty.setProperty(VOIStatisticList.mode + "Red" + "Total", nf.format(totalModeR));
                        statProperty.setProperty(VOIStatisticList.mode + "Green" + "Total", nf.format(totalModeG));
                        statProperty.setProperty(VOIStatisticList.mode + "Blue" + "Total", nf.format(totalModeB));

                        statProperty.setProperty(VOIStatisticList.modeCount + "Red" + "Total", nf.format(totalMaxCountR));
                        statProperty.setProperty(VOIStatisticList.modeCount + "Green" + "Total", nf.format(totalMaxCountG));
                        statProperty.setProperty(VOIStatisticList.modeCount + "Blue" + "Total", nf.format(totalMaxCountB));

                        statProperty.setProperty(VOIStatisticList.median + "Red" + "Total", nf.format(totalMedianR));
                        statProperty.setProperty(VOIStatisticList.median + "Green" + "Total", nf.format(totalMedianG));
                        statProperty.setProperty(VOIStatisticList.median + "Blue" + "Total", nf.format(totalMedianB));
                        // Centers of mass
                        xCOMR = totalXMassR * srcImage.getFileInfo(0).getResolutions()[0]/totalSumR;
                        yCOMR = totalYMassR * srcImage.getFileInfo(0).getResolutions()[1]/totalSumR;
                        zCOMR = totalZMassR/totalSumR;
                        unitStr = unit2DStr + "\tZ";
                        if (srcImage.getNDims() > 2) {
                            zCOMR *= srcImage.getFileInfo(0).getResolutions()[2];
                            unitStr = unit3DStr;
                        }

                        comStr = unitStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" +
                        nf.format(zCOMR);
                        centerPt = new Vector3f();
                        centerPt.X = (float)xCOMR;
                        centerPt.Y = (float)yCOMR;
                        centerPt.Z = (float)zCOMR;
                        comStr = addScannerLabels(comStr, centerPt);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red" + "Total", comStr);

                        xCOMG = totalXMassG * srcImage.getFileInfo(0).getResolutions()[0]/totalSumG;
                        yCOMG = totalYMassG * srcImage.getFileInfo(0).getResolutions()[1]/totalSumG;
                        zCOMG = totalZMassG/totalSumG;
                        if (srcImage.getNDims() > 2) {
                            zCOMG *= srcImage.getFileInfo(0).getResolutions()[2];
                        }

                        comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" +
                        nf.format(zCOMG);
                        centerPt = new Vector3f();
                        centerPt.X = (float)xCOMG;
                        centerPt.Y = (float)yCOMG;
                        centerPt.Z = (float)zCOMG;
                        comStr = addScannerLabels(comStr, centerPt);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green" + "Total", comStr);

                        xCOMB = totalXMassB * srcImage.getFileInfo(0).getResolutions()[0]/totalSumB;
                        yCOMB = totalYMassB * srcImage.getFileInfo(0).getResolutions()[1]/totalSumB;
                        zCOMB = totalZMassB/totalSumB;
                        if (srcImage.getNDims() > 2) {
                            zCOMB *= srcImage.getFileInfo(0).getResolutions()[2];
                        }

                        comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" +
                        nf.format(zCOMB);
                        centerPt = new Vector3f();
                        centerPt.X = (float)xCOMB;
                        centerPt.Y = (float)yCOMB;
                        centerPt.Z = (float)zCOMB;
                        comStr = addScannerLabels(comStr, centerPt);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue" + "Total", comStr);
                    } else {
                        statProperty.setProperty(VOIStatisticList.minIntensity + "Total", nf.format(totalMinIntensity));
                        statProperty.setProperty(VOIStatisticList.maxIntensity + "Total", nf.format(totalMaxIntensity));
                        statProperty.setProperty(VOIStatisticList.avgIntensity + "Total", nf.format(totalSum / totalNVox));
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Total",
                                nf.format((float) Math.sqrt(totalSum2/ (totalNVox-1))));
                        moment2 = totalSum2/totalNVox;
                        moment3 = totalSum3/totalNVox;
                        moment4 = totalSum4/totalNVox;
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Total",
                                nf.format((float) (moment3/Math.pow(moment2, 1.5))));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Total",
                                nf.format(moment4/(moment2 * moment2)));
                        statProperty.setProperty(VOIStatisticList.sumIntensities + "Total", nf.format(totalSum));
                        statProperty.setProperty(VOIStatisticList.mode + "Total", nf.format(totalMode));
                        statProperty.setProperty(VOIStatisticList.modeCount + "Total", nf.format(totalMaxCount));
                        // Center of mass
                        xCOM = totalXMass * srcImage.getFileInfo(0).getResolutions()[0]/totalSum;
                        yCOM = totalYMass * srcImage.getFileInfo(0).getResolutions()[1]/totalSum;
                        zCOM = totalZMass/totalSum;
                        unitStr = unit2DStr + "\tZ";
                        if (srcImage.getNDims() > 2) {
                            zCOM *= srcImage.getFileInfo(0).getResolutions()[2];
                            unitStr = unit3DStr;
                        }

                        comStr = unitStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" +
                        nf.format(zCOM);
                        centerPt = new Vector3f();
                        centerPt.X = (float)xCOM;
                        centerPt.Y = (float)yCOM;
                        centerPt.Z = (float)zCOM;
                        comStr = addScannerLabels(comStr, centerPt);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Total", comStr);
                    }
                }
            } else {
                VOIContour thisContour = ((VOIContour) (contours[0].elementAt(0)));
                /*
                thisContour.secondOrderAttributes(srcImage.getExtents()[0], srcImage.getExtents()[1],
                                                  srcImage.getFileInfo(0).getResolutions()[0],
                                                  srcImage.getFileInfo(0).getResolutions()[1],
                                                  srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                                  srcImage.getFileInfo(0).getUnitsOfMeasure()[1], tmpPAxis, tmpEcc,
                                                  tmpMajorAxis, tmpMinorAxis); */

                thisContour.secondOrderAttributes(srcImage, tmpPAxis, tmpEcc,
                        tmpMajorAxis, tmpMinorAxis);
                statProperty.setProperty(VOIStatisticList.axisDescription + "0;", nf.format(tmpPAxis[0]));
                statProperty.setProperty(VOIStatisticList.eccentricityDescription + "0;", nf.format(tmpEcc[0]));
                statProperty.setProperty(VOIStatisticList.majorAxisDescription + "0;", nf.format(tmpMajorAxis[0]));
                statProperty.setProperty(VOIStatisticList.minorAxisDescription + "0;", nf.format(tmpMinorAxis[0]));

                Vector3f selectedCOM = calcSelectedVOI.getGeometricCenter();

                selectedCOM.X *= srcImage.getFileInfo(0).getResolutions()[0];
                selectedCOM.Y *= srcImage.getFileInfo(0).getResolutions()[1];

                comStr = unit2DStr + "\n\t\t" +  nf.format(selectedCOM.X) + "\t" + nf.format(selectedCOM.Y);
                statProperty.setProperty(VOIStatisticList.geometricCenterDescription + "0;", comStr);

                statProperty.setProperty(VOIStatisticList.axisDescription, nf.format(tmpPAxis[0]));
                statProperty.setProperty(VOIStatisticList.eccentricityDescription, nf.format(tmpEcc[0]));
                statProperty.setProperty(VOIStatisticList.majorAxisDescription, nf.format(tmpMajorAxis[0]));
                statProperty.setProperty(VOIStatisticList.minorAxisDescription, nf.format(tmpMinorAxis[0]));
                statProperty.setProperty(VOIStatisticList.geometricCenterDescription, comStr);

                if (srcImage.getParentFrame() != null) {
                    calcSelectedVOI.createBinaryMask3D(mask,srcImage.getExtents()[0], srcImage.getExtents()[1], 
                            srcImage.getParentFrame().useXOR(), false);
                }
                else {
                    calcSelectedVOI.createBinaryMask3D(mask,srcImage.getExtents()[0], srcImage.getExtents()[1], 
                            false, false);   
                }

                // calc the perimeter
                totalPerimeter = 0f;
                largestSliceDistance = 0;

                for (int q = 0; q < contours.length; q++) {

                    // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                    for (int r = 0; r < contours[q].size(); r++) {
                        totalPerimeter += ((VOIContour) (contours[q].elementAt(r))).getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());
                        if(sliceDistanceFlag) {
                            largestSliceDistance = Math.max(largestSliceDistance,
                                    ((VOIContour)(contours[q].elementAt(r))).calcLargestSliceDistance(srcImage.getFileInfo(0).getResolutions()[0],
                                            srcImage.getFileInfo(0).getResolutions()[1]));
                        }
                    }
                }

                statProperty.setProperty(VOIStatisticList.perimeterDescription, nf.format(totalPerimeter));
                statProperty.setProperty(VOIStatisticList.perimeterDescription + "0;", nf.format(totalPerimeter));
                statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription, nf.format(largestSliceDistance));
                statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + "0;", nf.format(largestSliceDistance));

                if (srcImage.isColorImage()) {

                    for (int i = 0; i < length; i += 4) {

                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            sumR += imgBuffer[i + 1];
                            sumG += imgBuffer[i + 2];
                            sumB += imgBuffer[i + 3];
                            nVox++;

                            if (imgBuffer[i + 1] < minIntenRed) {
                                minIntenRed = imgBuffer[i + 1];
                            }

                            if (imgBuffer[i + 1] > maxIntenRed) {
                                maxIntenRed = imgBuffer[i + 1];
                            }

                            if (imgBuffer[i + 2] < minIntenGreen) {
                                minIntenGreen = imgBuffer[i + 2];
                            }

                            if (imgBuffer[i + 2] > maxIntenGreen) {
                                maxIntenGreen = imgBuffer[i + 2];
                            }

                            if (imgBuffer[i + 3] < minIntenBlue) {
                                minIntenBlue = imgBuffer[i + 3];
                            }

                            if (imgBuffer[i + 3] > maxIntenBlue) {
                                maxIntenBlue = imgBuffer[i + 3];
                            }
                        }
                    }




                    //second pass...
                    float[] buffR = new float[nVox];
                    float[] buffG = new float[nVox];
                    float[] buffB = new float[nVox];
                    int k = 0;
                    for (int i = 0; i < length; i += 4) {
                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            buffR[k] = imgBuffer[i + 1];
                            buffG[k] = imgBuffer[i + 2];
                            buffB[k] = imgBuffer[i + 3];
                            k++; 
                        }
                    }

                    //red
                    Arrays.sort(buffR);
                    float[] sortedR = buffR;
                    int cntR = sortedR.length;
                    double medianR;
                    float tempR = 0;
                    int countR = 0;
                    float modeR = 0;
                    float maxCountR = 0;
                    for(int i=0;i<sortedR.length;i++) {
                        if(i==0) {
                            tempR = sortedR[i];
                            countR = 1;
                            modeR = tempR;
                            maxCountR = 1;
                        }else {
                            if(sortedR[i] == tempR) {
                                countR++;
                                if(countR > maxCountR) {
                                    maxCountR = countR;
                                    modeR = tempR;
                                }

                            }else {
                                tempR = sortedR[i];
                                countR = 1;
                            }
                        } 
                    }
                    if (cntR%2 == 1) {
                        medianR = sortedR[cntR/2] ;   
                    }
                    else {
                        medianR = (sortedR[cntR/2] + sortedR[(cntR/2) - 1])/2.0;
                    }


                    //green
                    Arrays.sort(buffG);
                    float[] sortedG = buffG;
                    int cntG = sortedG.length;
                    double medianG;
                    float tempG = 0;
                    int countG = 0;
                    float modeG = 0;
                    float maxCountG = 0;
                    for(int i=0;i<sortedG.length;i++) {
                        if(i==0) {
                            tempG = sortedG[i];
                            countG = 1;
                            modeG = tempG;
                            maxCountG = 1;
                        }else {
                            if(sortedG[i] == tempG) {
                                countG++;
                                if(countG > maxCountG) {
                                    maxCountG = countG;
                                    modeG = tempG;
                                }

                            }else {
                                tempG = sortedG[i];
                                countG = 1;
                            }
                        } 
                    }
                    if (cntG%2 == 1) {
                        medianG = sortedG[cntG/2] ;   
                    }
                    else {
                        medianG = (sortedG[cntG/2] + sortedG[(cntG/2) - 1])/2.0;
                    }


                    //blue
                    Arrays.sort(buffB);
                    float[] sortedB = buffB;
                    int cntB = sortedB.length;
                    double medianB;
                    float tempB = 0;
                    int countB = 0;
                    float modeB = 0;
                    float maxCountB = 0;
                    for(int i=0;i<sortedB.length;i++) {
                        if(i==0) {
                            tempB = sortedB[i];
                            countB = 1;
                            modeB = tempB;
                            maxCountB = 1;
                        }else {
                            if(sortedB[i] == tempB) {
                                countB++;
                                if(countB > maxCountB) {
                                    maxCountB = countB;
                                    modeB = tempB;
                                }

                            }else {
                                tempB = sortedB[i];
                                countB = 1;
                            }
                        } 
                    }
                    if (cntB%2 == 1) {
                        medianB = sortedB[cntB/2] ;   
                    }
                    else {
                        medianB = (sortedB[cntB/2] + sortedB[(cntB/2) - 1])/2.0;
                    }

                    avgIntenR = sumR / nVox;
                    avgIntenG = sumG / nVox;
                    avgIntenB = sumB / nVox;

                    statProperty.setProperty(VOIStatisticList.minIntensity + "Red" + "0;", nf.format(minIntenRed));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Red" + "0;", nf.format(maxIntenRed));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Green" + "0;", nf.format(minIntenGreen));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Green" + "0;", nf.format(maxIntenGreen));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Blue" + "0;", nf.format(minIntenBlue));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue" + "0;", nf.format(maxIntenBlue));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Red" + "0;", nf.format(avgIntenR));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Green" + "0;", nf.format(avgIntenG));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue" + "0;", nf.format(avgIntenB));
                    statProperty.setProperty(VOIStatisticList.quantityDescription + "0;", nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Red" + "0;", nf.format(sumR));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Green" + "0;", nf.format(sumG));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue" + "0;", nf.format(sumB));
                    statProperty.setProperty(VOIStatisticList.median + "Red" + "0;", nf.format(medianR));
                    statProperty.setProperty(VOIStatisticList.median + "Green" + "0;", nf.format(medianG));
                    statProperty.setProperty(VOIStatisticList.median + "Blue" + "0;", nf.format(medianB));
                    statProperty.setProperty(VOIStatisticList.mode + "Red" + "0;", nf.format(modeR));
                    statProperty.setProperty(VOIStatisticList.mode + "Green" + "0;", nf.format(modeG));
                    statProperty.setProperty(VOIStatisticList.mode + "Blue" + "0;", nf.format(modeB));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Red" + "0;", nf.format(maxCountR));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Green" + "0;", nf.format(maxCountG));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Blue" + "0;", nf.format(maxCountB));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Red", nf.format(minIntenRed));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Red", nf.format(maxIntenRed));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Green", nf.format(minIntenGreen));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Green", nf.format(maxIntenGreen));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Blue", nf.format(minIntenBlue));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue", nf.format(maxIntenBlue));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Red", nf.format(avgIntenR));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Green", nf.format(avgIntenG));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue", nf.format(avgIntenB));
                    statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Red", nf.format(sumR));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Green", nf.format(sumG));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue", nf.format(sumB));
                    statProperty.setProperty(VOIStatisticList.median + "Red", nf.format(medianR));
                    statProperty.setProperty(VOIStatisticList.median + "Green", nf.format(medianG));
                    statProperty.setProperty(VOIStatisticList.median + "Blue", nf.format(medianB));
                    statProperty.setProperty(VOIStatisticList.mode + "Red", nf.format(modeR));
                    statProperty.setProperty(VOIStatisticList.mode + "Green", nf.format(modeG));
                    statProperty.setProperty(VOIStatisticList.mode + "Blue", nf.format(modeB));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Red", nf.format(maxCountR));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Green", nf.format(maxCountG));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Blue", nf.format(maxCountB));
                } else {

                    for (int i = 0; i < length; i++) {

                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            sum += imgBuffer[i];
                            nVox++;

                            if (imgBuffer[i] < minIntensity) {
                                minIntensity = imgBuffer[i];
                            }

                            if (imgBuffer[i] > maxIntensity) {
                                maxIntensity = imgBuffer[i];
                            }
                        }
                    }



                    //second pass...
                    float[] buff = new float[nVox];
                    int k = 0;
                    for (int i = 0; i < length; i++) {
                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            buff[k] = imgBuffer[i];
                            k++; 
                        }
                    }
                    Arrays.sort(buff);
                    float[] sorted = buff;
                    if(totalBuff == null) {
                        totalBuff = sorted;
                    }else {
                        float[] temp = totalBuff;
                        int tempLength = temp.length;
                        int sortedLength =  sorted.length;
                        int newLength = tempLength + sortedLength;
                        totalBuff = new float[newLength];
                        int counter = 0;
                        for(int i=0;i<temp.length;i++) {
                            totalBuff[counter] = temp[i];
                            counter++;
                        }
                        for(int i=counter;i<sorted.length;i++) {
                            totalBuff[counter] = sorted[i];
                            counter++;
                        }
                    }
                    int cnt = sorted.length;
                    double median;
                    float temp = 0;
                    int count = 0;
                    float mode = 0;
                    float maxCount = 0;
                    for(int i=0;i<sorted.length;i++) {
                        if(i==0) {
                            temp = sorted[i];
                            count = 1;
                            mode = temp;
                            maxCount = 1;
                        }else {
                            if(sorted[i] == temp) {
                                count++;
                                if(count > maxCount) {
                                    maxCount = count;
                                    mode = temp;
                                }

                            }else {
                                temp = sorted[i];
                                count = 1;
                            }
                        } 
                    }
                    if (cnt%2 == 1) {
                        median = sorted[cnt/2] ;   
                    }
                    else {
                        median = (sorted[cnt/2] + sorted[(cnt/2) - 1])/2.0;
                    }



                    avgInten = sum / nVox;

                    statProperty.setProperty(VOIStatisticList.minIntensity + "0;", nf.format(minIntensity));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "0;", nf.format(maxIntensity));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "0;", nf.format(avgInten));
                    statProperty.setProperty(VOIStatisticList.quantityDescription + "0;", nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "0;", nf.format(sum));
                    statProperty.setProperty(VOIStatisticList.median + "0;", nf.format(median));
                    statProperty.setProperty(VOIStatisticList.mode + "0;", nf.format(mode));
                    statProperty.setProperty(VOIStatisticList.modeCount + "0;", nf.format(maxCount));

                    statProperty.setProperty(VOIStatisticList.minIntensity, nf.format(minIntensity));
                    statProperty.setProperty(VOIStatisticList.maxIntensity, nf.format(maxIntensity));
                    statProperty.setProperty(VOIStatisticList.avgIntensity, nf.format(avgInten));
                    statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities, nf.format(sum));
                    statProperty.setProperty(VOIStatisticList.median, nf.format(median));
                    statProperty.setProperty(VOIStatisticList.mode, nf.format(mode));
                    statProperty.setProperty(VOIStatisticList.modeCount, nf.format(maxCount));
                }

                area = nVox * (fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1]);
                statProperty.setProperty(VOIStatisticList.areaDescription + "0;", nf.format(area));
                statProperty.setProperty(VOIStatisticList.volumeDescription + "0;", nf.format(area));

                statProperty.setProperty(VOIStatisticList.areaDescription, nf.format(area));
                statProperty.setProperty(VOIStatisticList.volumeDescription, nf.format(area));

                // calculate standard deviation, coefficient of skewness, and coefficient of kurtosis
                sum2 = sumR2 = sumG2 = sumB2 = sum3 = sumR3 = sumG3 = sumB3 = sum4 = sumR4 = sumG4 = sumB4 = 0;
                // Calculate center of mass
                xMass = yMass = 0;
                xMassR = yMassR = xMassG = yMassG = xMassB = yMassB = 0;

                int cnt = 0;

                if (srcImage.isColorImage()) {

                    for (int i = 0; i < length; i += 4) {

                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            x = (i/4) % srcImage.getExtents()[0];
                            y = (i/4) / srcImage.getExtents()[0];
                            xMassR += x * imgBuffer[i + 1];
                            yMassR += y * imgBuffer[i + 1];
                            diffR = imgBuffer[i + 1] - avgIntenR;
                            R2 = diffR * diffR;
                            sumR2 += R2;
                            R3 = R2 * diffR;
                            sumR3 += R3;
                            R4 = R3 * diffR;
                            sumR4 += R4;
                            xMassG += x * imgBuffer[i + 2];
                            yMassG += y * imgBuffer[i + 2];
                            diffG = imgBuffer[i + 2] - avgIntenG;
                            G2 = diffG * diffG;
                            sumG2 += G2;
                            G3 = G2 * diffG;
                            sumG3 += G3;
                            G4 = G3 * diffG;
                            sumG4 += G4;
                            xMassB += x * imgBuffer[i + 3];
                            yMassB += y * imgBuffer[i + 3];
                            diffB = imgBuffer[i + 3] - avgIntenB;
                            B2 = diffB * diffB;
                            sumB2 += B2;
                            B3 = B2 * diffB;
                            sumB3 += B3;
                            B4 = B3 * diffB;
                            sumB4 += B4;
                            cnt++;
                        }
                    }

                    stdDevR = (float) Math.sqrt(sumR2 / (cnt-1));
                    stdDevG = (float) Math.sqrt(sumG2 / (cnt-1));
                    stdDevB = (float) Math.sqrt(sumB2 / (cnt-1));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Red" + "0;", nf.format(stdDevR));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Green" + "0;", nf.format(stdDevG));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue" + "0;", nf.format(stdDevB));

                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Red", nf.format(stdDevR));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Green", nf.format(stdDevG));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue", nf.format(stdDevB));
                    // moments around the mean
                    moment2R = sumR2/cnt;
                    moment2G = sumG2/cnt;
                    moment2B = sumB2/cnt;
                    moment3R = sumR3/cnt;
                    moment3G = sumG3/cnt;
                    moment3B = sumB3/cnt;
                    moment4R = sumR4/cnt;
                    moment4G = sumG4/cnt;
                    moment4B = sumB4/cnt;
                    skewnessR = (float)(moment3R/Math.pow(moment2R, 1.5));
                    skewnessG = (float)(moment3G/Math.pow(moment2G, 1.5));
                    skewnessB = (float)(moment3B/Math.pow(moment2B, 1.5));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red" + "0;", nf.format(skewnessR));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green" + "0;", nf.format(skewnessG));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue" + "0;", nf.format(skewnessB));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red", nf.format(skewnessR));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green", nf.format(skewnessG));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue", nf.format(skewnessB));
                    kurtosisR = moment4R/(moment2R * moment2R);
                    kurtosisG = moment4G/(moment2G * moment2G);
                    kurtosisB = moment4B/(moment2B * moment2B);
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red" + "0;", nf.format(kurtosisR));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green" + "0;", nf.format(kurtosisG));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue" + "0;", nf.format(kurtosisB));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red", nf.format(kurtosisR));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green", nf.format(kurtosisG));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue", nf.format(kurtosisB));
                    // Centers of mass
                    xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/sumR;
                    yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/sumR;

                    comStr = unit2DStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red" + "0;", comStr);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red", comStr);

                    xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/sumG;
                    yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/sumG;
                    comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green" + "0;", comStr);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green", comStr);

                    xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/sumB;
                    yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/sumB;

                    comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue" + "0;", comStr);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue", comStr);
                } else {

                    for (int i = 0; i < length; i++) {

                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            x = i % srcImage.getExtents()[0];
                            y = i / srcImage.getExtents()[0];
                            xMass += x * imgBuffer[i];
                            yMass += y * imgBuffer[i];
                            diff = imgBuffer[i] - avgInten;
                            s2 = diff * diff;
                            sum2 += s2;
                            s3 = s2 * diff;
                            sum3 += s3;
                            s4 = s3 * diff;
                            sum4 += s4;
                            cnt++;
                        }
                    }

                    stdDev = (float) Math.sqrt(sum2 / (cnt-1));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "0;", nf.format(stdDev));
                    statProperty.setProperty(VOIStatisticList.deviationDescription, nf.format(stdDev));
                    moment2 = sum2/cnt;
                    moment3 = sum3/cnt;
                    moment4 = sum4/cnt;
                    skewness = (float)(moment3/Math.pow(moment2, 1.5));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "0;", nf.format(skewness));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription , nf.format(skewness));
                    kurtosis = moment4/(moment2 * moment2);
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "0;", nf.format(kurtosis));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription, nf.format(kurtosis));
                    // Center of mass
                    xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/sum;
                    yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/sum;
                    comStr = unit2DStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "0;", comStr);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription, comStr);
                }


            }
        }
    }

    private class Calc34D implements Runnable {

        /** The VOI being used just for this calculation (may be a single curve of a single slice or entire VOI **/
        private VOI calcSelectedVOI;

        public Calc34D(VOI selectedVOI) {
            this.calcSelectedVOI = selectedVOI;
        }

        public void run() {
            long time = System.currentTimeMillis();

            double minIntensity = Double.MAX_VALUE;
            double maxIntensity = -Double.MAX_VALUE;
            double minIntenRed = Double.MAX_VALUE;
            double maxIntenRed = -Double.MAX_VALUE;
            double minIntenGreen = Double.MAX_VALUE;
            double maxIntenGreen = -Double.MAX_VALUE;
            double minIntenBlue = Double.MAX_VALUE;
            double maxIntenBlue = -Double.MAX_VALUE;
            double avgInten = 0;
            double avgIntenR = 0;
            double avgIntenG = 0;
            double avgIntenB = 0;
            double skewness = 0, skewnessR = 0, skewnessG = 0, skewnessB = 0;
            double kurtosis = 0, kurtosisR = 0, kurtosisG = 0, kurtosisB = 0;
            double R2, R3, R4, G2, G3, G4, B2, B3, B4, s2, s3, s4;
            double diff, diffR, diffG, diffB;
            double stdDev = 0, stdDevR = 0, stdDevG = 0, stdDevB = 0;
            double volume = 0;
            double sum = 0, sumR = 0, sumG = 0, sumB = 0, area = 0;
            double sum2 = 0, sumR2 = 0, sumG2 = 0, sumB2 = 0;
            double sum3 = 0, sumR3 = 0, sumG3 = 0, sumB3 = 0;
            double sum4 = 0, sumR4 = 0, sumG4 = 0, sumB4 = 0;
            double moment2, moment2R, moment2G, moment2B;
            double moment3, moment3R, moment3G, moment3B;
            double moment4, moment4R, moment4G, moment4B;
            int nVox = 0;
            Vector3f totalC = new Vector3f(0, 0, 0);
            float[] imgBuffer;
            float[] tmpPAxis = null;
            float[] tmpEcc = null;
            float[] tmpMajorAxis = null;
            float[] tmpMinorAxis = null;
            float[] xExtents = null;
            float[] yExtents = null;
            float[] zExtents = null;
            int length;
            double totalPerimeter = 0;
            double largestAllSlicesDistance = 0;
            double largestDistance = 0;
            int x;
            int y;
            int z;
            double xMass, yMass, zMass, xMassR, yMassR, zMassR, xMassG, yMassG, zMassG, xMassB, yMassB, zMassB;
            double xCOM, yCOM, zCOM, xCOMR, yCOMR, zCOMR, xCOMG, yCOMG, zCOMG, xCOMB, yCOMB, zCOMB;
            int xUnits, yUnits, zUnits;
            String xStr, yStr, zStr;
            String unit2DStr = null;
            String unit3DStr = null;
            String unitStr = null;
            Vector3f centerPt;

            Vector<VOIBase> contours;
            BitSet mask;
            VOIStatisticalProperties statProperty = getVOIProperties(calcSelectedVOI);

            area = 0;
            volume = 0;
            nVox = 0;

            if (srcImage.isColorImage()) {
                length = 4 * srcImage.getSliceSize();
            } else {
                length = srcImage.getSliceSize();
            }

            FileInfoBase[] fileInfo = srcImage.getFileInfo();

            try {
                imgBuffer = new float[length * srcImage.getExtents()[2]];
                int offset4D = 0;
                if ( ViewUserInterface.getReference().getFrameContainingImage(srcImage) != null )
                {
                    offset4D = imgBuffer.length *
                    ViewUserInterface.getReference().getFrameContainingImage(srcImage).getViewableTimeSlice();
                }
                tmpPAxis = new float[1];
                tmpEcc = new float[1];
                tmpMajorAxis = new float[1];
                tmpMinorAxis = new float[1];
                xExtents = new float[2];
                yExtents = new float[2];
                zExtents = new float[2];

                srcImage.exportData(offset4D, imgBuffer.length, imgBuffer);
                mask = new BitSet(imgBuffer.length);

            } catch (IOException error) {
                displayError("Algorithm VOI Properties: Image(s) locked");
                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                displayError("Algorithm VOI Properties: Out of Memory");
                setCompleted(false);

                return;
            }  

            if (srcImage.getParentFrame() != null) {
                calcSelectedVOI.createBinaryMask3D(mask, srcImage.getExtents()[0], srcImage.getExtents()[1],
                        srcImage.getParentFrame().useXOR(), false);
            }
            else {
                calcSelectedVOI.createBinaryMask3D(mask, srcImage.getExtents()[0], srcImage.getExtents()[1],
                        false, false);    
            }

            calcSelectedVOI.getBounds(xExtents, yExtents, zExtents);

            float ignoreMin = calcSelectedVOI.getMinimumIgnore();
            float ignoreMax = calcSelectedVOI.getMaximumIgnore();

            contours = calcSelectedVOI.getCurves();

            if(distanceFlag) {
                long time2 = System.currentTimeMillis();
                largestDistance = calcSelectedVOI.calcLargestDistance(srcImage.getExtents()[0],
                        srcImage.getExtents()[1],
                        srcImage.getFileInfo(0).getResolutions()[0],
                        srcImage.getFileInfo(0).getResolutions()[1],
                        srcImage.getFileInfo(0).getResolutions()[2]);
                System.out.println("Total time: "+(System.currentTimeMillis() - time2));
            }

            xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            if (xUnits != FileInfoBase.UNKNOWN_MEASURE) {
                xStr = "X " + FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);    
            }
            else {
                xStr = "X ";
            }
            yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            if (yUnits != FileInfoBase.UNKNOWN_MEASURE) {
                yStr = "Y " + FileInfoBase.getUnitsOfMeasureAbbrevStr(yUnits);    
            }
            else {
                yStr = "Y ";
            }
            unit2DStr = xStr + "\t" + yStr;
            if (srcImage.getNDims() > 2) {
                zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
                if (zUnits != FileInfoBase.UNKNOWN_MEASURE) {
                    zStr = "Z " + FileInfoBase.getUnitsOfMeasureAbbrevStr(zUnits);
                    if ((srcImage.getFileInfo(0).getOrigin()[0] != 0) || (srcImage.getFileInfo(0).getOrigin()[1] != 0) ||
                            (srcImage.getFileInfo(0).getOrigin()[2] != 0)) {
                        zStr = zStr + "\tR-L:\tA-P:\tI-S:";
                    }
                }
                else {
                    zStr = "Z ";
                }
                unit3DStr = unit2DStr + "\t" + zStr;
            }

            if ((processType == PROCESS_PER_SLICE) ||
                    (processType == PROCESS_PER_SLICE_AND_CONTOUR) ||
                    (processType == PROCESS_PER_CONTOUR)) {

                if ( processType == PROCESS_PER_CONTOUR )
                {   
                    totalC = calcSelectedVOI.getGeometricCenter();

                    ContourStats[] stats = new ContourStats[contours.size()];
                    // since we're in a 3D image, contours.length is how many slices this VOI is on
                    for (int q = 0; q < contours.size(); q++) {
                        stats[q] = calcStatsPerContour( fileInfo[0], imgBuffer, contours.elementAt(q), 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                        printStatsPerContour( fileInfo[0], stats[q], statProperty, 0, q );
                    }               
                    if ( showTotals )
                    {
                        printTotals( fileInfo[0], stats, statProperty, 
                                unit2DStr, unit3DStr, "Total", mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                    }
                }
                else
                {
                    totalC = calcSelectedVOI.getGeometricCenter();
                    Vector<VOIBase>[] sortedContoursZ = calcSelectedVOI.getSortedCurves( VOIBase.ZPLANE, srcImage.getExtents()[2] );
                    Vector<ContourStats> allStats = new Vector<ContourStats>();
                    for ( int sortedZ = 0; sortedZ < sortedContoursZ.length; sortedZ++ )
                    {
                        if ( sortedContoursZ[sortedZ].size() == 0 )
                        {
                            continue;
                        }
                        ContourStats[] stats = new ContourStats[sortedContoursZ[sortedZ].size()];
                        // since we're in a 3D image, contours.length is how many slices this VOI is on
                        for (int q = 0; q < sortedContoursZ[sortedZ].size(); q++) {
                            stats[q] = calcStatsPerContour( fileInfo[0], imgBuffer, sortedContoursZ[sortedZ].elementAt(q), 
                                    unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                            if ( processType == PROCESS_PER_SLICE_AND_CONTOUR )
                            {
                                printStatsPerContour(  fileInfo[0], stats[q], statProperty, sortedZ, q );
                            }  
                            allStats.add( stats[q] );
                        }         
                        if ( showTotals )
                        {
                            printTotals( fileInfo[0], stats, statProperty, 
                                    unit2DStr, unit3DStr, new String( sortedZ + ";" ), mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                        }
                    }
                    if ( showTotals )
                    {
                        printTotals( fileInfo[0], allStats, statProperty, 
                                unit2DStr, unit3DStr, "Total", mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                    }
                }               
            } else {
                VOIContour thisContour = null;

                for (int i = 0; i < contours.size(); i++) {

                    if (contours.elementAt(i).size() > 0) {
                        thisContour = ((VOIContour) (contours.elementAt(i)));

                        break;
                    }
                }
                /*
                thisContour.secondOrderAttributes(srcImage.getExtents()[0], srcImage.getExtents()[1],
                                                  srcImage.getFileInfo(0).getResolutions()[0],
                                                  srcImage.getFileInfo(0).getResolutions()[1],
                                                  srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                                  srcImage.getFileInfo(0).getUnitsOfMeasure()[1], tmpPAxis, tmpEcc,
                                                  tmpMajorAxis, tmpMinorAxis); */

                thisContour.secondOrderAttributes(srcImage, tmpPAxis, tmpEcc,
                        tmpMajorAxis, tmpMinorAxis);
                statProperty.setProperty(VOIStatisticList.axisDescription, nf.format(tmpPAxis[0]));
                statProperty.setProperty(VOIStatisticList.eccentricityDescription, nf.format(tmpEcc[0]));
                statProperty.setProperty(VOIStatisticList.majorAxisDescription, nf.format(tmpMajorAxis[0]));
                statProperty.setProperty(VOIStatisticList.minorAxisDescription, nf.format(tmpMinorAxis[0]));

                Vector3f selectedCOM = calcSelectedVOI.getGeometricCenter();
                selectedCOM.X *= srcImage.getFileInfo(0).getResolutions()[0];
                selectedCOM.Y *= srcImage.getFileInfo(0).getResolutions()[1];
                unitStr = unit2DStr + "\tZ";

                if (srcImage.getNDims() > 2) {
                    selectedCOM.Z *= srcImage.getFileInfo(0).getResolutions()[2];
                    unitStr = unit3DStr;
                }


                String comStr = unitStr + "\n\t\t" + nf.format(selectedCOM.X) + "\t" + nf.format(selectedCOM.Y) + "\t" +
                nf.format(selectedCOM.Z);
                comStr = addScannerLabels(comStr, selectedCOM);

                statProperty.setProperty(VOIStatisticList.geometricCenterDescription, comStr);



                if (srcImage.isColorImage()) {

                    for (int i = 0; i < imgBuffer.length; i += 4) {

                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            sumR += imgBuffer[i + 1];
                            sumG += imgBuffer[i + 2];
                            sumB += imgBuffer[i + 3];
                            nVox++;

                            if (imgBuffer[i + 1] < minIntenRed) {
                                minIntenRed = imgBuffer[i + 1];
                            }

                            if (imgBuffer[i + 1] > maxIntenRed) {
                                maxIntenRed = imgBuffer[i + 1];
                            }

                            if (imgBuffer[i + 2] < minIntenGreen) {
                                minIntenGreen = imgBuffer[i + 2];
                            }

                            if (imgBuffer[i + 2] > maxIntenGreen) {
                                maxIntenGreen = imgBuffer[i + 2];
                            }

                            if (imgBuffer[i + 3] < minIntenBlue) {
                                minIntenBlue = imgBuffer[i + 3];
                            }

                            if (imgBuffer[i + 3] > maxIntenBlue) {
                                maxIntenBlue = imgBuffer[i + 3];
                            }
                        }
                    }



                    //second pass...
                    float[] buffR = new float[nVox];
                    float[] buffG = new float[nVox];
                    float[] buffB = new float[nVox];
                    int k = 0;
                    for (int i = 0; i < imgBuffer.length; i += 4) {
                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            buffR[k] = imgBuffer[i + 1];
                            buffG[k] = imgBuffer[i + 2];
                            buffB[k] = imgBuffer[i + 3];
                            k++; 
                        }
                    }

                    //red
                    Arrays.sort(buffR);
                    float[] sortedR = buffR;
                    int cntR = sortedR.length;
                    double medianR;
                    float tempR = 0;
                    int countR = 0;
                    float modeR = 0;
                    float maxCountR = 0;
                    for(int i=0;i<sortedR.length;i++) {
                        if(i==0) {
                            tempR = sortedR[i];
                            countR = 1;
                            modeR = tempR;
                            maxCountR = 1;
                        }else {
                            if(sortedR[i] == tempR) {
                                countR++;
                                if(countR > maxCountR) {
                                    maxCountR = countR;
                                    modeR = tempR;
                                }

                            }else {
                                tempR = sortedR[i];
                                countR = 1;
                            }
                        } 
                    }
                    if (cntR%2 == 1) {
                        medianR = sortedR[cntR/2] ;   
                    }
                    else {
                        medianR = (sortedR[cntR/2] + sortedR[(cntR/2) - 1])/2.0;
                    }

                    //green
                    Arrays.sort(buffG);
                    float[] sortedG = buffG;
                    int cntG = sortedG.length;
                    double medianG;
                    float tempG = 0;
                    int countG = 0;
                    float modeG = 0;
                    float maxCountG = 0;
                    for(int i=0;i<sortedG.length;i++) {
                        if(i==0) {
                            tempG = sortedG[i];
                            countG = 1;
                            modeG = tempG;
                            maxCountG = 1;
                        }else {
                            if(sortedG[i] == tempG) {
                                countG++;
                                if(countG > maxCountG) {
                                    maxCountG = countG;
                                    modeG = tempG;
                                }

                            }else {
                                tempG = sortedG[i];
                                countG = 1;
                            }
                        } 
                    }
                    if (cntG%2 == 1) {
                        medianG = sortedG[cntG/2] ;   
                    }
                    else {
                        medianG = (sortedG[cntG/2] + sortedG[(cntG/2) - 1])/2.0;
                    }


                    //blue
                    Arrays.sort(buffB);
                    float[] sortedB = buffB;
                    int cntB = sortedB.length;
                    double medianB;
                    float tempB = 0;
                    int countB = 0;
                    float modeB = 0;
                    float maxCountB = 0;
                    for(int i=0;i<sortedB.length;i++) {
                        if(i==0) {
                            tempB = sortedB[i];
                            countB = 1;
                            modeB = tempB;
                            maxCountB = 1;
                        }else {
                            if(sortedB[i] == tempB) {
                                countB++;
                                if(countB > maxCountB) {
                                    maxCountB = countB;
                                    modeB = tempB;
                                }

                            }else {
                                tempB = sortedB[i];
                                countB = 1;
                            }
                        } 
                    }
                    if (cntB%2 == 1) {
                        medianB = sortedB[cntB/2] ;   
                    }
                    else {
                        medianB = (sortedB[cntB/2] + sortedB[(cntB/2) - 1])/2.0;
                    }

                    avgIntenR = sumR / nVox;
                    avgIntenG = sumG / nVox;
                    avgIntenB = sumB / nVox;

                    statProperty.setProperty(VOIStatisticList.minIntensity + "Red", nf.format(minIntenRed));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Red", nf.format(maxIntenRed));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Green", nf.format(minIntenGreen));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Green", nf.format(maxIntenGreen));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Blue", nf.format(minIntenBlue));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue", nf.format(maxIntenBlue));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Red", nf.format(avgIntenR));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Green", nf.format(avgIntenG));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue", nf.format(avgIntenB));  
                    statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Red", nf.format(sumR));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Green", nf.format(sumG));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue", nf.format(sumB));
                    statProperty.setProperty(VOIStatisticList.median + "Red", nf.format(medianR));
                    statProperty.setProperty(VOIStatisticList.median + "Green", nf.format(medianG));
                    statProperty.setProperty(VOIStatisticList.median + "Blue", nf.format(medianB));

                    statProperty.setProperty(VOIStatisticList.mode + "Red", nf.format(modeR));
                    statProperty.setProperty(VOIStatisticList.mode + "Green", nf.format(modeG));
                    statProperty.setProperty(VOIStatisticList.mode + "Blue", nf.format(modeB));

                    statProperty.setProperty(VOIStatisticList.modeCount + "Red", nf.format(maxCountR));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Green", nf.format(maxCountG));
                    statProperty.setProperty(VOIStatisticList.modeCount + "Blue", nf.format(maxCountB));
                } else {

                    for (int i = 0; i < imgBuffer.length; i++) {

                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            sum += imgBuffer[i];
                            nVox++;

                            if (imgBuffer[i] < minIntensity) {
                                minIntensity = imgBuffer[i];
                            }

                            if (imgBuffer[i] > maxIntensity) {
                                maxIntensity = imgBuffer[i];
                            }
                        }
                    }




                    //second pass...
                    float[] buff = new float[nVox];
                    int k = 0;
                    for (int i = 0; i < imgBuffer.length; i++) {
                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            buff[k] = imgBuffer[i];
                            k++; 
                        }
                    }
                    Arrays.sort(buff);
                    float[] sorted = buff;
                    int cnt = sorted.length;
                    double median;
                    float temp = 0;
                    int count = 0;
                    float mode = 0;
                    float maxCount = 0;
                    for(int i=0;i<sorted.length;i++) {
                        if(i==0) {
                            temp = sorted[i];
                            count = 1;
                            mode = temp;
                            maxCount = 1;
                        }else {
                            if(sorted[i] == temp) {
                                count++;
                                if(count > maxCount) {
                                    maxCount = count;
                                    mode = temp;
                                }

                            }else {
                                temp = sorted[i];
                                count = 1;
                            }
                        } 
                    }
                    if (cnt%2 == 1) {
                        median = sorted[cnt/2] ;   
                    }
                    else {
                        median = (sorted[cnt/2] + sorted[(cnt/2) - 1])/2.0;
                    }

                    avgInten = sum / nVox;

                    statProperty.setProperty(VOIStatisticList.minIntensity, nf.format(minIntensity));
                    statProperty.setProperty(VOIStatisticList.maxIntensity, nf.format(maxIntensity));
                    statProperty.setProperty(VOIStatisticList.avgIntensity, nf.format(avgInten));
                    statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(nVox));
                    statProperty.setProperty(VOIStatisticList.sumIntensities, nf.format(sum));
                    statProperty.setProperty(VOIStatisticList.median, nf.format(median));
                    statProperty.setProperty(VOIStatisticList.mode, nf.format(mode));
                    statProperty.setProperty(VOIStatisticList.modeCount, nf.format(maxCount));
                }

                // calc the perimeter
                totalPerimeter = 0f;
                largestAllSlicesDistance = 0;

                //for (int q = 0; q < contours.length; q++) {

                // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                for (int r = 0; r < contours.size(); r++) {
                    totalPerimeter += ((VOIContour) (contours.elementAt(r))).getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());
                    if(sliceDistanceFlag) {
                        largestAllSlicesDistance = Math.max(largestAllSlicesDistance, 
                                ((VOIContour)(contours.elementAt(r))).calcLargestSliceDistance(
                                        srcImage.getFileInfo(0).getResolutions()[0], srcImage.getFileInfo(0).getResolutions()[1]));
                    }
                }
                // }

                statProperty.setProperty(VOIStatisticList.perimeterDescription, nf.format(totalPerimeter));
                statProperty.setProperty(VOIStatisticList.perimeterDescription + "0;", nf.format(totalPerimeter));
                statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription, nf.format(largestAllSlicesDistance));
                statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + "0;", nf.format(largestAllSlicesDistance));
                statProperty.setProperty(VOIStatisticList.largestDistanceDescription, nf.format(largestDistance));
                statProperty.setProperty(VOIStatisticList.largestDistanceDescription + "0;", nf.format(largestDistance));


                area = nVox *
                (fileInfo[fileInfo.length / 2].getResolutions()[0] *
                        fileInfo[fileInfo.length / 2].getResolutions()[1]);
                volume = area * fileInfo[fileInfo.length / 2].getResolutions()[2];
                statProperty.setProperty(VOIStatisticList.areaDescription, nf.format(area));
                statProperty.setProperty(VOIStatisticList.volumeDescription, nf.format(volume));

                // calculate standard deviation, coefficient of skewness, and coefficient of kurtosis
                sum2 = sumR2 = sumG2 = sumB2 = sum3 = sumR3 = sumG3 = sumB3 = sum4 = sumR4 = sumG4 = sumB4 = 0;
                // Calculate centers of mass
                xMass = yMass = zMass = 0;
                xMassR = yMassR = zMassR = xMassG = yMassG = zMassG = xMassB = yMassB = zMassB = 0;

                int cnt = 0;

                if (srcImage.isColorImage()) {

                    for (int i = 0; i < imgBuffer.length; i += 4) {

                        if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                            x = (i/4) % srcImage.getExtents()[0];
                            y = ((i/4) % srcImage.getSliceSize())/ srcImage.getExtents()[0];
                            z = (i/4)/srcImage.getSliceSize();
                            xMassR += x * imgBuffer[i + 1];
                            yMassR += y * imgBuffer[i + 1];
                            zMassR += z * imgBuffer[i + 1];
                            diffR = imgBuffer[i + 1] - avgIntenR;
                            R2 = diffR * diffR;
                            sumR2 += R2;
                            R3 = R2 * diffR;
                            sumR3 += R3;
                            R4 = R3 * diffR;
                            sumR4 += R4;
                            xMassG += x * imgBuffer[i + 2];
                            yMassG += y * imgBuffer[i + 2];
                            zMassG += z * imgBuffer[i + 2];
                            diffG = imgBuffer[i + 2] - avgIntenG;
                            G2 = diffG * diffG;
                            sumG2 += G2;
                            G3 = G2 * diffG;
                            sumG3 += G3;
                            G4 = G3 * diffG;
                            sumG4 += G4;
                            xMassB += x * imgBuffer[i + 3];
                            yMassB += y * imgBuffer[i + 3];
                            zMassB += z * imgBuffer[i + 3];
                            diffB = imgBuffer[i + 3] - avgIntenB;
                            B2 = diffB * diffB;
                            sumB2 += B2;
                            B3 = B2 * diffB;
                            sumB3 += B3;
                            B4 = B3 * diffB;
                            sumB4 += B4;
                            cnt++;
                        }
                    }

                    stdDevR = (float) Math.sqrt(sumR2 / (cnt-1));
                    stdDevG = (float) Math.sqrt(sumG2 / (cnt-1));
                    stdDevB = (float) Math.sqrt(sumB2 / (cnt-1));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Red", nf.format(stdDevR));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Green", nf.format(stdDevG));
                    statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue", nf.format(stdDevB));
                    // moments around the mean
                    moment2R = sumR2/cnt;
                    moment2G = sumG2/cnt;
                    moment2B = sumB2/cnt;
                    moment3R = sumR3/cnt;
                    moment3G = sumG3/cnt;
                    moment3B = sumB3/cnt;
                    moment4R = sumR4/cnt;
                    moment4G = sumG4/cnt;
                    moment4B = sumB4/cnt;
                    skewnessR = (float)(moment3R/Math.pow(moment2R, 1.5));
                    skewnessG = (float)(moment3G/Math.pow(moment2G, 1.5));
                    skewnessB = (float)(moment3B/Math.pow(moment2B, 1.5));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red", nf.format(skewnessR));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green", nf.format(skewnessG));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue", nf.format(skewnessB));
                    kurtosisR = moment4R/(moment2R * moment2R);
                    kurtosisG = moment4G/(moment2G * moment2G);
                    kurtosisB = moment4B/(moment2B * moment2B);
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red", nf.format(kurtosisR));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green", nf.format(kurtosisG));
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue", nf.format(kurtosisB));
                    // Centers of mass
                    xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/sumR;
                    yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/sumR;
                    zCOMR = zMassR/sumR;
                    unitStr = unit2DStr + "\tZ";
                    if (srcImage.getNDims() > 2) {
                        zCOMR *= srcImage.getFileInfo(0).getResolutions()[2];
                        unitStr = unit3DStr;
                    }

                    comStr = unitStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" +
                    nf.format(zCOMR);
                    centerPt = new Vector3f();
                    centerPt.X = (float)xCOMR;
                    centerPt.Y = (float)yCOMR;
                    centerPt.Z = (float)zCOMR;
                    comStr = addScannerLabels(comStr, centerPt);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red", comStr);

                    xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/sumG;
                    yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/sumG;
                    zCOMG= zMassG/sumG;
                    if (srcImage.getNDims() > 2) {
                        zCOMG *= srcImage.getFileInfo(0).getResolutions()[2];
                    }

                    comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" +
                    nf.format(zCOMG);
                    centerPt = new Vector3f();
                    centerPt.X = (float)xCOMG;
                    centerPt.Y = (float)yCOMG;
                    centerPt.Z = (float)zCOMG;
                    comStr = addScannerLabels(comStr, centerPt);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green", comStr);

                    xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/sumB;
                    yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/sumB;
                    zCOMB = zMassB/sumB;
                    if (srcImage.getNDims() > 2) {
                        zCOMB *= srcImage.getFileInfo(0).getResolutions()[2];
                    }

                    comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" +
                    nf.format(zCOMB);
                    centerPt = new Vector3f();
                    centerPt.X = (float)xCOMB;
                    centerPt.Y = (float)yCOMB;
                    centerPt.Z = (float)zCOMB;
                    comStr = addScannerLabels(comStr, centerPt);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue", comStr);

                } else {

                    for (int i = 0; i < imgBuffer.length; i++) {

                        if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                            x = i % srcImage.getExtents()[0];
                            y = (i % srcImage.getSliceSize())/ srcImage.getExtents()[0];
                            z = i / srcImage.getSliceSize();
                            xMass += x * imgBuffer[i];
                            yMass += y * imgBuffer[i];
                            zMass += z * imgBuffer[i];
                            diff = imgBuffer[i] - avgInten;
                            s2 = diff * diff;
                            sum2 += s2;
                            s3 = s2 * diff;
                            sum3 += s3;
                            s4 = s3 * diff;
                            sum4 += s4;
                            cnt++;
                        }
                    }

                    stdDev = (float) Math.sqrt(sum2 / (cnt-1));
                    statProperty.setProperty(VOIStatisticList.deviationDescription, nf.format(stdDev));
                    moment2 = sum2/cnt;
                    moment3 = sum3/cnt;
                    moment4 = sum4/cnt;
                    skewness = (float)(moment3/Math.pow(moment2, 1.5));
                    statProperty.setProperty(VOIStatisticList.skewnessDescription, nf.format(skewness));
                    kurtosis = moment4/(moment2 * moment2);
                    statProperty.setProperty(VOIStatisticList.kurtosisDescription, nf.format(kurtosis));
                    // Center of mass
                    xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/sum;
                    yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/sum;
                    zCOM = zMass/sum;
                    unitStr = unit2DStr + "\tZ";
                    if (srcImage.getNDims() > 2) {
                        zCOM *= srcImage.getFileInfo(0).getResolutions()[2];
                        unitStr = unit3DStr;
                    }

                    comStr = unitStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" +
                    nf.format(zCOM);
                    centerPt = new Vector3f();
                    centerPt.X = (float)xCOM;
                    centerPt.Y = (float)yCOM;
                    centerPt.Z = (float)zCOM;
                    comStr = addScannerLabels(comStr, centerPt);
                    statProperty.setProperty(VOIStatisticList.massCenterDescription, comStr);
                }
            }

            System.out.println("Time required to calculate "+calcSelectedVOI.getName()+": "+(System.currentTimeMillis() - time));
        }


        private class ContourStats
        {
            public ContourStats() {}
            public double area;
            public double volume;
            public double perimeter;
            public double largestContourDistance;

            public double[] xMass, yMass, zMass;
            public double[] massI;

            Vector<Float> values;
            Vector<ColorRGB> valuesRGB;            

            public double PAxis, Ecc, MajorAxis, MinorAxis;
            public Vector3f gCenter;
            public String gCenterString;
            public String massCenterDescriptionR;
            public String massCenterDescriptionG;
            public String massCenterDescriptionB;
            public String massCenterDescription;
            public Vector3f[] pts;

            public double minIntenRed;
            public double maxIntenRed;
            public double minIntenGreen;
            public double maxIntenGreen;
            public double minIntenBlue;
            public double maxIntenBlue;
            public double avgIntenR;
            public double avgIntenG;
            public double avgIntenB;
            public double nVox;
            public double sumR;
            public double sumG;
            public double sumB;
            public double medianR;
            public double medianG;
            public double medianB;
            public double modeR;
            public double modeG;
            public double modeB;
            public double maxCountR;
            public double maxCountG;
            public double maxCountB;

            public double minIntensity;
            public double maxIntensity;
            public double avgInten;
            public double sum;
            public double median;
            public double mode;
            public double maxCount;
            public double stdDevR;
            public double stdDevG;
            public double stdDevB;
            public double skewnessR;
            public double skewnessG;
            public double skewnessB;
            public double kurtosisR;
            public double kurtosisG;
            public double kurtosisB;
            public double stdDev;
            public double skewness;
            public double kurtosis;
        }

        private ContourStats calcStatsPerContour( FileInfoBase fileInfo, float[] imgBuffer, VOIBase contour, 
                String unit2DStr, String unit3DStr, float ignoreMin, float ignoreMax )
        {


            ContourStats stats = new ContourStats();
            int stop = 1;

            float[] tmpPAxis = new float[1];
            float[] tmpEcc = new float[1];
            float[] tmpMajorAxis = new float[1];
            float[] tmpMinorAxis = new float[1];

            ((VOIContour)contour).secondOrderAttributes(srcImage,
                    tmpPAxis, tmpEcc, tmpMajorAxis,
                    tmpMinorAxis);
            stats.PAxis = tmpPAxis[0];
            stats.Ecc = tmpEcc[0];
            stats.MajorAxis = tmpMajorAxis[0];
            stats.MinorAxis = tmpMinorAxis[0];

            Vector3f gCenter = contour.getGeometricCenter();
            gCenter.X *= srcImage.getFileInfo(0).getResolutions()[0];
            gCenter.Y *= srcImage.getFileInfo(0).getResolutions()[1];
            String unitStr = unit2DStr + "\tZ";

            if (srcImage.getNDims() > 2) {
                gCenter.Z *= srcImage.getFileInfo(0).getResolutions()[2];
                unitStr = unit3DStr;
            }

            String comStr = unitStr + "\n\t\t" + nf.format(gCenter.X) + "\t" + nf.format(gCenter.Y) + "\t" +  nf.format(gCenter.Z);
            comStr = addScannerLabels(comStr, gCenter);
            stats.gCenter = new Vector3f(gCenter);
            stats.gCenterString = new String(comStr);


            double perimeter = contour.getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());

            double largestContourDistance = 0;
            if(sliceDistanceFlag) {
                largestContourDistance = ((VOIContour) (contour)).calcLargestSliceDistance(
                        srcImage.getFileInfo(0).getResolutions()[0], srcImage.getFileInfo(0).getResolutions()[1]);
            }

            contour.getGroup().setAllActive(false);
            contour.setActive(true);

            Vector3f[] pts = calcSelectedVOI.maxWidth(true);
            stats.pts = pts;

            int nVox = 0;
            double sumR = 0;
            double sumG = 0;
            double sumB = 0;
            double avgIntenR = 0;
            double avgIntenG = 0;
            double avgIntenB = 0;
            double sum = 0;
            double avgInten = 0;
            if (srcImage.isColorImage()) {
                double minIntenRed = Double.MAX_VALUE;
                double maxIntenRed = -Double.MAX_VALUE;
                double minIntenGreen = Double.MAX_VALUE;
                double maxIntenGreen = -Double.MAX_VALUE;
                double minIntenBlue = Double.MAX_VALUE;
                double maxIntenBlue = -Double.MAX_VALUE;

                Vector<ColorRGB> values = contour.calcRGBIntensity( srcImage, ignoreMin, ignoreMax, rangeFlag );
                nVox = values.size();
                float[] buffR = new float[nVox];
                float[] buffG = new float[nVox];
                float[] buffB = new float[nVox];
                for ( int i = 0; i < values.size(); i++ )
                {
                    sumR += values.elementAt(i).R;
                    sumG += values.elementAt(i).G;
                    sumB += values.elementAt(i).B;
                    buffR[i] = values.elementAt(i).R;
                    buffG[i] = values.elementAt(i).G;
                    buffB[i] = values.elementAt(i).B;
                }

                stats.valuesRGB = values;

                //red
                Arrays.sort(buffR);
                float[] sortedR = buffR;
                minIntenRed = sortedR[0];
                maxIntenRed = sortedR[sortedR.length-1];

                int cntR = sortedR.length;
                double medianR;
                float tempR = 0;
                int countR = 0;
                float modeR = 0;
                float maxCountR = 0;
                for(int i=0;i<sortedR.length;i++) {
                    if(i==0) {
                        tempR = sortedR[i];
                        countR = 1;
                        modeR = tempR;
                        maxCountR = 1;
                    }else {
                        if(sortedR[i] == tempR) {
                            countR++;
                            if(countR > maxCountR) {
                                maxCountR = countR;
                                modeR = tempR;
                            }

                        }else {
                            tempR = sortedR[i];
                            countR = 1;
                        }
                    } 
                }
                if (cntR%2 == 1) {
                    medianR = sortedR[cntR/2] ;   
                }
                else {
                    medianR = (sortedR[cntR/2] + sortedR[(cntR/2) - 1])/2.0;
                }


                //green
                Arrays.sort(buffG);
                float[] sortedG = buffG;
                minIntenGreen = sortedG[0];
                maxIntenGreen = sortedG[sortedG.length-1];
                int cntG = sortedG.length;
                double medianG;
                float tempG = 0;
                int countG = 0;
                float modeG = 0;
                float maxCountG = 0;
                for(int i=0;i<sortedG.length;i++) {
                    if(i==0) {
                        tempG = sortedG[i];
                        countG = 1;
                        modeG = tempG;
                        maxCountG = 1;
                    }else {
                        if(sortedG[i] == tempG) {
                            countG++;
                            if(countG > maxCountG) {
                                maxCountG = countG;
                                modeG = tempG;
                            }

                        }else {
                            tempG = sortedG[i];
                            countG = 1;
                        }
                    } 
                }
                if (cntG%2 == 1) {
                    medianG = sortedG[cntG/2] ;   
                }
                else {
                    medianG = (sortedG[cntG/2] + sortedG[(cntG/2) - 1])/2.0;
                }


                //blue
                Arrays.sort(buffB);
                float[] sortedB = buffB;
                minIntenBlue = sortedB[0];
                maxIntenBlue = sortedB[sortedB.length-1];
                int cntB = sortedB.length;
                double medianB;
                float tempB = 0;
                int countB = 0;
                float modeB = 0;
                float maxCountB = 0;
                for(int i=0;i<sortedB.length;i++) {
                    if(i==0) {
                        tempB = sortedB[i];
                        countB = 1;
                        modeB = tempB;
                        maxCountB = 1;
                    }else {
                        if(sortedB[i] == tempB) {
                            countB++;
                            if(countB > maxCountB) {
                                maxCountB = countB;
                                modeB = tempB;
                            }

                        }else {
                            tempB = sortedB[i];
                            countB = 1;
                        }
                    } 
                }
                if (cntB%2 == 1) {
                    medianB = sortedB[cntB/2] ;   
                }
                else {
                    medianB = (sortedB[cntB/2] + sortedB[(cntB/2) - 1])/2.0;
                }

                avgIntenR = sumR / nVox;
                avgIntenG = sumG / nVox;
                avgIntenB = sumB / nVox;
                
                stats.minIntenRed = minIntenRed;
                stats.maxIntenRed = maxIntenRed;
                stats.maxIntenRed = maxIntenRed;
                stats.minIntenGreen = minIntenGreen;
                stats.maxIntenGreen = maxIntenGreen;
                stats.minIntenBlue = minIntenBlue;
                stats.maxIntenBlue = maxIntenBlue;
                stats.avgIntenR = avgIntenR;
                stats.avgIntenG = avgIntenG;
                stats.avgIntenB = avgIntenB;
                stats.nVox = nVox;
                stats.sumR = sumR;
                stats.sumG = sumG;
                stats.sumB = sumB;

                stats.medianR = medianR;
                stats.medianG = medianG;
                stats.medianB = medianB;

                stats.modeR = modeR;
                stats.modeG = modeG;
                stats.modeB = modeB;

                stats.maxCountR = maxCountR;
                stats.maxCountG = maxCountG;
                stats.maxCountB = maxCountB;
            } 
            else /* not color image */ 
            {
                double minIntensity = Double.MAX_VALUE, totalMinIntensity = Double.MAX_VALUE;
                double maxIntensity = -Double.MAX_VALUE, totalMaxIntensity = -Double.MAX_VALUE;

                Vector<Float> values = contour.calcIntensity( srcImage, ignoreMin, ignoreMax, rangeFlag );
                nVox = values.size();
                float[] buff = new float[nVox];
                for ( int i = 0; i < values.size(); i++ )
                {
                    sum += values.elementAt(i).floatValue();
                    buff[i] = values.elementAt(i).floatValue();
                }
                stats.values = values;

                Arrays.sort(buff);
                float[] sorted = buff;
                minIntensity = sorted[0];
                maxIntensity = sorted[sorted.length-1];
                int cnt = sorted.length;
                double median;
                float temp = 0;
                int count = 0;
                float mode = 0;
                float maxCount = 0;
                for(int i=0;i<sorted.length;i++) {
                    if(i==0) {
                        temp = sorted[i];
                        count = 1;
                        mode = temp;
                        maxCount = 1;
                    }else {
                        if(sorted[i] == temp) {
                            count++;
                            if(count > maxCount) {
                                maxCount = count;
                                mode = temp;
                            }

                        }else {
                            temp = sorted[i];
                            count = 1;
                        }
                    } 
                }
                if (cnt%2 == 1) {
                    median = sorted[cnt/2] ;   
                }
                else {
                    median = (sorted[cnt/2] + sorted[(cnt/2) - 1])/2.0;
                }

                avgInten = sum / nVox;

                if (minIntensity < totalMinIntensity) {
                    totalMinIntensity = minIntensity;
                }

                if (maxIntensity > totalMaxIntensity) {
                    totalMaxIntensity = maxIntensity;
                }

                stats.minIntensity = minIntensity;
                stats.maxIntensity = maxIntensity;
                stats.avgInten = avgInten;
                stats.nVox = nVox;
                stats.sum = sum;
                stats.median = median;
                stats.mode = mode;
                stats.maxCount = maxCount;
            }

            double area = nVox * (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1]);
            stats.area = area;
            double volume = area * fileInfo.getResolutions()[2];
            stats.volume = volume;

            // add perimeter
            stats.perimeter = perimeter;
            stats.largestContourDistance = largestContourDistance;

            // calculate standard deviation, coefficient of skewness, and coefficient of kurtosis

            double sum2 = 0, sumR2 = 0, sumG2 = 0, sumB2 = 0;
            double sum3 = 0, sumR3 = 0, sumG3 = 0, sumB3 = 0;
            double sum4 = 0, sumR4 = 0, sumG4 = 0, sumB4 = 0;
            sum2 = sumR2 = sumG2 = sumB2 = sum3 = sumR3 = sumG3 = sumB3 = sum4 = sumR4 = sumG4 = sumB4 = 0;
            // Calculate centers of mass
            double xMass, yMass, zMass, xMassR, yMassR, zMassR, xMassG, yMassG, zMassG, xMassB, yMassB, zMassB;
            xMass = yMass = zMass = 0;
            xMassR = yMassR = zMassR = xMassG = yMassG = zMassG = xMassB = yMassB = zMassB = 0;

            int cnt = 0;

            int xDim = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
            int yDim = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
            if (srcImage.isColorImage()) { 
                double diffR, diffG, diffB;
                double R2, R3, R4;
                double G2, G3, G4;
                double B2, B3, B4;
                Vector3f[] kBounds = contour.getImageBoundingBox();
                for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
                {
                    for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
                    {
                        for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                        {
                            int index = z * yDim * xDim + y * xDim + x;
                            float fR = imgBuffer[index + 1];
                            float fG = imgBuffer[index + 1];
                            float fB = imgBuffer[index + 1];

                            if ( contour.contains(x,y,z) && !inRange(ignoreMin, ignoreMax, fR) &&
                                    !inRange(ignoreMin, ignoreMax, fG) &&
                                    !inRange(ignoreMin, ignoreMax, fB)) {
                                xMassR += x * fR;
                                yMassR += y * fG;
                                zMassR += z * fB;
                                diffR = fR - avgIntenR;
                                R2 = diffR * diffR;
                                sumR2 += R2;
                                R3 = R2 * diffR;
                                sumR3 += R3;
                                R4 = R3 * diffR;
                                sumR4 += R4;
                                xMassG += x * fG;
                                yMassG += y * fG;
                                zMassG += z * fG;
                                diffG = fG - avgIntenG;
                                G2 = diffG * diffG;
                                sumG2 += G2;
                                G3 = G2 * diffG;
                                sumG3 += G3;
                                G4 = G3 * diffG;
                                sumG4 += G4;
                                xMassB += x * fB;
                                yMassB += y * fB;
                                zMassB += z * fB;
                                diffB = fB - avgIntenB;
                                B2 = diffB * diffB;
                                sumB2 += B2;
                                B3 = B2 * diffB;
                                sumB3 += B3;
                                B4 = B3 * diffB;
                                sumB4 += B4;
                                cnt++;
                            }
                        }
                    }
                }

                float stdDevR = (float) Math.sqrt(sumR2 / (cnt-1));
                float stdDevG = (float) Math.sqrt(sumG2 / (cnt-1));
                float stdDevB = (float) Math.sqrt(sumB2/ (cnt-1));
                stats.stdDevR = stdDevR;
                stats.stdDevG = stdDevG;
                stats.stdDevB = stdDevB;

                // moments around the mean
                double moment2R = sumR2/cnt;
                double moment2G = sumG2/cnt;
                double moment2B = sumB2/cnt;
                double moment3R = sumR3/cnt;
                double moment3G = sumG3/cnt;
                double moment3B = sumB3/cnt;
                double moment4R = sumR4/cnt;
                double moment4G = sumG4/cnt;
                double moment4B = sumB4/cnt;
                float skewnessR = (float)(moment3R/Math.pow(moment2R, 1.5));
                float skewnessG = (float)(moment3G/Math.pow(moment2G, 1.5));
                float skewnessB = (float)(moment3B/Math.pow(moment2B, 1.5));
                stats.skewnessR = skewnessR;
                stats.skewnessG = skewnessG;
                stats.skewnessB = skewnessB;
                double kurtosisR = moment4R/(moment2R * moment2R);
                double kurtosisG = moment4G/(moment2G * moment2G);
                double kurtosisB = moment4B/(moment2B * moment2B);
                stats.kurtosisR = kurtosisR;
                stats.kurtosisG = kurtosisG;
                stats.kurtosisB = kurtosisB;
                // Centers of mass
                double xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/sumR;
                double yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/sumR;
                double zCOMR = zMassR/sumR;
                unitStr = unit2DStr + "\tZ";
                if (srcImage.getNDims() > 2) {
                    zCOMR *= srcImage.getFileInfo(0).getResolutions()[2];
                    unitStr = unit3DStr;
                }

                comStr = unitStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" + nf.format(zCOMR);
                Vector3f centerPt = new Vector3f();
                centerPt.X = (float)xCOMR;
                centerPt.Y = (float)yCOMR;
                centerPt.Z = (float)zCOMR;
                comStr = addScannerLabels(comStr, centerPt);
                stats.massCenterDescriptionR = new String(comStr);

                double xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/sumG;
                double yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/sumG;
                double zCOMG= zMassG/sumG;
                if (srcImage.getNDims() > 2) {
                    zCOMG *= srcImage.getFileInfo(0).getResolutions()[2];
                }

                comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" + nf.format(zCOMG);
                centerPt = new Vector3f();
                centerPt.X = (float)xCOMG;
                centerPt.Y = (float)yCOMG;
                centerPt.Z = (float)zCOMG;
                comStr = addScannerLabels(comStr, centerPt);
                stats.massCenterDescriptionG = new String(comStr);

                double xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/sumB;
                double yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/sumB;
                double zCOMB = zMassB/sumB;
                if (srcImage.getNDims() > 2) {
                    zCOMB *= srcImage.getFileInfo(0).getResolutions()[2];
                }

                comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" + nf.format(zCOMB);
                centerPt = new Vector3f();
                centerPt.X = (float)xCOMB;
                centerPt.Y = (float)yCOMB;
                centerPt.Z = (float)zCOMB;
                comStr = addScannerLabels(comStr, centerPt);
                stats.massCenterDescriptionB = new String(comStr);

                stats.xMass = new double[]{ xMassR, xMassG, xMassB };
                stats.yMass = new double[]{ yMassR, yMassG, yMassB };
                stats.zMass = new double[]{  zMassR, zMassG, zMassB };
            } 
            else // not color image
            {
                double diff;
                double s2, s3, s4;
                Vector3f[] kBounds = contour.getImageBoundingBox();
                for ( int z = (int)kBounds[0].Z; z <= (int)kBounds[1].Z; z++ )
                {
                    for ( int y = (int)kBounds[0].Y; y <= (int)kBounds[1].Y; y++ )
                    {
                        for ( int x = (int)kBounds[0].X; x <= (int)kBounds[1].X; x++ )
                        {
                            int index = z * yDim * xDim + y * xDim + x;

                            if (contour.contains(x,y,z) && !inRange(ignoreMin, ignoreMax, imgBuffer[index])) {
                                xMass += x * imgBuffer[index];
                                yMass += y * imgBuffer[index];
                                zMass += z * imgBuffer[index];
                                diff = imgBuffer[index] - avgInten;
                                s2 = diff * diff;
                                sum2 += s2;
                                s3 = s2 * diff;
                                sum3 += s3;
                                s4 = s3 * diff;
                                sum4 += s4;
                                cnt++;
                            }
                        }
                    }
                }

                double stdDev = (float) Math.sqrt(sum2 / (cnt-1));
                stats.stdDev = stdDev;
                double moment2 = sum2/cnt;
                double moment3 = sum3/cnt;
                double moment4 = sum4/cnt;
                double skewness = (float)(moment3/Math.pow(moment2, 1.5));
                stats.skewness = skewness;
                double kurtosis = moment4/(moment2 * moment2);
                stats.kurtosis = kurtosis;
                // Center of mass
                double xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/sum;
                double yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/sum;
                double zCOM = zMass/sum;
                unitStr = unit2DStr + "\tZ";
                if (srcImage.getNDims() > 2) {
                    zCOM *= srcImage.getFileInfo(0).getResolutions()[2];
                    unitStr = unit3DStr;
                }

                comStr = unitStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" +
                nf.format(zCOM);
                Vector3f centerPt = new Vector3f();
                centerPt.X = (float)xCOM;
                centerPt.Y = (float)yCOM;
                centerPt.Z = (float)zCOM;
                comStr = addScannerLabels(comStr, centerPt);
                stats.massCenterDescription = new String(comStr);

                stats.massI = new double[]{ xMass, yMass, zMass };
            }
            return stats;
        }

        private void printStatsPerContour( FileInfoBase fileInfo, ContourStats stats, VOIStatisticalProperties statProperty, int iSlice, int iID )
        {
            String end = iSlice + ";" + iID;

            statProperty.setProperty(VOIStatisticList.axisDescription + end, nf.format(stats.PAxis));
            statProperty.setProperty(VOIStatisticList.eccentricityDescription + end, nf.format(stats.Ecc));
            statProperty.setProperty(VOIStatisticList.majorAxisDescription + end, nf.format(stats.MajorAxis));
            statProperty.setProperty(VOIStatisticList.minorAxisDescription + end, nf.format(stats.MinorAxis));
            statProperty.setProperty(VOIStatisticList.geometricCenterDescription + end, stats.gCenterString);
            statProperty.setProperty(VOIStatisticList.maxWidthDescription + end,
                    nf.format(Math.sqrt(((stats.pts[1].X - stats.pts[0].X) *
                            fileInfo.getResolutions()[0] *
                            (stats.pts[1].X - stats.pts[0].X) *
                            fileInfo.getResolutions()[0]) +
                            ((stats.pts[1].Y - stats.pts[0].Y) *
                                    fileInfo.getResolutions()[1] *
                                    (stats.pts[1].Y - stats.pts[0].Y) *
                                    fileInfo.getResolutions()[1]) +
                                    ((stats.pts[1].Z - stats.pts[0].Z) *
                                            fileInfo.getResolutions()[2] *
                                            (stats.pts[1].Z - stats.pts[0].Z) *
                                            fileInfo.getResolutions()[2]))));


            if (srcImage.isColorImage()) {
                statProperty.setProperty(VOIStatisticList.minIntensity + "Red" + end, nf.format(stats.minIntenRed));
                statProperty.setProperty(VOIStatisticList.maxIntensity + "Red" + end, nf.format(stats.maxIntenRed));
                statProperty.setProperty(VOIStatisticList.minIntensity + "Green" + end,
                        nf.format(stats.minIntenGreen));
                statProperty.setProperty(VOIStatisticList.maxIntensity + "Green" + end,
                        nf.format(stats.maxIntenGreen));
                statProperty.setProperty(VOIStatisticList.minIntensity + "Blue" + end, nf.format(stats.minIntenBlue));
                statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue" + end, nf.format(stats.maxIntenBlue));
                statProperty.setProperty(VOIStatisticList.avgIntensity + "Red" + end, nf.format(stats.avgIntenR));
                statProperty.setProperty(VOIStatisticList.avgIntensity + "Green" + end, nf.format(stats.avgIntenG));
                statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue" + end, nf.format(stats.avgIntenB));
                statProperty.setProperty(VOIStatisticList.quantityDescription + end, nf.format(stats.nVox));
                statProperty.setProperty(VOIStatisticList.sumIntensities + "Red"  + end, nf.format(stats.sumR));
                statProperty.setProperty(VOIStatisticList.sumIntensities + "Green" + end, nf.format(stats.sumG));
                statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue" + end, nf.format(stats.sumB));

                statProperty.setProperty(VOIStatisticList.median + "Red" + end, nf.format(stats.medianR));
                statProperty.setProperty(VOIStatisticList.median + "Green" + end, nf.format(stats.medianG));
                statProperty.setProperty(VOIStatisticList.median + "Blue" + end, nf.format(stats.medianB));

                statProperty.setProperty(VOIStatisticList.mode + "Red" + end, nf.format(stats.modeR));
                statProperty.setProperty(VOIStatisticList.mode + "Green" + end, nf.format(stats.modeG));
                statProperty.setProperty(VOIStatisticList.mode + "Blue" + end, nf.format(stats.modeB));

                statProperty.setProperty(VOIStatisticList.modeCount + "Red" + end, nf.format(stats.maxCountR));
                statProperty.setProperty(VOIStatisticList.modeCount + "Green" + end, nf.format(stats.maxCountG));
                statProperty.setProperty(VOIStatisticList.modeCount + "Blue" + end, nf.format(stats.maxCountB));
            } 
            else /* not color image */ 
            {
                statProperty.setProperty(VOIStatisticList.minIntensity + end, nf.format(stats.minIntensity));
                statProperty.setProperty(VOIStatisticList.maxIntensity + end, nf.format(stats.maxIntensity));
                statProperty.setProperty(VOIStatisticList.avgIntensity + end, nf.format(stats.avgInten));
                statProperty.setProperty(VOIStatisticList.quantityDescription + end, nf.format(stats.nVox));
                statProperty.setProperty(VOIStatisticList.sumIntensities + end, nf.format(stats.sum));
                statProperty.setProperty(VOIStatisticList.median + end, nf.format(stats.median));
                statProperty.setProperty(VOIStatisticList.mode + end, nf.format(stats.mode));
                statProperty.setProperty(VOIStatisticList.modeCount + end, nf.format(stats.maxCount));
            }
            statProperty.setProperty(VOIStatisticList.areaDescription + end, nf.format(stats.area));
            statProperty.setProperty(VOIStatisticList.volumeDescription + end, nf.format(stats.volume));

            // add perimeter
            statProperty.setProperty(VOIStatisticList.perimeterDescription + end, nf.format(stats.perimeter));
            statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + end, nf.format(stats.largestContourDistance));

            if (srcImage.isColorImage()) { 
                statProperty.setProperty(VOIStatisticList.deviationDescription + "Red" + end,
                        nf.format(stats.stdDevR));
                statProperty.setProperty(VOIStatisticList.deviationDescription + "Green" + end,
                        nf.format(stats.stdDevG));
                statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue" + end,
                        nf.format(stats.stdDevB));
                statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red" + end,
                        nf.format(stats.skewnessR));
                statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green" + end,
                        nf.format(stats.skewnessG));
                statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue" + end,
                        nf.format(stats.skewnessB));
                statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red" + end,
                        nf.format(stats.kurtosisR));
                statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green" + end,
                        nf.format(stats.kurtosisG));
                statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue" + end,
                        nf.format(stats.kurtosisB));
                statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red" + end, stats.massCenterDescriptionR);
                statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green" + end, stats.massCenterDescriptionG);
                statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue" + end, stats.massCenterDescriptionB);
            } 
            else // not color image
            {
                statProperty.setProperty(VOIStatisticList.deviationDescription + end, nf.format(stats.stdDev));
                statProperty.setProperty(VOIStatisticList.skewnessDescription + end, nf.format(stats.skewness));
                statProperty.setProperty(VOIStatisticList.kurtosisDescription + end, nf.format(stats.kurtosis));
                statProperty.setProperty(VOIStatisticList.massCenterDescription + end, stats.massCenterDescription);
            }
        }

        private void printTotals(FileInfoBase fileInfo, Vector<ContourStats> allStats, 
                VOIStatisticalProperties statProperty, 
                String unit2DStr, String unit3DStr, String end, BitSet mask, float[] imgBuffer, float ignoreMin, float ignoreMax,
                double largestDistance   )
        {
            ContourStats[] stats = new ContourStats[allStats.size()];
            for ( int i = 0; i < stats.length; i++ )
            {
                stats[i] = allStats.elementAt(i);
            }
            printTotals( fileInfo, stats, statProperty, 
                    unit2DStr, unit3DStr, end, mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
        }


        private void printTotalsColor(FileInfoBase fileInfo, ContourStats[] stats, VOIStatisticalProperties statProperty, 
                String unit2DStr, String unit3DStr, String end, BitSet mask, float[] imgBuffer, float ignoreMin, float ignoreMax  )
        {
            int totalNVox = 0;
            double totalXMassR = 0;
            double totalXMassG = 0;
            double totalXMassB = 0;
            double totalYMassR = 0;
            double totalYMassG = 0;
            double totalYMassB = 0;
            double totalZMassR = 0;
            double totalZMassG = 0;
            double totalZMassB = 0;
            for (int q = 0; q < stats.length; q++) {
                totalNVox += stats[q].valuesRGB.size();
                totalXMassR += stats[q].xMass[0];
                totalXMassG += stats[q].xMass[1];
                totalXMassB += stats[q].xMass[2];
                totalYMassR += stats[q].yMass[0];
                totalYMassG += stats[q].yMass[1];
                totalYMassB += stats[q].yMass[2];
                totalZMassR += stats[q].zMass[0];
                totalZMassG += stats[q].zMass[1];
                totalZMassB += stats[q].zMass[2];
            }
            float[] totalBuffR = new float[totalNVox];
            float[] totalBuffG = new float[totalNVox];
            float[] totalBuffB = new float[totalNVox];
            int vox = 0;
            double totalSumR = 0;
            double totalSumG = 0;
            double totalSumB = 0;
            for (int q = 0; q < stats.length; q++) {
                for ( int r = 0; r < stats[q].valuesRGB.size(); r++ )
                {
                    totalBuffR[ vox ] = stats[q].valuesRGB.elementAt(r).R;
                    totalBuffG[ vox ] = stats[q].valuesRGB.elementAt(r).G;
                    totalBuffB[ vox ] = stats[q].valuesRGB.elementAt(r).B;
                    totalSumR += stats[q].valuesRGB.elementAt(r).R;
                    totalSumG += stats[q].valuesRGB.elementAt(r).G;
                    totalSumB += stats[q].valuesRGB.elementAt(r).B;
                    vox++;
                }
            }                       

            Arrays.sort(totalBuffR);
            float[] totalSortedR = totalBuffR;
            double totalMinIntenRed = totalSortedR[0];
            double totalMaxIntenRed = totalSortedR[ totalSortedR.length-1];
            double totalMedianR = 0;
            int totalCntR = totalSortedR.length;
            if (totalCntR%2 == 1) {
                totalMedianR = totalSortedR[totalCntR/2] ;   
            }
            else {
                totalMedianR = (totalSortedR[totalCntR/2] + totalSortedR[(totalCntR/2) - 1])/2.0;
            }

            float tempR = 0;
            int countR = 0;
            float modeR = 0;
            float maxCountR = 0;
            for( int i=0; i < totalSortedR.length; i++ )
            {
                if( i==0 )
                {
                    tempR = totalSortedR[i];
                    countR = 1;
                    modeR = tempR;
                    maxCountR = 1;
                }
                else {
                    if( totalSortedR[i] == tempR ) {
                        countR++;
                        if( countR > maxCountR ) {
                            maxCountR = countR;
                            modeR = tempR;
                        }
                    }
                    else {
                        tempR = totalSortedR[i];
                        countR = 1;
                    }
                } 
            }

            Arrays.sort(totalBuffG);
            float[] totalSortedG = totalBuffG;
            double totalMinIntenGreen = totalSortedG[0];
            double totalMaxIntenGreen = totalSortedG[ totalSortedG.length-1];
            double totalMedianG = 0;
            int totalCntG = totalSortedG.length;
            if (totalCntG%2 == 1) {
                totalMedianG = totalSortedG[totalCntG/2] ;   
            }
            else {
                totalMedianG = (totalSortedG[totalCntG/2] + totalSortedG[(totalCntG/2) - 1])/2.0;
            }
            float tempG = 0;
            int countG = 0;
            float modeG = 0;
            float maxCountG = 0;
            for( int i=0; i < totalSortedG.length; i++ )
            {
                if( i==0 )
                {
                    tempG = totalSortedG[i];
                    countG = 1;
                    modeG = tempG;
                    maxCountG = 1;
                }
                else {
                    if( totalSortedG[i] == tempG ) {
                        countG++;
                        if( countG > maxCountG ) {
                            maxCountG = countG;
                            modeG = tempG;
                        }
                    }
                    else {
                        tempG = totalSortedG[i];
                        countG = 1;
                    }
                } 
            }



            Arrays.sort(totalBuffB);
            float[] totalSortedB = totalBuffB;
            double totalMinIntenBlue = totalSortedB[0];
            double totalMaxIntenBlue = totalSortedB[ totalSortedG.length-1];
            double totalMedianB = 0;
            int totalCntB = totalSortedB.length;
            if (totalCntB%2 == 1) {
                totalMedianB = totalSortedB[totalCntB/2] ;   
            }
            else {
                totalMedianB = (totalSortedB[totalCntB/2] + totalSortedB[(totalCntB/2) - 1])/2.0;
            }
            float tempB = 0;
            int countB = 0;
            float modeB = 0;
            float maxCountB = 0;
            for( int i=0; i < totalSortedB.length; i++ )
            {
                if( i==0 )
                {
                    tempB = totalSortedB[i];
                    countB = 1;
                    modeB = tempB;
                    maxCountB = 1;
                }
                else {
                    if( totalSortedB[i] == tempB ) {
                        countB++;
                        if( countB > maxCountB ) {
                            maxCountB = countB;
                            modeB = tempB;
                        }
                    }
                    else {
                        tempB = totalSortedB[i];
                        countB = 1;
                    }
                } 
            }


            statProperty.setProperty(VOIStatisticList.minIntensity + "Red" + end, nf.format(totalMinIntenRed));
            statProperty.setProperty(VOIStatisticList.maxIntensity + "Red" + end, nf.format(totalMaxIntenRed));
            statProperty.setProperty(VOIStatisticList.minIntensity + "Green" + end, nf.format(totalMinIntenGreen));
            statProperty.setProperty(VOIStatisticList.maxIntensity + "Green" + end, nf.format(totalMaxIntenGreen));
            statProperty.setProperty(VOIStatisticList.minIntensity + "Blue" + end, nf.format(totalMinIntenBlue));
            statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue" + end, nf.format(totalMaxIntenBlue));
            statProperty.setProperty(VOIStatisticList.avgIntensity + "Red" + end, nf.format(totalSumR / totalNVox));
            statProperty.setProperty(VOIStatisticList.avgIntensity + "Green" + end, nf.format(totalSumG / totalNVox));
            statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue" + end, nf.format(totalSumB / totalNVox));
            statProperty.setProperty(VOIStatisticList.sumIntensities + "Red" + end, nf.format(totalSumR));
            statProperty.setProperty(VOIStatisticList.sumIntensities + "Green" + end, nf.format(totalSumG));
            statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue" + end, nf.format(totalSumB));

            statProperty.setProperty(VOIStatisticList.mode + "Red" + end, nf.format(modeR));
            statProperty.setProperty(VOIStatisticList.mode + "Green" + end, nf.format(modeG));
            statProperty.setProperty(VOIStatisticList.mode + "Blue" + end, nf.format(modeB));

            statProperty.setProperty(VOIStatisticList.modeCount + "Red" + end, nf.format(maxCountR));
            statProperty.setProperty(VOIStatisticList.modeCount + "Green" + end, nf.format(maxCountG));
            statProperty.setProperty(VOIStatisticList.modeCount + "Blue" + end, nf.format(maxCountB));

            statProperty.setProperty(VOIStatisticList.median + "Red" + end, nf.format(totalMedianR));
            statProperty.setProperty(VOIStatisticList.median + "Green" + end, nf.format(totalMedianG));
            statProperty.setProperty(VOIStatisticList.median + "Blue" + end, nf.format(totalMedianB));




            double sumR2 = 0, sumG2 = 0, sumB2 = 0;
            double sumR3 = 0, sumG3 = 0, sumB3 = 0;
            double sumR4 = 0, sumG4 = 0, sumB4 = 0;
            // Calculate centers of mass
            double xMassR, yMassR, zMassR, xMassG, yMassG, zMassG, xMassB, yMassB, zMassB;
            xMassR = yMassR = zMassR = xMassG = yMassG = zMassG = xMassB = yMassB = zMassB = 0;

            int cnt = 0;

            int xDim = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
            int yDim = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;

            double diffR, diffG, diffB;
            double R2, R3, R4;
            double G2, G3, G4;
            double B2, B3, B4;
            double avgIntenR = totalSumR / totalNVox;
            double avgIntenG = totalSumG / totalNVox;
            double avgIntenB = totalSumB / totalNVox;

            int[] xB = new int[2];
            int[] yB = new int[2];
            int[] zB = new int[2];
            calcSelectedVOI.getBounds( xB, yB, zB );
            for ( int z = zB[0]; z <= zB[1]; z++ )
            {
                for ( int y = yB[0]; y <= yB[1]; y++ )
                {
                    for ( int x = xB[0]; x <= xB[1]; x++ )
                    {
                        int index = z * yDim * xDim + y * xDim + x;
                        float fR = imgBuffer[index + 1];
                        float fG = imgBuffer[index + 1];
                        float fB = imgBuffer[index + 1];

                        if ( mask.get(index) && !inRange(ignoreMin, ignoreMax, fR) &&
                                !inRange(ignoreMin, ignoreMax, fG) &&
                                !inRange(ignoreMin, ignoreMax, fB)) {
                            xMassR += x * fR;
                            yMassR += y * fG;
                            zMassR += z * fB;
                            diffR = fR - avgIntenR;
                            R2 = diffR * diffR;
                            sumR2 += R2;
                            R3 = R2 * diffR;
                            sumR3 += R3;
                            R4 = R3 * diffR;
                            sumR4 += R4;
                            xMassG += x * fG;
                            yMassG += y * fG;
                            zMassG += z * fG;
                            diffG = fG - avgIntenG;
                            G2 = diffG * diffG;
                            sumG2 += G2;
                            G3 = G2 * diffG;
                            sumG3 += G3;
                            G4 = G3 * diffG;
                            sumG4 += G4;
                            xMassB += x * fB;
                            yMassB += y * fB;
                            zMassB += z * fB;
                            diffB = fB - avgIntenB;
                            B2 = diffB * diffB;
                            sumB2 += B2;
                            B3 = B2 * diffB;
                            sumB3 += B3;
                            B4 = B3 * diffB;
                            sumB4 += B4;
                            cnt++;
                        }
                    }
                }
            }

            float stdDevR = (float) Math.sqrt(sumR2 / (cnt-1));
            float stdDevG = (float) Math.sqrt(sumG2 / (cnt-1));
            float stdDevB = (float) Math.sqrt(sumB2/ (cnt-1));

            // moments around the mean
            double moment2R = sumR2/cnt;
            double moment2G = sumG2/cnt;
            double moment2B = sumB2/cnt;
            double moment3R = sumR3/cnt;
            double moment3G = sumG3/cnt;
            double moment3B = sumB3/cnt;
            double moment4R = sumR4/cnt;
            double moment4G = sumG4/cnt;
            double moment4B = sumB4/cnt;
            float skewnessR = (float)(moment3R/Math.pow(moment2R, 1.5));
            float skewnessG = (float)(moment3G/Math.pow(moment2G, 1.5));
            float skewnessB = (float)(moment3B/Math.pow(moment2B, 1.5));
            double kurtosisR = moment4R/(moment2R * moment2R);
            double kurtosisG = moment4G/(moment2G * moment2G);
            double kurtosisB = moment4B/(moment2B * moment2B);
            // Centers of mass
            double xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/totalSumR;
            double yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/totalSumR;
            double zCOMR = zMassR/totalSumR;
            String unitStr = unit2DStr + "\tZ";
            if (srcImage.getNDims() > 2) {
                zCOMR *= srcImage.getFileInfo(0).getResolutions()[2];
                unitStr = unit3DStr;
            }

            String comStr = unitStr + "\n  Red\t\t" + nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" + nf.format(zCOMR);
            Vector3f centerPt = new Vector3f();
            centerPt.X = (float)xCOMR;
            centerPt.Y = (float)yCOMR;
            centerPt.Z = (float)zCOMR;
            comStr = addScannerLabels(comStr, centerPt);

            double xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/totalSumG;
            double yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/totalSumG;
            double zCOMG= zMassG/totalSumG;
            if (srcImage.getNDims() > 2) {
                zCOMG *= srcImage.getFileInfo(0).getResolutions()[2];
            }

            comStr = "\n  Green\t\t" + nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" + nf.format(zCOMG);
            centerPt = new Vector3f();
            centerPt.X = (float)xCOMG;
            centerPt.Y = (float)yCOMG;
            centerPt.Z = (float)zCOMG;
            comStr = addScannerLabels(comStr, centerPt);

            double xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/totalSumB;
            double yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/totalSumB;
            double zCOMB = zMassB/totalSumB;
            if (srcImage.getNDims() > 2) {
                zCOMB *= srcImage.getFileInfo(0).getResolutions()[2];
            }

            comStr = "\n  Blue\t\t" + nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" + nf.format(zCOMB);
            centerPt = new Vector3f();
            centerPt.X = (float)xCOMB;
            centerPt.Y = (float)yCOMB;
            centerPt.Z = (float)zCOMB;
            comStr = addScannerLabels(comStr, centerPt);
            
            statProperty.setProperty(VOIStatisticList.deviationDescription + "Red" + end, nf.format(stdDevR));
            statProperty.setProperty(VOIStatisticList.deviationDescription + "Green" + end, nf.format(stdDevG));
            statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue" + end, nf.format(stdDevB));
            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red" + end, nf.format(skewnessR));
            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green" + end, nf.format(skewnessG));
            statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue" + end, nf.format(skewnessB));
            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red" + end, nf.format(kurtosisR));
            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green" + end, nf.format(kurtosisG));
            statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue" + end, nf.format(kurtosisB));
            statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red" + end, comStr);
            statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue" + end, comStr);
        }

        private void printTotalsGrayScale(FileInfoBase fileInfo, ContourStats[] stats, 
                VOIStatisticalProperties statProperty, 
                String unit2DStr, String unit3DStr, String end, BitSet mask, float[] imgBuffer, float ignoreMin, float ignoreMax )
        {
            int xDim = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
            int yDim = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;

            int totalNVox = 0;
            double totalXMass = 0;
            double totalYMass = 0;
            double totalZMass = 0;
            for (int q = 0; q < stats.length; q++) {
                totalNVox += stats[q].values.size();
                totalXMass += stats[q].massI[0];
                totalYMass += stats[q].massI[1];
                totalZMass += stats[q].massI[2];
            }

            float[] totalBuff = new float[totalNVox];
            int vox = 0;
            double totalSum = 0;
            for (int q = 0; q < stats.length; q++) {
                for ( int r = 0; r < stats[q].values.size(); r++ )
                {
                    totalBuff[ vox++ ] = stats[q].values.elementAt(r).floatValue();
                    totalSum += stats[q].values.elementAt(r).floatValue();
                }
            }                       
            Arrays.sort(totalBuff);
            float[] totalSorted = totalBuff;
            double totalMinIntensity = totalSorted[0];
            double totalMaxIntensity = totalSorted[ totalSorted.length -1];
            double totalMedian = 0;
            int totalCnt = totalSorted.length;
            if (totalCnt%2 == 1) {
                totalMedian = totalSorted[totalCnt/2] ;   
            }
            else {
                totalMedian = (totalSorted[totalCnt/2] + totalSorted[(totalCnt/2) - 1])/2.0;
            }


            float temp = 0;
            int count = 0;
            float mode = 0;
            float maxCount = 0;
            for( int i = 0; i < totalSorted.length; i++ )
            {
                if( i==0 )
                {
                    temp = totalSorted[i];
                    count = 1;
                    mode = temp;
                    maxCount = 1;
                }
                else {
                    if ( totalSorted[i] == temp ) {
                        count++;
                        if ( count > maxCount ) {
                            maxCount = count;
                            mode = temp;
                        }
                    } else {
                        temp = totalSorted[i];
                        count = 1;
                    }
                } 
            }

            statProperty.setProperty(VOIStatisticList.minIntensity + end, nf.format(totalMinIntensity));
            statProperty.setProperty(VOIStatisticList.maxIntensity + end, nf.format(totalMaxIntensity));
            statProperty.setProperty(VOIStatisticList.avgIntensity + end, nf.format(totalSum / totalNVox));
            statProperty.setProperty(VOIStatisticList.quantityDescription + end, nf.format(totalNVox));
            statProperty.setProperty(VOIStatisticList.sumIntensities + end, nf.format(totalSum));
            statProperty.setProperty(VOIStatisticList.mode + end, nf.format(mode));
            statProperty.setProperty(VOIStatisticList.modeCount + end, nf.format(maxCount));
            statProperty.setProperty(VOIStatisticList.median + end, nf.format(totalMedian));

            double avgInten = totalSum/totalNVox;
            double diff;
            double s2, s3, s4;
            double xMass = 0, yMass = 0, zMass = 0;
            double sum2 = 0, sum3 = 0, sum4 = 0;
            int cnt = 0;
            int[] xB = new int[2];
            int[] yB = new int[2];
            int[] zB = new int[2];
            calcSelectedVOI.getBounds( xB, yB, zB );
            for ( int z = zB[0]; z <= zB[1]; z++ )
            {
                for ( int y = yB[0]; y <= yB[1]; y++ )
                {
                    for ( int x = xB[0]; x <= xB[1]; x++ )
                    {
                        int index = z * yDim * xDim + y * xDim + x;

                        System.err.println( mask.get(index) );
                        System.err.println( imgBuffer[index] );
                        
                        if (mask.get(index) && !inRange(ignoreMin, ignoreMax, imgBuffer[index])) {
                            xMass += x * imgBuffer[index];
                            yMass += y * imgBuffer[index];
                            zMass += z * imgBuffer[index];
                            diff = imgBuffer[index] - avgInten;
                            s2 = diff * diff;
                            sum2 += s2;
                            s3 = s2 * diff;
                            sum3 += s3;
                            s4 = s3 * diff;
                            sum4 += s4;
                            cnt++;
                        }
                    }
                }
            }

            float stdDev = (float) Math.sqrt(sum2 / (cnt-1));
            statProperty.setProperty(VOIStatisticList.deviationDescription + end, nf.format(stdDev));
            double moment2 = sum2/cnt;
            double moment3 = sum3/cnt;
            double moment4 = sum4/cnt;
            double skewness = (float)(moment3/Math.pow(moment2, 1.5));
            statProperty.setProperty(VOIStatisticList.skewnessDescription + end, nf.format(skewness));
            double kurtosis = moment4/(moment2 * moment2);
            statProperty.setProperty(VOIStatisticList.kurtosisDescription + end, nf.format(kurtosis));
            // Center of mass
            double xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/totalSum;
            double yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/totalSum;
            double zCOM = zMass/totalSum;
            String unitStr = unit2DStr + "\tZ";
            if (srcImage.getNDims() > 2) {
                zCOM *= srcImage.getFileInfo(0).getResolutions()[2];
                unitStr = unit3DStr;
            }

            String comStr = unitStr + "\n\t\t" + nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" +
            nf.format(zCOM);
            Vector3f centerPt = new Vector3f();
            centerPt.X = (float)xCOM;
            centerPt.Y = (float)yCOM;
            centerPt.Z = (float)zCOM;
            comStr = addScannerLabels(comStr, centerPt);
            statProperty.setProperty(VOIStatisticList.massCenterDescription + end, comStr);
        }


        private void printTotals(FileInfoBase fileInfo, ContourStats[] stats, VOIStatisticalProperties statProperty, 
                String unit2DStr, String unit3DStr, String end, BitSet mask, float[] imgBuffer, float ignoreMin, float ignoreMax,
                double largestDistance)
        {
            double totalNVox = 0;
            double totalArea = 0;
            double totalVolume = 0;
            double totalPerimeter = 0;
            double largestAllSlicesDistance = 0;

            for (int q = 0; q < stats.length; q++) {
                totalArea += stats[q].area;
                totalVolume += stats[q].volume;
                totalPerimeter += stats[q].perimeter;
                largestAllSlicesDistance = Math.max( largestAllSlicesDistance, stats[q].largestContourDistance);
            }

            double xRes = srcImage.getFileInfo(0).getResolutions().length > 0 ? srcImage.getFileInfo(0).getResolutions()[0] : 1;
            double yRes = srcImage.getFileInfo(0).getResolutions().length > 1 ? srcImage.getFileInfo(0).getResolutions()[1] : 1;
            double zRes = srcImage.getFileInfo(0).getResolutions().length > 2 ? srcImage.getFileInfo(0).getResolutions()[2] : 1;

            Vector3f totalC = calcSelectedVOI.getGeometricCenter();
            totalC.X *= xRes;
            totalC.Y *= yRes;
            totalC.Z *= zRes;
            String unitStr = unit2DStr + "\tZ";

            String comStr = unitStr + "\n\t\t" + nf.format(totalC.X) + "\t" + nf.format(totalC.Y) + "\t" + nf.format(totalC.Z);
            comStr = addScannerLabels(comStr, totalC);

            //these do not have meaning on a 2.5D basis
            statProperty.setProperty(VOIStatisticList.axisDescription + end, "\t");
            statProperty.setProperty(VOIStatisticList.eccentricityDescription + end, "\t");
            statProperty.setProperty(VOIStatisticList.majorAxisDescription + end, "\t");
            statProperty.setProperty(VOIStatisticList.minorAxisDescription + end, "\t");

            statProperty.setProperty(VOIStatisticList.geometricCenterDescription + end, comStr);
            statProperty.setProperty(VOIStatisticList.areaDescription + end, nf.format(totalArea));
            statProperty.setProperty(VOIStatisticList.volumeDescription + end, nf.format(totalVolume));
            statProperty.setProperty(VOIStatisticList.quantityDescription + end, nf.format(totalNVox));
            statProperty.setProperty(VOIStatisticList.perimeterDescription + end, nf.format(totalPerimeter));
            statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription + end, nf.format(largestAllSlicesDistance));
            statProperty.setProperty(VOIStatisticList.largestDistanceDescription + end, nf.format(largestDistance));

            if ( srcImage.isColorImage() )
            {
                printTotalsColor( fileInfo, stats, statProperty,  unit2DStr, unit3DStr, end, mask, imgBuffer, ignoreMin, ignoreMax );
            }
            else
            {
                printTotalsGrayScale( fileInfo, stats, statProperty,  unit2DStr, unit3DStr, end, mask, imgBuffer, ignoreMin, ignoreMax );
            }
        }

    }


    /**
     * DOCUMENT ME!
     *
     * @param  numberOfVOIs  DOCUMENT ME!
     */
    private void initialiseDataHolders(int numberOfVOIs) {
        propertyList = new Vector<VOIStatisticalProperties>(numberOfVOIs);

        for (int i = 0; i < numberOfVOIs; i++) {
            propertyList.add(new VOIStatisticalProperties());
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   ignoreMin  DOCUMENT ME!
     * @param   ignoreMax  DOCUMENT ME!
     * @param   num        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean inRange(float ignoreMin, float ignoreMax, float num) {

        if (rangeFlag == JDialogVOIStatistics.NO_RANGE) {
            return false;
        } else if (rangeFlag == JDialogVOIStatistics.BETWEEN) {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                return true;
            } else {
                return false;
            }
        } else if (rangeFlag == JDialogVOIStatistics.OUTSIDE) {

            if ((num <= ignoreMin) || (num >= ignoreMax)) {
                return true;
            } else {
                return false;
            }
        } else {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                System.out.println(" min  = " + ignoreMax + " max = " + ignoreMax);

                return true;
            } else {
                return false;
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  leadBase  DOCUMENT ME!
     */
    private String addScannerLabels(String baseString, Vector3f currentPt) {

        String[] positions = null;
        Vector3f scaledPt;

        if ((srcImage.getFileInfo(0).getOrigin()[0] != 0) || (srcImage.getFileInfo(0).getOrigin()[1] != 0) ||
                (srcImage.getFileInfo(0).getOrigin()[2] != 0)) {

            scaledPt = new Vector3f();
            scaledPt.X = (currentPt.X/srcImage.getFileInfo(0).getResolutions()[0]);
            scaledPt.Y = (currentPt.Y/srcImage.getFileInfo(0).getResolutions()[1]);
            scaledPt.Z = (currentPt.Z/srcImage.getFileInfo(0).getResolutions()[2]);


            if (srcImage.getNDims() > 2) {
                positions = getScannerPositionLabels(scaledPt);
            }

            if (positions != null) {    
                baseString += positions[0] + " " + positions[1] + " " + positions[2];
            } 

        } 

        return baseString;

    }


    /**
     * Gets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL, or the orientations must be
     * set up correctly, or else the function returns null.
     * 
     * @param image The image the point lies within.
     * @param position (x,y,z(slice)) position in FileCoordinates
     * 
     * @return An array of strings that represent patient position.
     */
    public String[] getScannerPositionLabels(Vector3f position) {
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        Vector3f kOut = new Vector3f();
        if (srcImage.getNDims() < 3) {
            // return null;
        }

        MipavCoordinateSystems.fileToScanner(position, kOut, srcImage);

        float[] tCoord = new float[3];
        tCoord[0] = kOut.X;
        tCoord[1] = kOut.Y;
        tCoord[2] = kOut.Z;

        String[] labels = {"R-L: ", "A-P: ", "I-S: "};

        if ( !srcImage.getRadiologicalView()) {
            labels[0] = new String("L-R: ");
        }

        String[] strs = new String[3];

        if (srcImage.getRadiologicalView()) {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String("\t" + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String("\t" + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        } else {

            if ( (tCoord[0] < 0)) {
                strs[0] = new String("\t" + labels[0].charAt(2) + ": " + String.valueOf(nf.format(tCoord[0])));
            } else {
                strs[0] = new String("\t" + labels[0].charAt(0) + ": " + String.valueOf(nf.format(tCoord[0])));
            }
        }

        for (int i = 1; i < 3; i++) {

            if ( (tCoord[i] < 0)) {
                strs[i] = new String("\t" + labels[i].charAt(0) + ": " + String.valueOf(nf.format(tCoord[i])));
            } else {
                strs[i] = new String("\t" + labels[i].charAt(2) + ": " + String.valueOf(nf.format(tCoord[i])));
            }
        }

        return strs;
    }







}
