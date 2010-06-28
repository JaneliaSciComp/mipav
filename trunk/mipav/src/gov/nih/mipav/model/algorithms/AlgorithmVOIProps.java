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
    protected DecimalFormat nf;

    /** How the VOI calculations should be performed (entire, contour, slice). */
    protected int processType = PROCESS_PER_VOI;

    /** Vector to hold all properties calculated within the algorithm for later access. */
    protected Vector<VOIStatisticalProperties> propertyList;

    /** Whether or not to exclude a range of values. */
    protected int rangeFlag;

    /** Whether or not to calculate largest slice distance, true by default */
    protected boolean sliceDistanceFlag;

    /** Whether or not to calculate largest distance (only 3D), true by default */
    protected boolean distanceFlag;

    /** Vector of all VOIs that will have calculations performed. */
    protected ViewVOIVector selectedVOIset;

    /** Whether or not to show totals for each calculation. */
    protected boolean showTotals = false;

    /** Boolean for if the algorithm should ONLY check active contours */
    protected boolean doOnlyActiveContours = false;

    /**The top-level group of threads used for calculating. */
    private ThreadGroup calcGroup = new ThreadGroup("CalcVOI");

    protected boolean[] statsList = new boolean[numberOfStatistics];

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

        for ( int i = 0; i < numberOfStatistics; i++ )
        {
            statsList[i] = true;
        }
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
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.areaDescription)).floatValue();
    } // {return area;}

    /**
     * Gets the average intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgInten() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.avgIntensity)).floatValue();
    } // {return avgInten;}

    /**
     * Gets the average intensity of the Blue channel of VOI return average intensity of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.avgIntensity +
        "Blue")).floatValue();
    } // {return avgIntenB;}

    /**
     * Gets the average intensity of the Green channel of VOI return average intensity of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.avgIntensity +
        "Green")).floatValue();
    } // {return avgIntenG;}

    /**
     * Gets the average intensity of the Red channel of VOI return average intensity of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.avgIntensity +
        "Red")).floatValue();
    } // {return avgIntenR;}



    /**
     * Gets the median
     *
     * @return  DOCUMENT ME!
     */
    public float getMedian() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.median)).floatValue();
    }

    /**
     * Gets the median of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.median +
        "Blue")).floatValue();
    }

    /**
     * Gets the median of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.median +
        "Green")).floatValue();
    } 

    /**
     * Gets the median of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMedianR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.median +
        "Red")).floatValue();
    } 





    /**
     * Gets the mode
     *
     * @return  DOCUMENT ME!
     */
    public float getMode() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.mode)).floatValue();
    }

    /**
     * Gets the mode of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.mode +
        "Blue")).floatValue();
    }

    /**
     * Gets the mode of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.mode +
        "Green")).floatValue();
    } 

    /**
     * Gets the mode of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getModeR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.mode +
        "Red")).floatValue();
    } 





    /**
     * Gets the mode
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCount() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.modeCount)).intValue();
    }

    /**
     * Gets the mode of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountB() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.modeCount +
        "Blue")).intValue();
    }

    /**
     * Gets the mode of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountG() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.modeCount +
        "Green")).intValue();
    } 

    /**
     * Gets the mode of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getModeCountR() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.modeCount +
        "Red")).intValue();
    }






    /**
     * Gets the the geometric center of the VOI ; return geometric center defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs
    public String getGeometricCenter() {
        VOIStatisticalProperties p = propertyList.firstElement(); 
        return (p).getProperty(VOIStatisticList.geometricCenterDescription);
    } // {return gcPt;}

    /**
     * Gets the the center of mass of the VOI ; return center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs
    public String getCenterOfMass() {
        return propertyList.firstElement().getProperty(VOIStatisticList.massCenterDescription);
    } // {return cenMassPt;}

    /**
     * Gets the the red center of mass of the VOI ; return red center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    //TODO: Should report results of all VOIs, etc.
    public String getCenterOfMassR() {
        return propertyList.firstElement().getProperty(VOIStatisticList.massCenterDescription + "Red");
    } // {return cenMassPtR;}

    /**
     * Gets the the green center of mass of the VOI ; return green center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterOfMassG() {
        return propertyList.firstElement().getProperty(VOIStatisticList.massCenterDescription + "Green");
    } // {return cenMassPtG;}

    /**
     * Gets the the blue center of mass of the VOI ; return blue center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterOfMassB() {
        return propertyList.firstElement().getProperty(VOIStatisticList.massCenterDescription + "Blue");
    } // {return cenMassPtB;}

    /**
     * Gets the eccentricity of the VOI: 1 = line, 0 = circle; return eccentricity of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getEccentricity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.eccentricityDescription)).floatValue();
    } // {return eccentricity;}

    /**
     * Gets the major axis of VOI (only valid for 2D object); return major axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMajorAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.majorAxisDescription)).floatValue();
    } // {return majorAxis;}

    /**
     * Gets the maximum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.maxIntensity)).floatValue();
    } // {return maxIntensity;}

    /**
     * Gets the maximum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityBlue() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.maxIntensity +
        "Blue")).floatValue();
    } // {return maxIntenBlue;}

    /**
     * Gets the maximum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityGreen() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.maxIntensity +
        "Green")).floatValue();
    } // {return maxIntenGreen;}

    /**
     * Gets the maximum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityRed() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.maxIntensity +
        "Red")).floatValue();
    } // {return maxIntenRed;}

    /**
     * Gets the greatest distance between any two point of the VOI return distance.
     *
     * @return  DOCUMENT ME!
    public double getMaxWidth() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.maxWidthDescription)).floatValue();
    } // {return maxDistance;}

     */
    /**
     * Gets the minimum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensity() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.minIntensity)).floatValue();
    } // {return minIntensity;}

    /**
     * Gets the minimum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityBlue() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.minIntensity +
        "Blue")).floatValue();
    } // {return minIntenBlue;}

    /**
     * Gets the minimum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityGreen() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.minIntensity +
        "Green")).floatValue();
    } // {return minIntenGreen;}

    /**
     * Gets the minimum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityRed() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.minIntensity +
        "Red")).floatValue();
    } // {return minIntenRed;}

    /**
     * Gets the minor axis of VOI (only valid for 2D object); return minor axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinorAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.minorAxisDescription)).floatValue();
    } // {return minorAxis;}

    /**
     * Gets the the number of pixels return number of pixels defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getNVoxels() {
        return Integer.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.quantityDescription)).intValue();
    } // {return nVox;}

    /**
     * Gets the perimeter of the VOI (in terms of res).
     *
     * @return  String perimeter string
     */
    public String getPerimeter() {
        return propertyList.firstElement().getProperty(VOIStatisticList.perimeterDescription);
    } // {return perimeter;}

    /**
     * Gets the largest line segment totally contained within a VOI slice (in terms of res).
     * If this unexpectedly returns zero, make sure you have not inadvertently set sliceDistanceFlag to false.
     *
     * @return  String largest slice distance string
     */
    public String getLargestSliceDistance() {
        return propertyList.firstElement().getProperty(VOIStatisticList.largestSliceDistanceDescription);
    } // {return largestSliceDistance;}

    /**
     * Gets the largest line segment totally contained within a 3D VOI (in terms of res).
     * If this unexpectedly returns zero, make sure you have not inadvertently set distanceFlag to false.
     *
     * @return  String largest distance string
     */
    public String getLargestDistance() {
        return propertyList.firstElement().getProperty(VOIStatisticList.largestDistanceDescription);
    } // {return largestDistance;}

    /**
     * Gets the principle axis of VOI (only valid for 2D object); return pricipal axis angle of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getPrincipalAxis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.axisDescription)).floatValue();
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
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.deviationDescription)).floatValue();
    } // {return stdDev;}

    /**
     * Gets the get standard deviation of image intensities (blue channel) return standard deviation of image
     * intensities defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.deviationDescription +
        "Blue")).floatValue();
    } // {return stdDevB;}

    /**
     * Gets the standard deviation of image intensities (green channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.deviationDescription +
        "Green")).floatValue();
    } // {return stdDevG;}


    /**
     * Gets the standard deviation of image intensities (red channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.deviationDescription +
        "Red")).floatValue();
    } // {return stdDevR;}

    /**
     * Gets the sum of image intensities defined by the VOI
     * @return
     */
    public float getSumIntensities() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.sumIntensities)).floatValue();
    }

    /**
     * Gets the sum of red channel image intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.sumIntensities + "Red")).floatValue();
    }

    /**
     * Gets the sum of green channel image intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.sumIntensities + "Green")).floatValue();
    }

    /**
     * Gets the sum of blue channel mage intensities defined by the VOI
     * @return
     */
    public float getSumIntensitiesB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.sumIntensities + "Blue")).floatValue();
    }




    /**
     * DOCUMENT ME!
     *
     * @param   aVOI  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public VOIStatisticalProperties getVOIProperties(VOI aVOI) {
        return propertyList.elementAt(selectedVOIset.indexOf(aVOI));
    }

    /**
     * Gets the volume of the VOI; return volume defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getVolume() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.volumeDescription)).floatValue();
    } // {return volume;}

    /**
     * Gets the coefficient of skewness of the pixel values in the VOI
     * @return
     */
    public float getSkewness() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.skewnessDescription)).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the red pixel values in the VOI
     * @return
     */
    public float getSkewnessR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.skewnessDescription + "Red")).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the green pixel values in the VOI
     * @return
     */
    public float getSkewnessG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.skewnessDescription + "Green")).floatValue();
    }

    /**
     * Gets the coefficient of skewness of the blue pixel values in the VOI
     * @return
     */
    public float getSkewnessB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.skewnessDescription + "Blue")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the pixel values in the VOI
     * @return
     */
    public float getKurtosis() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.kurtosisDescription)).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the red pixel values in the VOI
     * @return
     */
    public float getKurtosisR() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.kurtosisDescription + "Red")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the green pixel values in the VOI
     * @return
     */
    public float getKurtosisG() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.kurtosisDescription + "Green")).floatValue();
    }

    /**
     * Gets the coefficient of kurtosis of the blue pixel values in the VOI
     * @return
     */
    public float getKurtosisB() {
        return Float.valueOf(propertyList.firstElement().getProperty(VOIStatisticList.kurtosisDescription + "Blue")).floatValue();
    }

    protected int indexOf( String statistic )
    {
        for ( int i = 0; i < numberOfStatistics; i++ )
        {
            if ( statistic.equals( statisticDescription[i] ) )
            {
                return i;
            }
        }
        return 0;
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
                activeVOI = selectedVOIset.elementAt(i);
                if (!doOnlyActiveContours) {
                    activeVOI.setAllActive(false);
                    if (srcImage.getNDims() == 2) {
                        //Calc2D calcVOI = new Calc2D(activeVOI);
                        Calc34D calcVOI = new Calc34D(activeVOI);
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
                    for (int k = tempVOI.getCurves().size() - 1; k >= 0 ; k--) {
                        if (!tempVOI.getCurves().elementAt(k).isActive()) {
                            tempVOI.getCurves().remove(k);
                        }
                    }
                    tempVOI.setAllActive(false);

                    selectedVOIset.remove(i);
                    selectedVOIset.insertElementAt(tempVOI, i);
                    if (srcImage.getNDims() == 2) {
                        //Calc2D calcVOI = new Calc2D(tempVOI);
                        Calc34D calcVOI = new Calc34D(tempVOI);
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
     * Sets the list of selected statistics to calculate. Default is to calculate all statistics.
     * @param checkList
     */
    public void setSelectedStatistics( boolean[] checkList )
    {
        statsList = checkList;
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

    private class Calc34D implements Runnable {

        /** The VOI being used just for this calculation (may be a single curve of a single slice or entire VOI **/
        private VOI calcSelectedVOI;

        public Calc34D(VOI selectedVOI) {
            this.calcSelectedVOI = selectedVOI;
        }

        public void run() {
            long time = System.currentTimeMillis();
            float[] imgBuffer;
            double largestDistance = 0;
            String xStr, yStr, zStr;
            String unit2DStr = null;
            String unit3DStr = null;

            BitSet mask;
            VOIStatisticalProperties statProperty = getVOIProperties(calcSelectedVOI);


            int length = srcImage.getSliceSize();
            if (srcImage.isColorImage()) {
                length *= 4;
            }

            int zDim = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;
            FileInfoBase fileInfo = srcImage.getFileInfo()[zDim/2];

            try {
                imgBuffer = new float[length * zDim];
                int offset4D = 0;
                if ( ViewUserInterface.getReference().getFrameContainingImage(srcImage) != null )
                {
                    offset4D = imgBuffer.length *
                    ViewUserInterface.getReference().getFrameContainingImage(srcImage).getViewableTimeSlice();
                }

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

            float ignoreMin = calcSelectedVOI.getMinimumIgnore();
            float ignoreMax = calcSelectedVOI.getMaximumIgnore();

            if(distanceFlag) {
                long time2 = System.currentTimeMillis();
                largestDistance = calcSelectedVOI.calcLargestDistance(
                        srcImage.getFileInfo(0).getResolutions()[0],
                        srcImage.getFileInfo(0).getResolutions()[1],
                        srcImage.getFileInfo(0).getResolutions()[2]);
                System.out.println("Total time: "+(System.currentTimeMillis() - time2));
            }

            int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            if (xUnits != FileInfoBase.UNKNOWN_MEASURE) {
                xStr = "X " + FileInfoBase.getUnitsOfMeasureAbbrevStr(xUnits);    
            }
            else {
                xStr = "X ";
            }
            int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            if (yUnits != FileInfoBase.UNKNOWN_MEASURE) {
                yStr = "Y " + FileInfoBase.getUnitsOfMeasureAbbrevStr(yUnits);    
            }
            else {
                yStr = "Y ";
            }
            unit2DStr = xStr + "\t" + yStr;
            if (srcImage.getNDims() > 2) {
                int zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
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
                    (processType == PROCESS_PER_SLICE_AND_CONTOUR)) {

                Vector<VOIBase>[] sortedContoursZ = calcSelectedVOI.getSortedCurves( VOIBase.ZPLANE, zDim );
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
                        stats[q] = calcStatsPerContour( fileInfo, imgBuffer, sortedContoursZ[sortedZ].elementAt(q), 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                        if ( processType == PROCESS_PER_SLICE_AND_CONTOUR )
                        {
                            printStatsPerContour(  fileInfo, stats[q], statProperty, sortedZ, q );
                        }  
                        allStats.add( stats[q] );
                    }         
                    if ( showTotals )
                    {
                        printTotals( fileInfo, stats, statProperty, 
                                unit2DStr, unit3DStr, new String( sortedZ + ";" ), mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                    }
                }
                if ( showTotals )
                {
                    printTotals( fileInfo, allStats, statProperty, 
                            unit2DStr, unit3DStr, "Total", mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                }          
            } else if ( processType == PROCESS_PER_CONTOUR ) {

                Vector<VOIBase> contours = calcSelectedVOI.getCurves();
                ContourStats[] stats = new ContourStats[contours.size()];
                // since we're in a 3D image, contours.length is how many slices this VOI is on
                for (int q = 0; q < contours.size(); q++) {
                    stats[q] = calcStatsPerContour( fileInfo, imgBuffer, contours.elementAt(q), 
                            unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                    if ( processType == PROCESS_PER_CONTOUR )
                    {
                        printStatsPerContour( fileInfo, stats[q], statProperty, 0, q );
                    }
                }               
                if ( showTotals )
                {
                    printTotals( fileInfo, stats, statProperty, 
                            unit2DStr, unit3DStr, "Total", mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                }
                else
                {
                    printTotals( fileInfo, stats, statProperty, 
                            unit2DStr, unit3DStr, "", mask, imgBuffer, ignoreMin, ignoreMax, largestDistance );
                }
            }
            else
            {
                Vector<VOIBase> contours = calcSelectedVOI.getCurves();
                calcStatsTotal( contours, statProperty, fileInfo, unit2DStr, unit3DStr, ignoreMin, ignoreMax, largestDistance );
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
            long time = System.currentTimeMillis();
            contour.update();
            ContourStats stats = new ContourStats();
            
            contour.getGroup().setAllActive(false);
            contour.setActive(true);


            if ( statsList[ indexOf( quantityDescription ) ] || 
                    statsList[ indexOf( volumeDescription ) ] ||
                    statsList[ indexOf( areaDescription ) ] )
            {               
                stats.nVox = contour.getNumVoxels();
                stats.area = stats.nVox * (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1]);
                stats.volume = stats.area * fileInfo.getResolutions()[2]; 
                
                System.out.println("Time required to calculate "+quantityDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( perimeterDescription ) ] )
            {               
                stats.perimeter = contour.getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());                
                System.out.println("Time required to calculate "+perimeterDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( minIntensity ) ] ||
                    statsList[ indexOf( maxIntensity ) ] ||
                    statsList[ indexOf( avgIntensity ) ] ||
                    statsList[ indexOf( sumIntensities ) ] ||   

                    statsList[ indexOf( median ) ] || 
                    statsList[ indexOf( mode ) ] || 
                    statsList[ indexOf( modeCount ) ] ||

                    statsList[ indexOf( deviationDescription ) ] ||
                    statsList[ indexOf( skewnessDescription ) ] || 
                    statsList[ indexOf( kurtosisDescription ) ] ||
                    statsList[ indexOf( massCenterDescription ) ]  )
            {                   
                stats.nVox = contour.getNumVoxels();       
                if (srcImage.isColorImage()) {
                    ColorRGB kMin = new ColorRGB();
                    ColorRGB kMax = new ColorRGB();
                    ColorRGB kSum = new ColorRGB();

                    stats.valuesRGB = contour.calcRGBIntensity( srcImage, kMin, kMax, kSum,
                            ignoreMin, ignoreMax, rangeFlag );


                    stats.minIntenRed = kMin.R;
                    stats.minIntenGreen = kMin.G;
                    stats.minIntenBlue = kMin.B;

                    stats.maxIntenRed = kMax.R;
                    stats.maxIntenGreen = kMax.G;
                    stats.maxIntenBlue = kMax.B;

                    stats.sumR = kSum.R;
                    stats.sumG = kSum.G;
                    stats.sumB = kSum.B;
                    stats.avgIntenR = kSum.R/stats.nVox;
                    stats.avgIntenG = kSum.G/stats.nVox;
                    stats.avgIntenB = kSum.B/stats.nVox;                 
                }
                else
                {
                    Vector3f kMinMax = new Vector3f();
                    stats.values = contour.calcIntensity( srcImage, kMinMax, ignoreMin, ignoreMax, rangeFlag );
                    stats.minIntensity = kMinMax.X;
                    stats.maxIntensity = kMinMax.Y;
                    stats.sum = kMinMax.Z;
                    stats.avgInten = stats.sum/stats.nVox;
                }

                System.out.println("Time required to calculate "+minIntensity+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();

                if ( statsList[ indexOf( median ) ] || statsList[ indexOf( mode ) ] || statsList[ indexOf( modeCount ) ] )
                {               
                    if (srcImage.isColorImage()) {
                        getMedianStatisticsRGB( stats );
                    }
                    else
                    {
                        getMedianStatistics( stats );
                    }
                    System.out.println("Time required to calculate "+median+": "+(System.currentTimeMillis() - time));
                    time = System.currentTimeMillis();
                }

                if ( statsList[ indexOf( deviationDescription ) ] ||
                        statsList[ indexOf( skewnessDescription ) ] || 
                        statsList[ indexOf( kurtosisDescription ) ] ||
                        statsList[ indexOf( massCenterDescription ) ]    )
                {       
                    if (srcImage.isColorImage())
                    {
                        getStdSkewStatisticsRGB(stats, contour.getMaskPositions(), 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                    }
                    else
                    {                        
                        getStdSkewStatistics(stats, contour.getMaskPositions(), 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                    }
                    System.out.println("Time required to calculate "+deviationDescription+": "+(System.currentTimeMillis() - time));
                    time = System.currentTimeMillis();
                }
                
            }
            if ( statsList[ indexOf( geometricCenterDescription ) ] )
            {               
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
                stats.gCenterString = new String(comStr);
                System.out.println("Time required to calculate "+geometricCenterDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( axisDescription ) ] ||
                    statsList[ indexOf( eccentricityDescription ) ] ||
                    statsList[ indexOf( majorAxisDescription ) ] ||
                    statsList[ indexOf( minorAxisDescription ) ] )
            {               
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
                System.out.println("Time required to calculate "+axisDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( largestSliceDistanceDescription ) ] )
            {               
                stats.largestContourDistance = ((VOIContour) (contour)).calcLargestSliceDistance(
                        srcImage.getFileInfo(0).getResolutions()[0], srcImage.getFileInfo(0).getResolutions()[1]);

                System.out.println("Time required to calculate "+largestSliceDistanceDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
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
            /*
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
*/

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
            statProperty.setProperty(VOIStatisticList.axisDescription + end, nf.format(stats[0].PAxis));
            statProperty.setProperty(VOIStatisticList.eccentricityDescription + end, nf.format(stats[0].Ecc));
            statProperty.setProperty(VOIStatisticList.majorAxisDescription + end, nf.format(stats[0].MajorAxis));
            statProperty.setProperty(VOIStatisticList.minorAxisDescription + end, nf.format(stats[0].MinorAxis));

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

        private void calcStatsTotal( Vector<VOIBase> contours, VOIStatisticalProperties statProperty,
                FileInfoBase fileInfo, 
                String unit2DStr, String unit3DStr, float ignoreMin, float ignoreMax, double largestDistance )
        {
            ContourStats stats = new ContourStats();
            long time = System.currentTimeMillis();

            if ( statsList[ indexOf( quantityDescription ) ] || 
                    statsList[ indexOf( volumeDescription ) ] ||
                    statsList[ indexOf( areaDescription ) ] )
            {    
                stats.nVox = 0;
                stats.area = 0;
                stats.volume = 0;
                for ( int i = 0; i < contours.size(); i++ )
                {
                    int nVox = contours.elementAt(i).getNumVoxels();
                    stats.nVox += nVox;
                    stats.area += nVox * (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1]);
                    stats.volume += nVox * (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1] * fileInfo.getResolutions()[2]);
                }
                statProperty.setProperty(VOIStatisticList.areaDescription, nf.format(stats.area));
                statProperty.setProperty(VOIStatisticList.volumeDescription, nf.format(stats.volume));
                statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(stats.nVox));            
                System.out.println("Time required to calculate "+quantityDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( perimeterDescription ) ] )
            {           
                stats.perimeter = 0;
                for ( int i = 0; i < contours.size(); i++ )
                {                   
                    stats.perimeter += contours.elementAt(i).getLengthPtToPt(srcImage.getFileInfo(0).getResolutions());
                }
                statProperty.setProperty(VOIStatisticList.perimeterDescription, nf.format(stats.perimeter));      
                System.out.println("Time required to calculate "+perimeterDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( minIntensity ) ] ||
                    statsList[ indexOf( maxIntensity ) ] ||
                    statsList[ indexOf( avgIntensity ) ] ||
                    statsList[ indexOf( sumIntensities ) ] ||   

                    statsList[ indexOf( median ) ] || 
                    statsList[ indexOf( mode ) ] || 
                    statsList[ indexOf( modeCount ) ] ||

                    statsList[ indexOf( deviationDescription ) ] ||
                    statsList[ indexOf( skewnessDescription ) ] || 
                    statsList[ indexOf( kurtosisDescription ) ] ||
                    statsList[ indexOf( massCenterDescription ) ]  )
            {                   

                stats.nVox = 0;      
                stats.values = new Vector<Float>();
                stats.valuesRGB = new Vector<ColorRGB>();
                for ( int i = 0; i < contours.size(); i++ )
                {
                    int nVox = contours.elementAt(i).getNumVoxels();
                    stats.nVox += nVox;       
                    if (srcImage.isColorImage()) {
                        ColorRGB kMin = new ColorRGB();
                        ColorRGB kMax = new ColorRGB();
                        ColorRGB kSum = new ColorRGB();

                        stats.valuesRGB.addAll( contours.elementAt(i).calcRGBIntensity( srcImage, kMin, kMax, kSum,
                                ignoreMin, ignoreMax, rangeFlag ) );
                        if ( i == 0 )
                        {
                            stats.minIntenRed = kMin.R;
                            stats.minIntenGreen = kMin.G;
                            stats.minIntenBlue = kMin.B;

                            stats.maxIntenRed = kMax.R;
                            stats.maxIntenGreen = kMax.G;
                            stats.maxIntenBlue = kMax.B;
                        }
                        
                        stats.minIntenRed = Math.min( stats.minIntenRed, kMin.R );
                        stats.minIntenGreen = Math.min( stats.minIntenGreen, kMin.G );
                        stats.minIntenBlue = Math.min( stats.minIntenBlue, kMin.B );

                        stats.maxIntenRed = Math.max( stats.minIntenRed, kMax.R );
                        stats.maxIntenGreen = Math.max( stats.minIntenGreen, kMax.G );
                        stats.maxIntenBlue = Math.max( stats.minIntenBlue, kMax.B );

                        stats.sumR += kSum.R;
                        stats.sumG += kSum.G;
                        stats.sumB += kSum.B;           
                    }
                    else
                    {
                        Vector3f kMinMax = new Vector3f();
                        stats.values.addAll( contours.elementAt(i).calcIntensity( srcImage, kMinMax, ignoreMin, ignoreMax, rangeFlag ) );
                        if ( i == 0 )
                        {
                            stats.minIntensity = kMinMax.X;
                            stats.maxIntensity = kMinMax.Y; 
                        }
                        
                        stats.minIntensity = Math.min( stats.minIntensity, kMinMax.X );
                        stats.maxIntensity = Math.max( stats.maxIntensity, kMinMax.Y );
                        stats.sum += kMinMax.Z;
                        
                    }
                }      


                statProperty.setProperty(VOIStatisticList.quantityDescription, nf.format(stats.nVox));
                if (srcImage.isColorImage()) {

                    stats.avgIntenR = stats.sumR/stats.nVox;
                    stats.avgIntenG = stats.sumG/stats.nVox;
                    stats.avgIntenB = stats.sumB/stats.nVox;
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Red", nf.format(stats.minIntenRed));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Red", nf.format(stats.maxIntenRed));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Green", nf.format(stats.minIntenGreen));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Green", nf.format(stats.maxIntenGreen));
                    statProperty.setProperty(VOIStatisticList.minIntensity + "Blue", nf.format(stats.minIntenBlue));
                    statProperty.setProperty(VOIStatisticList.maxIntensity + "Blue", nf.format(stats.maxIntenBlue));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Red", nf.format(stats.avgIntenR));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Green", nf.format(stats.avgIntenG));
                    statProperty.setProperty(VOIStatisticList.avgIntensity + "Blue", nf.format(stats.avgIntenB));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Red", nf.format(stats.sumR));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Green", nf.format(stats.sumG));
                    statProperty.setProperty(VOIStatisticList.sumIntensities + "Blue", nf.format(stats.sumB));
                }
                else
                {
                    stats.avgInten = stats.sum/stats.nVox;
                    statProperty.setProperty(VOIStatisticList.minIntensity, nf.format(stats.minIntensity));
                    statProperty.setProperty(VOIStatisticList.maxIntensity, nf.format(stats.maxIntensity));
                    statProperty.setProperty(VOIStatisticList.avgIntensity, nf.format(stats.avgInten));
                    statProperty.setProperty(VOIStatisticList.sumIntensities, nf.format(stats.sum));
                }
                
                System.out.println("Time required to calculate "+minIntensity+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
                
                if ( statsList[ indexOf( median ) ] || statsList[ indexOf( mode ) ] || statsList[ indexOf( modeCount ) ] )
                {               
                    if (srcImage.isColorImage()) {
                        getMedianStatisticsRGB( stats );

                        statProperty.setProperty(VOIStatisticList.mode + "Red", nf.format(stats.modeR));
                        statProperty.setProperty(VOIStatisticList.mode + "Green", nf.format(stats.modeG));
                        statProperty.setProperty(VOIStatisticList.mode + "Blue", nf.format(stats.modeB));

                        statProperty.setProperty(VOIStatisticList.modeCount + "Red", nf.format(stats.maxCountR));
                        statProperty.setProperty(VOIStatisticList.modeCount + "Green", nf.format(stats.maxCountG));
                        statProperty.setProperty(VOIStatisticList.modeCount + "Blue", nf.format(stats.maxCountB));

                        statProperty.setProperty(VOIStatisticList.median + "Red", nf.format(stats.medianR));
                        statProperty.setProperty(VOIStatisticList.median + "Green", nf.format(stats.medianG));
                        statProperty.setProperty(VOIStatisticList.median + "Blue", nf.format(stats.medianB));
                    }
                    else
                    {
                        getMedianStatistics( stats );
                        statProperty.setProperty(VOIStatisticList.mode, nf.format(stats.mode));
                        statProperty.setProperty(VOIStatisticList.modeCount, nf.format(stats.maxCount));
                        statProperty.setProperty(VOIStatisticList.median, nf.format(stats.median));
                    }
                    System.out.println("Time required to calculate "+median+": "+(System.currentTimeMillis() - time));
                    time = System.currentTimeMillis();
                    
                }

                if ( statsList[ indexOf( deviationDescription ) ] ||
                        statsList[ indexOf( skewnessDescription ) ] || 
                        statsList[ indexOf( kurtosisDescription ) ] ||
                        statsList[ indexOf( massCenterDescription ) ]    )
                {       
                    Vector<Vector3f> kPositions = new Vector<Vector3f>();
                    for ( int i = 0; i < contours.size(); i++ )
                    {
                        kPositions.addAll( contours.elementAt(i).getMaskPositions() );
                    }
                    if (srcImage.isColorImage())
                    {
                        getStdSkewStatisticsRGB(stats, kPositions, 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                        
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Red", nf.format(stats.stdDevR));
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Green", nf.format(stats.stdDevG));
                        statProperty.setProperty(VOIStatisticList.deviationDescription + "Blue", nf.format(stats.stdDevB));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Red", nf.format(stats.skewnessR));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Green", nf.format(stats.skewnessG));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription + "Blue", nf.format(stats.skewnessB));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Red", nf.format(stats.kurtosisR));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Green", nf.format(stats.kurtosisG));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription + "Blue", nf.format(stats.kurtosisB));
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Red", stats.massCenterDescriptionR);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Green", stats.massCenterDescriptionG);
                        statProperty.setProperty(VOIStatisticList.massCenterDescription + "Blue", stats.massCenterDescriptionB);
                    }
                    else
                    {                        
                        getStdSkewStatistics(stats, kPositions, 
                                unit2DStr, unit3DStr, ignoreMin, ignoreMax);
                        
                        statProperty.setProperty(VOIStatisticList.deviationDescription, nf.format(stats.stdDev));
                        statProperty.setProperty(VOIStatisticList.skewnessDescription, nf.format(stats.skewness));
                        statProperty.setProperty(VOIStatisticList.kurtosisDescription , nf.format(stats.kurtosis));
                        statProperty.setProperty(VOIStatisticList.massCenterDescription, stats.massCenterDescription);
                    }
                    System.out.println("Time required to calculate "+deviationDescription+": "+(System.currentTimeMillis() - time));
                    time = System.currentTimeMillis();
                }

            }
            if ( statsList[ indexOf( geometricCenterDescription ) ] )
            {               
                Vector3f selectedCOM = calcSelectedVOI.getGeometricCenter();
                selectedCOM.X *= srcImage.getFileInfo(0).getResolutions()[0];
                selectedCOM.Y *= srcImage.getFileInfo(0).getResolutions()[1];
                String unitStr = unit2DStr + "\tZ";

                if (srcImage.getNDims() > 2) {
                    selectedCOM.Z *= srcImage.getFileInfo(0).getResolutions()[2];
                    unitStr = unit3DStr;
                }


                String comStr = unitStr + "\n\t\t" + nf.format(selectedCOM.X) + "\t" + nf.format(selectedCOM.Y) + "\t" +
                nf.format(selectedCOM.Z);
                comStr = addScannerLabels(comStr, selectedCOM);
                stats.gCenterString = new String(comStr);
                statProperty.setProperty(VOIStatisticList.geometricCenterDescription, stats.gCenterString);
                System.out.println("Time required to calculate "+geometricCenterDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( largestSliceDistanceDescription ) ] )
            {               
                for ( int i = 0; i < contours.size(); i++ )
                {
                    if ( i == 0 )
                    {
                        stats.largestContourDistance = ((VOIContour) (contours.elementAt(i))).calcLargestSliceDistance(
                                srcImage.getFileInfo(0).getResolutions()[0], srcImage.getFileInfo(0).getResolutions()[1]);
                    }
                    else
                    {
                        stats.largestContourDistance = Math.max( stats.largestContourDistance, 
                                ((VOIContour) (contours.elementAt(i))).calcLargestSliceDistance(
                                        srcImage.getFileInfo(0).getResolutions()[0], srcImage.getFileInfo(0).getResolutions()[1]));
                    }
                }
                statProperty.setProperty(VOIStatisticList.largestSliceDistanceDescription, nf.format(stats.largestContourDistance));
                

                System.out.println("Time required to calculate "+largestSliceDistanceDescription+": "+(System.currentTimeMillis() - time));
                time = System.currentTimeMillis();
            }
            if ( statsList[ indexOf( largestDistanceDescription ) ] )
            {               
                stats.largestContourDistance = largestDistance;
                statProperty.setProperty(VOIStatisticList.largestDistanceDescription, nf.format(stats.largestContourDistance));
                
            }


        }



        public void getMedianStatisticsRGB( ContourStats stats )
        {
            int nVox = stats.valuesRGB.size();
            float[] buffR = new float[nVox];
            float[] buffG = new float[nVox];
            float[] buffB = new float[nVox];
            for ( int i = 0; i < stats.valuesRGB.size(); i++ )
            {
                buffR[i] = stats.valuesRGB.elementAt(i).R;
                buffG[i] = stats.valuesRGB.elementAt(i).G;
                buffB[i] = stats.valuesRGB.elementAt(i).B;
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

        public void getMedianStatistics( ContourStats stats )
        {
            int nVox = stats.values.size();
            float[] buff = new float[nVox];
            for ( int i = 0; i < stats.values.size(); i++ )
            {
                buff[i] = stats.values.elementAt(i).floatValue();
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
            stats.median = median;
            stats.mode = mode;
            stats.maxCount = maxCount;
        }

        public void getStdSkewStatisticsRGB( ContourStats stats, Vector<Vector3f> kPositions, 
                String unit2DStr, String unit3DStr, float ignoreMin, float ignoreMax )
        {

            // calculate standard deviation, coefficient of skewness, and coefficient of kurtosis

            double sumR2 = 0, sumG2 = 0, sumB2 = 0;
            double sumR3 = 0, sumG3 = 0, sumB3 = 0;
            double sumR4 = 0, sumG4 = 0, sumB4 = 0;
            sumR2 = sumG2 = sumB2 = sumR3 = sumG3 = sumB3 = sumR4 = sumG4 = sumB4 = 0;
            // Calculate centers of mass
            double xMassR, yMassR, zMassR, xMassG, yMassG, zMassG, xMassB, yMassB, zMassB;
            xMassR = yMassR = zMassR = xMassG = yMassG = zMassG = xMassB = yMassB = zMassB = 0;

            int cnt = 0;

            double diffR, diffG, diffB;
            double R2, R3, R4;
            double G2, G3, G4;
            double B2, B3, B4;
            for ( int i = 0; i < kPositions.size(); i++ )
            {
                Vector3f kPos = kPositions.elementAt(i);
                int x = (int)kPos.X;
                int y = (int)kPos.Y;
                int z = (int)kPos.Z;


                float fR = srcImage.getFloatC(x,y,z,1);
                float fG = srcImage.getFloatC(x,y,z,2);
                float fB = srcImage.getFloatC(x,y,z,3);


                if ( !inRange(ignoreMin, ignoreMax, fR) &&
                        !inRange(ignoreMin, ignoreMax, fG) &&
                        !inRange(ignoreMin, ignoreMax, fB)) {
                    xMassR += x * fR;
                    yMassR += y * fG;
                    zMassR += z * fB;
                    diffR = fR - stats.avgIntenR;
                    R2 = diffR * diffR;
                    sumR2 += R2;
                    R3 = R2 * diffR;
                    sumR3 += R3;
                    R4 = R3 * diffR;
                    sumR4 += R4;
                    xMassG += x * fG;
                    yMassG += y * fG;
                    zMassG += z * fG;
                    diffG = fG - stats.avgIntenG;
                    G2 = diffG * diffG;
                    sumG2 += G2;
                    G3 = G2 * diffG;
                    sumG3 += G3;
                    G4 = G3 * diffG;
                    sumG4 += G4;
                    xMassB += x * fB;
                    yMassB += y * fB;
                    zMassB += z * fB;
                    diffB = fB - stats.avgIntenB;
                    B2 = diffB * diffB;
                    sumB2 += B2;
                    B3 = B2 * diffB;
                    sumB3 += B3;
                    B4 = B3 * diffB;
                    sumB4 += B4;
                    cnt++;
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
            double xCOMR = xMassR * srcImage.getFileInfo(0).getResolutions()[0]/stats.sumR;
            double yCOMR = yMassR * srcImage.getFileInfo(0).getResolutions()[1]/stats.sumR;
            double zCOMR = zMassR/stats.sumR;
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
            stats.massCenterDescriptionR = new String(comStr);

            double xCOMG = xMassG * srcImage.getFileInfo(0).getResolutions()[0]/stats.sumG;
            double yCOMG = yMassG * srcImage.getFileInfo(0).getResolutions()[1]/stats.sumG;
            double zCOMG= zMassG/stats.sumG;
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

            double xCOMB = xMassB * srcImage.getFileInfo(0).getResolutions()[0]/stats.sumB;
            double yCOMB = yMassB * srcImage.getFileInfo(0).getResolutions()[1]/stats.sumB;
            double zCOMB = zMassB/stats.sumB;
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

        public void getStdSkewStatistics( ContourStats stats, Vector<Vector3f> kPositions,
                String unit2DStr, String unit3DStr, float ignoreMin, float ignoreMax )
        {

            double sum2 = 0;
            double sum3 = 0;
            double sum4 = 0;
            // Calculate centers of mass
            double xMass = 0, yMass = 0, zMass = 0;

            int cnt = 0;
            
            double diff;
            double s2, s3, s4;
            for ( int i = 0; i < kPositions.size(); i++ )
            {
                Vector3f kPos = kPositions.elementAt(i);
                int x = (int)kPos.X;
                int y = (int)kPos.Y;
                int z = (int)kPos.Z;

                float fVal = srcImage.getFloat(x,y,z);

                if (!inRange(ignoreMin, ignoreMax, fVal)) {
                    xMass += x * fVal;
                    yMass += y * fVal;
                    zMass += z * fVal;
                    diff = fVal - stats.avgInten;
                    s2 = diff * diff;
                    sum2 += s2;
                    s3 = s2 * diff;
                    sum3 += s3;
                    s4 = s3 * diff;
                    sum4 += s4;
                    cnt++;
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
            double xCOM = xMass * srcImage.getFileInfo(0).getResolutions()[0]/stats.sum;
            double yCOM = yMass * srcImage.getFileInfo(0).getResolutions()[1]/stats.sum;
            double zCOM = zMass/stats.sum;
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
            stats.massCenterDescription = new String(comStr);

            stats.massI = new double[]{ xMass, yMass, zMass };
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
    protected boolean inRange(float ignoreMin, float ignoreMax, float num) {

        if (rangeFlag == JDialogVOIStatistics.NO_RANGE) {
            return false;
        } else if (rangeFlag == JDialogVOIStatistics.BETWEEN) {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                return true;
            }
            return false;
        } else if (rangeFlag == JDialogVOIStatistics.OUTSIDE) {

            if ((num <= ignoreMin) || (num >= ignoreMax)) {
                return true;
            }
            return false;
        } else {

            if ((num >= ignoreMin) && (num <= ignoreMax)) {
                System.out.println(" min  = " + ignoreMax + " max = " + ignoreMax);

                return true;
            }
            return false;
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  leadBase  DOCUMENT ME!
     */
    protected String addScannerLabels(String baseString, Vector3f currentPt) {

        String[] positions = null;
        Vector3f scaledPt;

        if ((srcImage.getFileInfo(0).getOrigin()[0] != 0) || (srcImage.getFileInfo(0).getOrigin()[1] != 0) ||
                (srcImage.getFileInfo(0).getOrigin()[2] != 0)) {

            scaledPt = new Vector3f();

            float xRes = srcImage.getFileInfo(0).getResolutions().length > 0 ? srcImage.getFileInfo(0).getResolutions()[0] : 1;
            float yRes = srcImage.getFileInfo(0).getResolutions().length > 1 ? srcImage.getFileInfo(0).getResolutions()[1] : 1;
            float zRes = srcImage.getFileInfo(0).getResolutions().length > 2 ? srcImage.getFileInfo(0).getResolutions()[2] : 1;

            scaledPt.X = (currentPt.X/xRes);
            scaledPt.Y = (currentPt.Y/yRes);
            scaledPt.Z = (currentPt.Z/zRes);


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
