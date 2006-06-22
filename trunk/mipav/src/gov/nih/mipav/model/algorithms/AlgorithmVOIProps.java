package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.util.*;
import javax.swing.*;
import java.text.*;


/**
 * This class calculates a properties of an image defined by a VOI. Attributes include: volume, area, number of pixels,
 * center of mass, average pixel intensity, standard deviation of intensity, eccentricity, and principalAxis.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/algorithms/AlgorithmVOIProps.java $ $Revision: 44 $ $Date:
 *           12/19/05 4:51p $</p>
 */

public class AlgorithmVOIProps extends AlgorithmBase implements VOIStatisticList {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The VOI on which to perform the calculations */
    private VOI activeVOI;

    /** Whether to perform calculations on per-contour basis */
    private boolean perContour = false;

    /** Whether to perform calculations on per-slice basis */
    private boolean perSlice = false;

    /** Vector to hold all properties calculated within the algorithm for later access */
    private Vector propertyList;

    /** Whether or not to exclude a range of values */
    private int rangeFlag;

    /** Vector of all Active VOIs */
    private ViewVOIVector selectedVOIset;

    /** Whether or not to show totals for each calculation */
    private boolean showTotals = false;

    /** both of these have graphic resources, but useful information and references. */
    private ModelImage srcImage; // references to the VOI curves we wlll act on.

    /** Panel determines which statistics to calculate */
    private JPanelStatisticsList statisticList; // collection of items to perform stats on

    /** Formatting for float values into strings */
    private NumberFormat nf;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor. sets the source image of the algorithm, and presets the algorithm to calculate properties of 3D
     * images as a volume of interest, rather than by slice.
     *
     * @param  srcImg  image model that contain the VOI
     */
    public AlgorithmVOIProps(ModelImage srcImg) {
        this(srcImg, false);
    }

    /**
     * constructor.
     *
     * @param  srcImg        image model that contain the VOI
     * @param  sliceBySlice  perform the property calculations for each slice, rather than for whole volume of interest
     *                       (VOI).
     */
    public AlgorithmVOIProps(ModelImage srcImg, boolean sliceBySlice) {
        this(srcImg, null, sliceBySlice, 0);
    }

    /**
     * constructor. note that if there are no VOIs to act on, this constructor returns quietly.
     *
     * @param  srcImg        image model that contain the VOI
     * @param  list          list of items to perform the statistics operations on.
     * @param  sliceBySlice  perform the property calculations for each slice, rather than for whole volume of interest
     *                       (VOI).
     * @param  rangeFlag     DOCUMENT ME!
     */
    public AlgorithmVOIProps(ModelImage srcImg, JPanelStatisticsList list, boolean sliceBySlice, int rangeFlag) {

        nf = NumberFormat.getInstance();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        this.rangeFlag = rangeFlag;
        this.srcImage = srcImg;
        this.statisticList = list;
        perSlice = (srcImage.getNDims() == 2) ? true : sliceBySlice;
        selectedVOIset = getActiveVOIs();

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
        statisticList = null;
        super.finalize();
    }

    /**
     * Gets the area of the VOI; return area defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getArea() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.areaDescription)).floatValue();
    } // {return area;}

    /**
     * Gets the average intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgInten() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.avgIntensity)).floatValue();
    } // {return avgInten;}

    /**
     * Gets the average intensity of the Blue channel of VOI return average intensity of the Blue channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenB() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.avgIntensity +
                                                                                                  "Blue")).floatValue();
    } // {return avgIntenB;}

    /**
     * Gets the average intensity of the Green channel of VOI return average intensity of the Green channel of image
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenG() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.avgIntensity +
                                                                                                  "Green")).floatValue();
    } // {return avgIntenG;}

    /**
     * Gets the average intensity of the Red channel of VOI return average intensity of the Red channel of image defined
     * by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getAvgIntenR() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.avgIntensity +
                                                                                                  "Red")).floatValue();
    } // {return avgIntenR;}

    /**
     * Gets the the center of mass of the VOI ; return center of mass defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterOfMass() {
        return ((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.centerDescription);
    } // {return cMass;}

    /**
     * Gets the eccentricity of the VOI: 1 = line, 0 = circle; return eccentricity of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getEccentricity() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.eccentricityDescription)).floatValue();
    } // {return eccentricity;}

    /**
     * Gets the major axis of VOI (only valid for 2D object); return major axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMajorAxis() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.majorAxisDescription)).floatValue();
    } // {return majorAxis;}

    /**
     * Gets the maximum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensity() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.maxIntensity)).floatValue();
    } // {return maxIntensity;}

    /**
     * Gets the maximum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityBlue() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.maxIntensity +
                                                                                                  "Blue")).floatValue();
    } // {return maxIntenBlue;}

    /**
     * Gets the maximum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityGreen() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.maxIntensity +
                                                                                                  "Green")).floatValue();
    } // {return maxIntenGreen;}

    /**
     * Gets the maximum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxIntensityRed() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.maxIntensity +
                                                                                                  "Red")).floatValue();
    } // {return maxIntenRed;}

    /**
     * Gets the greatest distance between any two point of the VOI return distance.
     *
     * @return  DOCUMENT ME!
     */
    public double getMaxWidth() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.maxWidthDescription)).floatValue();
    } // {return maxDistance;}

    /**
     * Gets the minimum intensity of the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensity() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.minIntensity)).floatValue();
    } // {return minIntensity;}

    /**
     * Gets the minimum intensity of the Blue channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityBlue() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.minIntensity +
                                                                                                  "Blue")).floatValue();
    } // {return minIntenBlue;}

    /**
     * Gets the minimum intensity of the Green channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityGreen() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.minIntensity +
                                                                                                  "Green")).floatValue();
    } // {return minIntenGreen;}

    /**
     * Gets the minimum intensity of the Red channel for the VOI return average intensity of image defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinIntensityRed() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.minIntensity +
                                                                                                  "Red")).floatValue();
    } // {return minIntenRed;}

    /**
     * Gets the minor axis of VOI (only valid for 2D object); return minor axis length of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinorAxis() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.minorAxisDescription)).floatValue();
    } // {return minorAxis;}

    /**
     * Gets the the number of pixels return number of pixels defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public int getNVoxels() {
        return Integer.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.quantityDescription)).intValue();
    } // {return nVox;}

    /**
     * Gets the perimeter of the VOI (in terms of res).
     *
     * @return  String perimeter string
     */
    public String getPerimeter() {
        return ((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.perimeterDescription);
    } // {return perimeter;}

    /**
     * Gets the principle axis of VOI (only valid for 2D object); return pricipal axis angle of the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getPrincipalAxis() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.axisDescription)).floatValue();
    } // {return principalAxis;}

    /**
     * Gets the standard deviation of image intensities return standard deviation of image intensities defined by the
     * VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDev() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.deviationDescription)).floatValue();
    } // {return stdDev;}

    /**
     * Gets the get standard deviation of image intensities (blue channel) return standard deviation of image
     * intensities defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevB() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.deviationDescription +
                                                                                                  "Blue")).floatValue();
    } // {return stdDevB;}

    /**
     * Gets the standard deviation of image intensities (green channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevG() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.deviationDescription +
                                                                                                  "Green")).floatValue();
    } // {return stdDevG;}

    /**
     * Gets the standard deviation of image intensities (red channel) return standard deviation of image intensities
     * defined by the VOI.
     *
     * @return  DOCUMENT ME!
     */
    public float getStdDevR() {
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.deviationDescription +
                                                                                                  "Red")).floatValue();
    } // {return stdDevR;}

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
        return Float.valueOf(((VOIStatisticalProperties) propertyList.firstElement()).getProperty(VOIStatisticalProperties.volumeDescription)).floatValue();
    } // {return volume;}

    /**
     * reports if this algorithm is performing property calculations for each slice, rather than on the volume of
     * interest (VOI) as a whole.
     *
     * @return  boolean
     */
    public boolean isCalculatedByContour() {
        return perContour;
    }

    /**
     * reports if this algorithm is performing property calculations for each slice, rather than on the volume of
     * interest (VOI) as a whole.
     *
     * @return  boolean
     */
    public boolean isCalculatedBySlice() {
        return perSlice;
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

        try {

            // stats for 2D images are by defined to work only on the VOIs in the slice
            // find the VOI to calculate for (need not be active!!)
            // System.out.println("algoVOIprops n VOIs = " + selectedVOIset.size());
            for (int i = 0; i < selectedVOIset.size(); i++) {
                activeVOI = (VOI) selectedVOIset.elementAt(i);
                activeVOI.setAllActive(false);

                if (srcImage.getNDims() == 2) {
                    calc2D(activeVOI);
                } else if (srcImage.getNDims() > 2) {
                    calc34D(activeVOI);
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
    }

    /**
     * sets the list of items to calculate statistics for.
     *
     * @param  list  DOCUMENT ME!
     */
    public void setCalculationList(JPanelStatisticsList list) {
        statisticList = list;
    }

    /**
     * Sets the String float formatter to trim to numDecimal number of decimals
     * @param numDecimal int number of decimals
     * @param doForce boolean force numDecimal or allow zero
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
     * tells the algorithm to perform property calculations for each contour, rather than for the whole volume of
     * interest (VOI).
     *
     * @param  byContour  DOCUMENT ME!
     */
    public void setPerContour(boolean byContour) {
        perContour = byContour;
    }

    /**
     * tells the algorithm to perform property calculations for each slice, rather than for the whole volume of interest
     * (VOI). This method is not applicable to 2D images, and will merely reset the algorithm to act on each slice for
     * 2D images.
     *
     * @param  bySlices  DOCUMENT ME!
     */
    public void setPerSlice(boolean bySlices) {
        perSlice = (srcImage.getNDims() == 2) ? true : bySlices;
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
            selectedVOIset.addElement(list.getElementAt(i));
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

    /**
     * Calculates the image properties in an region defined by the VOI.
     *
     * @param  selectedVOI  DOCUMENT ME!
     */
    private void calc2D(VOI selectedVOI) {
        float minIntensity = Float.MAX_VALUE, totalMinIntensity = Float.MAX_VALUE;
        float maxIntensity = -Float.MAX_VALUE, totalMaxIntensity = -Float.MAX_VALUE;
        float minIntenRed = Float.MAX_VALUE, totalMinIntenRed = Float.MAX_VALUE;
        float maxIntenRed = -Float.MAX_VALUE, totalMaxIntenRed = -Float.MAX_VALUE;
        float minIntenGreen = Float.MAX_VALUE, totalMinIntenGreen = Float.MAX_VALUE;
        float maxIntenGreen = -Float.MAX_VALUE, totalMaxIntenGreen = -Float.MAX_VALUE;
        float minIntenBlue = Float.MAX_VALUE, totalMinIntenBlue = Float.MAX_VALUE;
        float maxIntenBlue = -Float.MAX_VALUE, totalMaxIntenBlue = -Float.MAX_VALUE;
        float avgInten = 0;
        float avgIntenR = 0;
        float avgIntenG = 0;
        float avgIntenB = 0;
        float stdDev = 0, stdDevR = 0, stdDevG = 0, stdDevB = 0;
        float totalStdDev = 0, totalStdDevR = 0, totalStdDevG = 0, totalStdDevB = 0;
        float volume = 0;
        float sum = 0, sumR = 0, sumG = 0, sumB = 0, area = 0;
        float totalSum = 0, totalSumR = 0, totalSumG = 0, totalSumB = 0, totalArea = 0;
        float totalAxis = 0, totalEcc = 0;
        float totalMajorAxis = 0;
        float totalMinorAxis = 0;
        int nVox = 0, totalNVox = 0;
        Point3Df cMass = new Point3Df(0, 0, 0);
        Point3Df totalC = new Point3Df(0, 0, 0);
        float[] imgBuffer;
        float[] tmpPAxis = null;
        float[] tmpEcc = null;
        float[] tmpMajorAxis = null;
        float[] tmpMinorAxis = null;
        float totalPerimeter = 0f;

        int length;

        Vector[] contours;
        BitSet mask;
        VOIStatisticalProperties statProperty = getVOIProperties(selectedVOI);

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

        float ignoreMin = selectedVOI.getMinimumIgnore();
        float ignoreMax = selectedVOI.getMaximumIgnore();
        float perimeter = 0f;

        contours = selectedVOI.getCurves();

        if (perContour == true) {
            // since we're in a 2D image, contours.length should = 1

            for (int q = 0; q < contours.length; q++) {

                // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                for (int r = 0; r < contours[q].size(); r++) {
                    sum = 0;
                    sumR = 0;
                    sumG = 0;
                    sumB = 0;
                    nVox = 0;

                    ((VOIContour) (contours[q].elementAt(r))).secondOrderAttributes(srcImage.getExtents()[0],
                                                                                    srcImage.getExtents()[1],
                                                                                    srcImage.getFileInfo(0).getResolutions()[0],
                                                                                    srcImage.getFileInfo(0).getResolutions()[1],
                                                                                    srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                                                                    srcImage.getFileInfo(0).getUnitsOfMeasure()[1],
                                                                                    tmpPAxis, tmpEcc, tmpMajorAxis,
                                                                                    tmpMinorAxis);
                    statProperty.setProperty(statProperty.axisDescription + "0;" + r, nf.format(tmpPAxis[0]));
                    statProperty.setProperty(statProperty.eccentricityDescription + "0;" + r,
                                             nf.format(tmpEcc[0]));
                    statProperty.setProperty(statProperty.majorAxisDescription + "0;" + r,
                                             nf.format(tmpMajorAxis[0]));
                    statProperty.setProperty(statProperty.minorAxisDescription + "0;" + r,
                                             nf.format(tmpMinorAxis[0]));

                    Point3Df centerOfMass = ((VOIContour) (contours[q].elementAt(r))).getCenterOfMass();
                    centerOfMass.x *= srcImage.getFileInfo(0).getResolutions()[0];
                    centerOfMass.y *= srcImage.getFileInfo(0).getResolutions()[1];
                    if (srcImage.getNDims() > 2) {
                        centerOfMass.z *= srcImage.getFileInfo(0).getResolutions()[2];
                    }


                    String comStr = nf.format(centerOfMass.x) + "\t" + nf.format(centerOfMass.y) + "\t" + nf.format(centerOfMass.z);
                    statProperty.setProperty(statProperty.centerDescription + "0;" + r, comStr);


                    totalEcc += tmpEcc[0];
                    totalAxis += tmpPAxis[0];
                    totalMajorAxis += tmpMajorAxis[0];
                    totalMinorAxis += tmpMinorAxis[0];
                    perimeter = ((VOIContour) (contours[q].elementAt(r))).calcPerimeter(srcImage.getFileInfo(0).getResolutions()[0],
                                                                                        srcImage.getFileInfo(0).getResolutions()[1]);
                    totalPerimeter += perimeter;

                    totalC = selectedVOI.getCenterOfMass();

                    ((VOIContour) (contours[q].elementAt(r))).setActive(true);

                    // mask.clear(); only works for Java1.4
                    for (int m = 0; m < mask.size(); m++) {
                        mask.clear(m);
                    }

                    selectedVOI.createActiveContourBinaryMask(mask, srcImage.getExtents()[0], srcImage.getExtents()[1]);
                    ((VOIContour) (contours[q].elementAt(r))).setActive(false);

                    if (srcImage.isColorImage()) {
                        minIntenRed = Float.MAX_VALUE;
                        maxIntenRed = -Float.MAX_VALUE;
                        minIntenGreen = Float.MAX_VALUE;
                        maxIntenGreen = -Float.MAX_VALUE;
                        minIntenBlue = Float.MAX_VALUE;
                        maxIntenBlue = -Float.MAX_VALUE;

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

                        statProperty.setProperty(statProperty.minIntensity + "Red" + "0;" + r,
                                                 nf.format(minIntenRed));
                        statProperty.setProperty(statProperty.maxIntensity + "Red" + "0;" + r,
                                                 nf.format(maxIntenRed));
                        statProperty.setProperty(statProperty.minIntensity + "Green" + "0;" + r,
                                                 nf.format(minIntenGreen));
                        statProperty.setProperty(statProperty.maxIntensity + "Green" + "0;" + r,
                                                 nf.format(maxIntenGreen));
                        statProperty.setProperty(statProperty.minIntensity + "Blue" + "0;" + r,
                                                 nf.format(minIntenBlue));
                        statProperty.setProperty(statProperty.maxIntensity + "Blue" + "0;" + r,
                                                 nf.format(maxIntenBlue));
                        statProperty.setProperty(statProperty.avgIntensity + "Red" + "0;" + r,
                                                 nf.format(avgIntenR));
                        statProperty.setProperty(statProperty.avgIntensity + "Green" + "0;" + r,
                                                 nf.format(avgIntenG));
                        statProperty.setProperty(statProperty.avgIntensity + "Blue" + "0;" + r,
                                                 nf.format(avgIntenB));
                        statProperty.setProperty(statProperty.quantityDescription + "0;" + r, nf.format(nVox));
                    } else {
                        minIntensity = Float.MAX_VALUE;
                        maxIntensity = -Float.MAX_VALUE;

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

                        avgInten = sum / nVox;

                        totalSum += sum;
                        totalNVox += nVox;

                        if (minIntensity < totalMinIntensity) {
                            totalMinIntensity = minIntensity;
                        }

                        if (maxIntensity > totalMaxIntensity) {
                            totalMaxIntensity = maxIntensity;
                        }

                        statProperty.setProperty(statProperty.minIntensity + "0;" + r, nf.format(minIntensity));
                        statProperty.setProperty(statProperty.maxIntensity + "0;" + r, nf.format(maxIntensity));
                        statProperty.setProperty(statProperty.avgIntensity + "0;" + r, nf.format(avgInten));
                        statProperty.setProperty(statProperty.quantityDescription + "0;" + r, nf.format(nVox));
                    }

                    area = nVox * (fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1]);
                    statProperty.setProperty(statProperty.areaDescription + "0;" + r, nf.format(area));
                    statProperty.setProperty(statProperty.volumeDescription + "0;" + r, nf.format(area));

                    // add perimeter
                    statProperty.setProperty(statProperty.perimeterDescription + "0;" + r, nf.format(perimeter));

                    totalArea += area;

                    // calculate standard deviation
                    sum = sumR = sumG = sumB = 0;

                    int cnt = 0;

                    if (srcImage.isColorImage()) {

                        for (int i = 0; i < length; i += 4) {

                            if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                                    !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                                    !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                                sumR += ((imgBuffer[i + 1] - avgIntenR) * (imgBuffer[i + 1] - avgIntenR));
                                sumG += ((imgBuffer[i + 2] - avgIntenG) * (imgBuffer[i + 2] - avgIntenG));
                                sumB += ((imgBuffer[i + 3] - avgIntenB) * (imgBuffer[i + 3] - avgIntenB));
                                cnt++;
                            }
                        }

                        stdDevR = (float) Math.sqrt(sumR / cnt);
                        stdDevG = (float) Math.sqrt(sumG / cnt);
                        stdDevB = (float) Math.sqrt(sumB / cnt);
                        statProperty.setProperty(statProperty.deviationDescription + "Red" + "0;" + r,
                                                 nf.format(stdDevR));
                        statProperty.setProperty(statProperty.deviationDescription + "Green" + "0;" + r,
                                                 nf.format(stdDevG));
                        statProperty.setProperty(statProperty.deviationDescription + "Blue" + "0;" + r,
                                                 nf.format(stdDevB));

                        totalStdDevR += sumR;
                        totalStdDevG += sumG;
                        totalStdDevB += sumB;
                    } else {

                        for (int i = 0; i < length; i++) {

                            if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                                sum += ((imgBuffer[i] - avgInten) * (imgBuffer[i] - avgInten));
                                cnt++;
                            }
                        }

                        stdDev = (float) Math.sqrt(sum / cnt);
                        statProperty.setProperty(statProperty.deviationDescription + "0;" + r, nf.format(stdDev));
                        totalStdDev += sum;
                    }
                }

            }

            if (showTotals == true) {
                statProperty.setProperty(statProperty.axisDescription + "Total", nf.format(totalAxis));
                statProperty.setProperty(statProperty.eccentricityDescription + "Total", nf.format(totalEcc));
                statProperty.setProperty(statProperty.majorAxisDescription + "Total", nf.format(totalMajorAxis));
                statProperty.setProperty(statProperty.minorAxisDescription + "Total", nf.format(totalMinorAxis));

                totalC.x *= srcImage.getFileInfo(0).getResolutions()[0];
                totalC.y *= srcImage.getFileInfo(0).getResolutions()[1];

                String comStr = nf.format(totalC.x) + "\t" + nf.format(totalC.y);

                statProperty.setProperty(statProperty.centerDescription + "Total", comStr);
                statProperty.setProperty(statProperty.areaDescription + "Total", nf.format(totalArea));
                statProperty.setProperty(statProperty.volumeDescription + "Total", nf.format(totalArea));
                statProperty.setProperty(statProperty.quantityDescription + "Total", nf.format(totalNVox));
                statProperty.setProperty(statProperty.perimeterDescription + "Total", nf.format(totalPerimeter));

                if (srcImage.isColorImage()) {
                    statProperty.setProperty(statProperty.deviationDescription + "Red" + "Total",
                                             nf.format((float) Math.sqrt(totalStdDevR / totalNVox)));
                    statProperty.setProperty(statProperty.deviationDescription + "Green" + "Total",
                                             nf.format((float) Math.sqrt(totalStdDevG / totalNVox)));
                    statProperty.setProperty(statProperty.deviationDescription + "Blue" + "Total",
                                             nf.format((float) Math.sqrt(totalStdDevB / totalNVox)));
                    statProperty.setProperty(statProperty.minIntensity + "Red" + "Total",
                                             nf.format(totalMinIntenRed));
                    statProperty.setProperty(statProperty.maxIntensity + "Red" + "Total",
                                             nf.format(totalMaxIntenRed));
                    statProperty.setProperty(statProperty.minIntensity + "Green" + "Total",
                                             nf.format(totalMinIntenGreen));
                    statProperty.setProperty(statProperty.maxIntensity + "Green" + "Total",
                                             nf.format(totalMaxIntenGreen));
                    statProperty.setProperty(statProperty.minIntensity + "Blue" + "Total",
                                             nf.format(totalMinIntenBlue));
                    statProperty.setProperty(statProperty.maxIntensity + "Blue" + "Total",
                                             nf.format(totalMaxIntenBlue));
                    statProperty.setProperty(statProperty.avgIntensity + "Red" + "Total",
                                             nf.format(totalSumR / totalNVox));
                    statProperty.setProperty(statProperty.avgIntensity + "Green" + "Total",
                                             nf.format(totalSumG / totalNVox));
                    statProperty.setProperty(statProperty.avgIntensity + "Blue" + "Total",
                                             nf.format(totalSumB / totalNVox));
                } else {
                    statProperty.setProperty(statProperty.minIntensity + "Total", nf.format(totalMinIntensity));
                    statProperty.setProperty(statProperty.maxIntensity + "Total", nf.format(totalMaxIntensity));
                    statProperty.setProperty(statProperty.avgIntensity + "Total", nf.format(totalSum / totalNVox));
                    statProperty.setProperty(statProperty.deviationDescription + "Total",
                                             nf.format((float) Math.sqrt(totalStdDev / totalNVox)));
                }
            }
        } else {
            VOIContour thisContour = ((VOIContour) (contours[0].elementAt(0)));

            thisContour.secondOrderAttributes(srcImage.getExtents()[0], srcImage.getExtents()[1],
                                              srcImage.getFileInfo(0).getResolutions()[0],
                                              srcImage.getFileInfo(0).getResolutions()[1],
                                              srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                              srcImage.getFileInfo(0).getUnitsOfMeasure()[1], tmpPAxis, tmpEcc,
                                              tmpMajorAxis, tmpMinorAxis);
            statProperty.setProperty(statProperty.axisDescription + "0;", nf.format(tmpPAxis[0]));
            statProperty.setProperty(statProperty.eccentricityDescription + "0;", nf.format(tmpEcc[0]));
            statProperty.setProperty(statProperty.majorAxisDescription + "0;", nf.format(tmpMajorAxis[0]));
            statProperty.setProperty(statProperty.minorAxisDescription + "0;", nf.format(tmpMinorAxis[0]));

            Point3Df selectedCOM = selectedVOI.getCenterOfMass();

            selectedCOM.x *= srcImage.getFileInfo(0).getResolutions()[0];
            selectedCOM.y *= srcImage.getFileInfo(0).getResolutions()[1];

            String comStr = nf.format(selectedCOM.x) + "\t" + nf.format(selectedCOM.y);
            statProperty.setProperty(statProperty.centerDescription + "0;", comStr);

            statProperty.setProperty(statProperty.axisDescription, nf.format(tmpPAxis[0]));
            statProperty.setProperty(statProperty.eccentricityDescription, nf.format(tmpEcc[0]));
            statProperty.setProperty(statProperty.majorAxisDescription, nf.format(tmpMajorAxis[0]));
            statProperty.setProperty(statProperty.minorAxisDescription, nf.format(tmpMinorAxis[0]));
            statProperty.setProperty(statProperty.centerDescription, comStr);

            selectedVOI.createBinaryMask(mask, srcImage.getExtents()[0], srcImage.getExtents()[1],
                                         srcImage.getParentFrame().useXOR(), false);

            // calc the perimeter
            totalPerimeter = 0f;

            for (int q = 0; q < contours.length; q++) {

                // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                for (int r = 0; r < contours[q].size(); r++) {
                    totalPerimeter += ((VOIContour) (contours[q].elementAt(r))).calcPerimeter(srcImage.getFileInfo(0).getResolutions()[0],
                                                                                              srcImage.getFileInfo(0).getResolutions()[1]);
                }
            }

            statProperty.setProperty(statProperty.perimeterDescription, nf.format(totalPerimeter));
            statProperty.setProperty(statProperty.perimeterDescription + "0;", nf.format(totalPerimeter));

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

                avgIntenR = sumR / nVox;
                avgIntenG = sumG / nVox;
                avgIntenB = sumB / nVox;

                statProperty.setProperty(statProperty.minIntensity + "Red" + "0;", nf.format(minIntenRed));
                statProperty.setProperty(statProperty.maxIntensity + "Red" + "0;", nf.format(maxIntenRed));
                statProperty.setProperty(statProperty.minIntensity + "Green" + "0;", nf.format(minIntenGreen));
                statProperty.setProperty(statProperty.maxIntensity + "Green" + "0;", nf.format(maxIntenGreen));
                statProperty.setProperty(statProperty.minIntensity + "Blue" + "0;", nf.format(minIntenBlue));
                statProperty.setProperty(statProperty.maxIntensity + "Blue" + "0;", nf.format(maxIntenBlue));
                statProperty.setProperty(statProperty.avgIntensity + "Red" + "0;", nf.format(avgIntenR));
                statProperty.setProperty(statProperty.avgIntensity + "Green" + "0;", nf.format(avgIntenG));
                statProperty.setProperty(statProperty.avgIntensity + "Blue" + "0;", nf.format(avgIntenB));
                statProperty.setProperty(statProperty.quantityDescription + "0;", nf.format(nVox));

                statProperty.setProperty(statProperty.minIntensity + "Red", nf.format(minIntenRed));
                statProperty.setProperty(statProperty.maxIntensity + "Red", nf.format(maxIntenRed));
                statProperty.setProperty(statProperty.minIntensity + "Green", nf.format(minIntenGreen));
                statProperty.setProperty(statProperty.maxIntensity + "Green", nf.format(maxIntenGreen));
                statProperty.setProperty(statProperty.minIntensity + "Blue", nf.format(minIntenBlue));
                statProperty.setProperty(statProperty.maxIntensity + "Blue", nf.format(maxIntenBlue));
                statProperty.setProperty(statProperty.avgIntensity + "Red", nf.format(avgIntenR));
                statProperty.setProperty(statProperty.avgIntensity + "Green", nf.format(avgIntenG));
                statProperty.setProperty(statProperty.avgIntensity + "Blue", nf.format(avgIntenB));
                statProperty.setProperty(statProperty.quantityDescription, nf.format(nVox));
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

                avgInten = sum / nVox;

                statProperty.setProperty(statProperty.minIntensity + "0;", nf.format(minIntensity));
                statProperty.setProperty(statProperty.maxIntensity + "0;", nf.format(maxIntensity));
                statProperty.setProperty(statProperty.avgIntensity + "0;", nf.format(avgInten));
                statProperty.setProperty(statProperty.quantityDescription + "0;", nf.format(nVox));

                statProperty.setProperty(statProperty.minIntensity, nf.format(minIntensity));
                statProperty.setProperty(statProperty.maxIntensity, nf.format(maxIntensity));
                statProperty.setProperty(statProperty.avgIntensity, nf.format(avgInten));
                statProperty.setProperty(statProperty.quantityDescription, nf.format(nVox));
            }

            area = nVox * (fileInfo[0].getResolutions()[0] * fileInfo[0].getResolutions()[1]);
            statProperty.setProperty(statProperty.areaDescription + "0;", nf.format(area));
            statProperty.setProperty(statProperty.volumeDescription + "0;", nf.format(area));

            statProperty.setProperty(statProperty.areaDescription, nf.format(area));
            statProperty.setProperty(statProperty.volumeDescription, nf.format(area));

            // calculate standard deviation
            sum = sumR = sumG = sumB = 0;

            int cnt = 0;

            if (srcImage.isColorImage()) {

                for (int i = 0; i < length; i += 4) {

                    if (mask.get(i / 4) && !inRange(ignoreMin, ignoreMax, imgBuffer[i + 1]) &&
                            !inRange(ignoreMin, ignoreMax, imgBuffer[i + 2]) &&
                            !inRange(ignoreMin, ignoreMax, imgBuffer[i + 3])) {
                        sumR += ((imgBuffer[i + 1] - avgIntenR) * (imgBuffer[i + 1] - avgIntenR));
                        sumG += ((imgBuffer[i + 2] - avgIntenG) * (imgBuffer[i + 2] - avgIntenG));
                        sumB += ((imgBuffer[i + 3] - avgIntenB) * (imgBuffer[i + 3] - avgIntenB));
                        cnt++;
                    }
                }

                stdDevR = (float) Math.sqrt(sumR / cnt);
                stdDevG = (float) Math.sqrt(sumG / cnt);
                stdDevB = (float) Math.sqrt(sumB / cnt);
                statProperty.setProperty(statProperty.deviationDescription + "Red" + "0;", nf.format(stdDevR));
                statProperty.setProperty(statProperty.deviationDescription + "Green" + "0;", nf.format(stdDevG));
                statProperty.setProperty(statProperty.deviationDescription + "Blue" + "0;", nf.format(stdDevB));

                statProperty.setProperty(statProperty.deviationDescription + "Red", nf.format(stdDevR));
                statProperty.setProperty(statProperty.deviationDescription + "Green", nf.format(stdDevG));
                statProperty.setProperty(statProperty.deviationDescription + "Blue", nf.format(stdDevB));
            } else {

                for (int i = 0; i < length; i++) {

                    if (mask.get(i) && !inRange(ignoreMin, ignoreMax, imgBuffer[i])) {
                        sum += ((imgBuffer[i] - avgInten) * (imgBuffer[i] - avgInten));
                        cnt++;
                    }
                }

                stdDev = (float) Math.sqrt(sum / cnt);
                statProperty.setProperty(statProperty.deviationDescription + "0;", nf.format(stdDev));
                statProperty.setProperty(statProperty.deviationDescription, nf.format(stdDev));
            }


        }

        setCompleted(true);
    }

    /**
     *   Calculates the image properties in an region defined by the VOI
     */
    private void calc34D( VOI selectedVOI ) {
        float minIntensity = Float.MAX_VALUE, totalMinIntensity = Float.MAX_VALUE;
        float maxIntensity = -Float.MAX_VALUE, totalMaxIntensity = -Float.MAX_VALUE;
        float minIntenRed = Float.MAX_VALUE, totalMinIntenRed = Float.MAX_VALUE;
        float maxIntenRed = -Float.MAX_VALUE, totalMaxIntenRed = -Float.MAX_VALUE;
        float minIntenGreen = Float.MAX_VALUE, totalMinIntenGreen = Float.MAX_VALUE;
        float maxIntenGreen = -Float.MAX_VALUE, totalMaxIntenGreen = -Float.MAX_VALUE;
        float minIntenBlue = Float.MAX_VALUE, totalMinIntenBlue = Float.MAX_VALUE;
        float maxIntenBlue = -Float.MAX_VALUE, totalMaxIntenBlue = -Float.MAX_VALUE;
        float avgInten = 0;
        float avgIntenR = 0;
        float avgIntenG = 0;
        float avgIntenB = 0;
        float stdDev = 0, stdDevR = 0, stdDevG = 0, stdDevB = 0;
        float totalStdDev = 0, totalStdDevR = 0, totalStdDevG = 0, totalStdDevB = 0;
        float volume = 0, totalVolume = 0;
        float sum = 0, sumR = 0, sumG = 0, sumB = 0, area = 0;
        float totalSum = 0, totalSumR = 0, totalSumG = 0, totalSumB = 0, totalArea = 0;
        float totalAxis = 0, totalEcc = 0;
        float totalMajorAxis = 0;
        float totalMinorAxis = 0;
        int nVox = 0, totalNVox = 0;
        Point3Df totalC = new Point3Df( 0, 0, 0 );
        float[] imgBuffer;
        float[] tmpPAxis = null;
        float[] tmpEcc = null;
        float[] tmpMajorAxis = null;
        float[] tmpMinorAxis = null;
        float[] xExtents = null;
        float[] yExtents = null;
        float[] zExtents = null;
        int length;
        float perimeter = 0f;
        float totalPerimeter = 0f;


        Vector[] contours;
        BitSet mask;
        VOIStatisticalProperties statProperty = getVOIProperties( selectedVOI );

        area = 0;
        volume = 0;
        nVox = 0;

        if ( srcImage.isColorImage() ) {
            length = 4 * srcImage.getSliceSize();
        } else {
            length = srcImage.getSliceSize();
        }

        FileInfoBase[] fileInfo = srcImage.getFileInfo();

        try {
            imgBuffer = new float[length * srcImage.getExtents()[2]];
            int offset4D = imgBuffer.length *
                           ViewUserInterface.getReference().getFrameContainingImage(srcImage).getViewableTimeSlice();

            mask = new BitSet( length );
            tmpPAxis = new float[1];
            tmpEcc = new float[1];
            tmpMajorAxis = new float[1];
            tmpMinorAxis = new float[1];
            xExtents = new float[2];
            yExtents = new float[2];
            zExtents = new float[2];

            srcImage.exportData(offset4D, imgBuffer.length, imgBuffer);

        } catch ( IOException error ) {
            displayError( "Algorithm VOI Properties: Image(s) locked" );
            setCompleted( false );
            return;
        } catch ( OutOfMemoryError e ) {
            displayError( "Algorithm VOI Properties: Out of Memory" );
            setCompleted( false );
            return;
        }

        selectedVOI.getBounds( xExtents, yExtents, zExtents );
        float ignoreMin = selectedVOI.getMinimumIgnore();
        float ignoreMax = selectedVOI.getMaximumIgnore();

        contours = selectedVOI.getCurves();
        if ( perSlice == true || perContour == true ) {
            // since we're in a 3D image, contours.length is how many slices this VOI is on
            for ( int q = 0; q < contours.length; q++ ) {
                int stop = 1;
                String end = q + ";";

                if ( perContour == true ) {
                    stop = contours[q].size();
                }
                if ( contours[q].size() < 1 ) {
                    stop = 0;
                }
                for ( int r = 0; r < stop; r++ ) {
                    if ( perContour == true ) {
                        end = q + ";" + r;
                    }

                    sum = 0;
                    sumR = 0;
                    sumG = 0;
                    sumB = 0;
                    nVox = 0;
                    area = 0;
                    volume = 0;

                    ( (VOIContour) ( contours[q].elementAt( r ) ) ).secondOrderAttributes(
                      srcImage.getExtents()[0], srcImage.getExtents()[1],
                      srcImage.getFileInfo(0).getResolutions()[0],
                      srcImage.getFileInfo(0).getResolutions()[1],
                      srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                      srcImage.getFileInfo(0).getUnitsOfMeasure()[1],
                      tmpPAxis, tmpEcc, tmpMajorAxis, tmpMinorAxis );
                    statProperty.setProperty( statProperty.axisDescription + end, nf.format( tmpPAxis[0] ) );
                    statProperty.setProperty( statProperty.eccentricityDescription + end, nf.format( tmpEcc[0] ) );
                    statProperty.setProperty( statProperty.majorAxisDescription + end, nf.format( tmpMajorAxis[0] ) );
                    statProperty.setProperty( statProperty.minorAxisDescription + end, nf.format( tmpMinorAxis[0] ) );

                    Point3Df centerOfMass = ((VOIContour) (contours[q].elementAt(r))).getCenterOfMass();
                    centerOfMass.x *= srcImage.getFileInfo(0).getResolutions()[0];
                    centerOfMass.y *= srcImage.getFileInfo(0).getResolutions()[1];
                    if (srcImage.getNDims() > 2) {
                        centerOfMass.z *= srcImage.getFileInfo(0).getResolutions()[2];
                    }

                    String comStr = nf.format(centerOfMass.x) + "\t" + nf.format(centerOfMass.y) + "\t" + nf.format(centerOfMass.z);

                    statProperty.setProperty( statProperty.centerDescription + end, comStr );


                    totalEcc += tmpEcc[0];
                    totalAxis += tmpPAxis[0];
                    totalMajorAxis += tmpMajorAxis[0];
                    totalMinorAxis += tmpMinorAxis[0];
                    totalC = selectedVOI.getCenterOfMass();

                    perimeter = ((VOIContour)(contours[q].elementAt(r))).calcPerimeter(srcImage.getFileInfo(0).getResolutions()[0],
                        srcImage.getFileInfo(0).getResolutions()[1]);
                    totalPerimeter += perimeter;


                     mask.clear(); //only works for Java1.4
                    //for ( int m = 0; m < mask.size(); m++ ) {
                    //    mask.clear( m );
                    //}
                    if ( perSlice == true ) {
                        for ( int rr = 0; rr < contours[q].size(); rr++ ) {
                            ( (VOIContour) ( contours[q].elementAt( rr ) ) ).setActive( true );
                        }
                    } else {
                        ( (VOIContour) ( contours[q].elementAt( r ) ) ).setActive( true );
                    }
                    selectedVOI.createActiveContourBinaryMask( srcImage.getExtents()[0], srcImage.getExtents()[1], q,
                            mask, true );
                    if ( perSlice == true ) {
                        for ( int rr = 0; rr < contours[q].size(); rr++ ) {
                            ( (VOIContour) ( contours[q].elementAt( rr ) ) ).setActive( false );
                        }
                    } else {
                        ( (VOIContour) ( contours[q].elementAt( r ) ) ).setActive( false );
                    }

                    Point3Df[] pts = selectedVOI.maxWidth();

                    statProperty.setProperty( statProperty.maxWidthDescription + end,
                            nf.format(
                            Math.sqrt(
                                    ( pts[1].x - pts[0].x ) * fileInfo[q].getResolutions()[0] * ( pts[1].x - pts[0].x )
                                    * fileInfo[q].getResolutions()[0]
                                            + ( pts[1].y - pts[0].y ) * fileInfo[q].getResolutions()[1]
                                            * ( pts[1].y - pts[0].y ) * fileInfo[q].getResolutions()[1]
                                            + ( pts[1].z - pts[0].z ) * fileInfo[q].getResolutions()[2]
                                            * ( pts[1].z - pts[0].z ) * fileInfo[q].getResolutions()[2] ) ) );

                    if ( srcImage.isColorImage() ) {
                        minIntenRed = Float.MAX_VALUE;
                        maxIntenRed = -Float.MAX_VALUE;
                        minIntenGreen = Float.MAX_VALUE;
                        maxIntenGreen = -Float.MAX_VALUE;
                        minIntenBlue = Float.MAX_VALUE;
                        maxIntenBlue = -Float.MAX_VALUE;
                        int offset = length * q;

                        for ( int i = 0; i < length; i += 4 ) {
                            if ( mask.get( i / 4 ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 1] )
                                    && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 2] )
                                    && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 3] ) ) {
                                sumR += imgBuffer[offset + i + 1];
                                sumG += imgBuffer[offset + i + 2];
                                sumB += imgBuffer[offset + i + 3];
                                nVox++;
                                if ( imgBuffer[offset + i + 1] < minIntenRed ) {
                                    minIntenRed = imgBuffer[offset + i + 1];
                                }
                                if ( imgBuffer[offset + i + 1] > maxIntenRed ) {
                                    maxIntenRed = imgBuffer[offset + i + 1];
                                }

                                if ( imgBuffer[offset + i + 2] < minIntenGreen ) {
                                    minIntenGreen = imgBuffer[offset + i + 2];
                                }
                                if ( imgBuffer[offset + i + 2] > maxIntenGreen ) {
                                    maxIntenGreen = imgBuffer[offset + i + 2];
                                }

                                if ( imgBuffer[offset + i + 3] < minIntenBlue ) {
                                    minIntenBlue = imgBuffer[offset + i + 3];
                                }
                                if ( imgBuffer[offset + i + 3] > maxIntenBlue ) {
                                    maxIntenBlue = imgBuffer[offset + i + 3];
                                }
                            }
                        }
                        avgIntenR = sumR / nVox;
                        avgIntenG = sumG / nVox;
                        avgIntenB = sumB / nVox;

                        totalSumR += sumR;
                        totalSumG += sumG;
                        totalSumB += sumB;
                        totalNVox += nVox;

                        if ( minIntenRed < totalMinIntenRed ) {
                            totalMinIntenRed = minIntenRed;
                        }
                        if ( minIntenGreen < totalMinIntenRed ) {
                            totalMinIntenGreen = minIntenGreen;
                        }
                        if ( minIntenBlue < totalMinIntenRed ) {
                            totalMinIntenBlue = minIntenBlue;
                        }
                        if ( maxIntenRed > totalMaxIntenRed ) {
                            totalMaxIntenRed = maxIntenRed;
                        }
                        if ( maxIntenGreen > totalMaxIntenGreen ) {
                            totalMaxIntenGreen = maxIntenGreen;
                        }
                        if ( maxIntenBlue > totalMaxIntenBlue ) {
                            totalMaxIntenBlue = maxIntenBlue;
                        }

                        statProperty.setProperty( statProperty.minIntensity + "Red" + end, nf.format( minIntenRed ) );
                        statProperty.setProperty( statProperty.maxIntensity + "Red" + end, nf.format( maxIntenRed ) );
                        statProperty.setProperty( statProperty.minIntensity + "Green" + end,
                                nf.format( minIntenGreen ) );
                        statProperty.setProperty( statProperty.maxIntensity + "Green" + end,
                                nf.format( maxIntenGreen ) );
                        statProperty.setProperty( statProperty.minIntensity + "Blue" + end,
                                nf.format( minIntenBlue ) );
                        statProperty.setProperty( statProperty.maxIntensity + "Blue" + end,
                                nf.format( maxIntenBlue ) );
                        statProperty.setProperty( statProperty.avgIntensity + "Red" + end, nf.format( avgIntenR ) );
                        statProperty.setProperty( statProperty.avgIntensity + "Green" + end, nf.format( avgIntenG ) );
                        statProperty.setProperty( statProperty.avgIntensity + "Blue" + end, nf.format( avgIntenB ) );
                        statProperty.setProperty( statProperty.quantityDescription + end, nf.format( nVox ) );
                    } else {
                        minIntensity = Float.MAX_VALUE;
                        maxIntensity = -Float.MAX_VALUE;
                        int offset = length * q;

                        for ( int i = 0; i < length; i++ ) {
                            if ( mask.get( i ) && !inRange( ignoreMin, ignoreMax, imgBuffer[offset + i] ) ) {
                                sum += imgBuffer[offset + i];
                                nVox++;
                                if ( imgBuffer[offset + i] < minIntensity ) {
                                    minIntensity = imgBuffer[offset + i];
                                }
                                if ( imgBuffer[offset + i] > maxIntensity ) {
                                    maxIntensity = imgBuffer[offset + i];
                                }
                            }
                        }
                        avgInten = sum / nVox;

                        totalSum += sum;
                        totalNVox += nVox;
                        if ( minIntensity < totalMinIntensity ) {
                            totalMinIntensity = minIntensity;
                        }
                        if ( maxIntensity > totalMaxIntensity ) {
                            totalMaxIntensity = maxIntensity;
                        }

                        statProperty.setProperty( statProperty.minIntensity + end, nf.format( minIntensity ) );
                        statProperty.setProperty( statProperty.maxIntensity + end, nf.format( maxIntensity ) );
                        statProperty.setProperty( statProperty.avgIntensity + end, nf.format( avgInten ) );
                        statProperty.setProperty( statProperty.quantityDescription + end, nf.format( nVox ) );
                    }
                    area = nVox * ( fileInfo[q].getResolutions()[0] * fileInfo[q].getResolutions()[1] );
                    statProperty.setProperty( statProperty.areaDescription + end, nf.format( area ) );
                    volume = area * fileInfo[q].getResolutions()[2];
                    statProperty.setProperty( statProperty.volumeDescription + end, nf.format( volume ) );

                    totalArea += area;
                    totalVolume += volume;

                    //add perimeter
                    statProperty.setProperty( statProperty.perimeterDescription + end, nf.format(perimeter));

                    // calculate standard deviation
                    sum = sumR = sumG = sumB = 0;
                    int cnt = 0;

                    if ( srcImage.isColorImage() ) {
                        int offset = length * q;

                        for ( int i = 0; i < length; i += 4 ) {
                            if ( mask.get( i / 4 ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 1] )
                                    && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 2] )
                                    && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 3] ) ) {
                                sumR += ( ( imgBuffer[offset + i + 1] - avgIntenR )
                                        * ( imgBuffer[offset + i + 1] - avgIntenR ) );
                                sumG += ( ( imgBuffer[offset + i + 2] - avgIntenG )
                                        * ( imgBuffer[offset + i + 2] - avgIntenG ) );
                                sumB += ( ( imgBuffer[offset + i + 3] - avgIntenB )
                                        * ( imgBuffer[offset + i + 3] - avgIntenB ) );
                                cnt++;
                            }
                        }
                        stdDevR = (float) Math.sqrt( sumR / cnt );
                        stdDevG = (float) Math.sqrt( sumG / cnt );
                        stdDevB = (float) Math.sqrt( sumB / cnt );
                        statProperty.setProperty( statProperty.deviationDescription + "Red" + end,
                                nf.format( stdDevR ) );
                        statProperty.setProperty( statProperty.deviationDescription + "Green" + end,
                                nf.format( stdDevG ) );
                        statProperty.setProperty( statProperty.deviationDescription + "Blue" + end,
                                nf.format( stdDevB ) );

                        totalStdDevR += sumR;
                        totalStdDevG += sumG;
                        totalStdDevB += sumB;
                    } else {
                        int offset = length * q;

                        for ( int i = 0; i < length; i++ ) {
                            if ( mask.get( i ) && !inRange( ignoreMin, ignoreMax, imgBuffer[offset + i] ) ) {
                                sum += ( ( imgBuffer[offset + i] - avgInten ) * ( imgBuffer[offset + i] - avgInten ) );
                                cnt++;
                            }
                        }
                        stdDev = (float) Math.sqrt( sum / cnt );
                        statProperty.setProperty( statProperty.deviationDescription + end, nf.format( stdDev ) );
                        totalStdDev += sum;
                    }
                }
            }
            if ( showTotals == true ) {

                totalC.x *= srcImage.getFileInfo(0).getResolutions()[0];
                totalC.y *= srcImage.getFileInfo(0).getResolutions()[1];
                if (srcImage.getNDims() > 2) {
                    totalC.z *= srcImage.getFileInfo(0).getResolutions()[2];
                }

                String comStr = nf.format(totalC.x) + "\t" + nf.format(totalC.y) + "\t" + nf.format(totalC.z);

                statProperty.setProperty( statProperty.axisDescription + "Total", nf.format( totalAxis ) );
                statProperty.setProperty( statProperty.eccentricityDescription + "Total", nf.format( totalEcc ) );
                statProperty.setProperty( statProperty.majorAxisDescription + "Total", nf.format( totalMajorAxis ) );
                statProperty.setProperty( statProperty.minorAxisDescription + "Total", nf.format( totalMinorAxis ) );
                statProperty.setProperty( statProperty.centerDescription + "Total", comStr );
                statProperty.setProperty( statProperty.areaDescription + "Total", nf.format( totalArea ) );
                statProperty.setProperty( statProperty.volumeDescription + "Total", nf.format( totalVolume ) );
                statProperty.setProperty( statProperty.quantityDescription + "Total", nf.format( totalNVox ) );
                statProperty.setProperty( statProperty.perimeterDescription + "Total", nf.format( totalPerimeter ) );

                if ( srcImage.isColorImage() ) {
                    statProperty.setProperty( statProperty.deviationDescription + "Red" + "Total",
                            nf.format( (float) Math.sqrt( totalStdDevR / totalNVox ) ) );
                    statProperty.setProperty( statProperty.deviationDescription + "Green" + "Total",
                            nf.format( (float) Math.sqrt( totalStdDevG / totalNVox ) ) );
                    statProperty.setProperty( statProperty.deviationDescription + "Blue" + "Total",
                            nf.format( (float) Math.sqrt( totalStdDevB / totalNVox ) ) );
                    statProperty.setProperty( statProperty.minIntensity + "Red" + "Total",
                            nf.format( totalMinIntenRed ) );
                    statProperty.setProperty( statProperty.maxIntensity + "Red" + "Total",
                            nf.format( totalMaxIntenRed ) );
                    statProperty.setProperty( statProperty.minIntensity + "Green" + "Total",
                            nf.format( totalMinIntenGreen ) );
                    statProperty.setProperty( statProperty.maxIntensity + "Green" + "Total",
                            nf.format( totalMaxIntenGreen ) );
                    statProperty.setProperty( statProperty.minIntensity + "Blue" + "Total",
                            nf.format( totalMinIntenBlue ) );
                    statProperty.setProperty( statProperty.maxIntensity + "Blue" + "Total",
                            nf.format( totalMaxIntenBlue ) );
                    statProperty.setProperty( statProperty.avgIntensity + "Red" + "Total",
                            nf.format( totalSumR / totalNVox ) );
                    statProperty.setProperty( statProperty.avgIntensity + "Green" + "Total",
                            nf.format( totalSumG / totalNVox ) );
                    statProperty.setProperty( statProperty.avgIntensity + "Blue" + "Total",
                            nf.format( totalSumB / totalNVox ) );
                } else {
                    statProperty.setProperty( statProperty.minIntensity + "Total", nf.format( totalMinIntensity ) );
                    statProperty.setProperty( statProperty.maxIntensity + "Total", nf.format( totalMaxIntensity ) );
                    statProperty.setProperty( statProperty.avgIntensity + "Total",
                            nf.format( totalSum / totalNVox ) );
                    statProperty.setProperty( statProperty.deviationDescription + "Total",
                            nf.format( (float) Math.sqrt( totalStdDev / totalNVox ) ) );
                }
            }
        } else {
            VOIContour thisContour = null;

            for ( int i = 0; i < contours.length; i++ ) {
                if ( contours[i].size() > 0 ) {
                    thisContour = ( (VOIContour) ( contours[i].elementAt( 0 ) ) );
                    break;
                }
            }
            thisContour.secondOrderAttributes( srcImage.getExtents()[0], srcImage.getExtents()[1],
                                               srcImage.getFileInfo(0).getResolutions()[0],
                                               srcImage.getFileInfo(0).getResolutions()[1],
                                               srcImage.getFileInfo(0).getUnitsOfMeasure()[0],
                                               srcImage.getFileInfo(0).getUnitsOfMeasure()[1],
                                               tmpPAxis, tmpEcc, tmpMajorAxis, tmpMinorAxis );
            statProperty.setProperty( statProperty.axisDescription, nf.format( tmpPAxis[0] ) );
            statProperty.setProperty( statProperty.eccentricityDescription, nf.format( tmpEcc[0] ) );
            statProperty.setProperty( statProperty.majorAxisDescription, nf.format( tmpMajorAxis[0] ) );
            statProperty.setProperty( statProperty.minorAxisDescription, nf.format( tmpMinorAxis[0] ) );

            Point3Df selectedCOM = selectedVOI.getCenterOfMass();
            selectedCOM.x *= srcImage.getFileInfo(0).getResolutions()[0];
            selectedCOM.y *= srcImage.getFileInfo(0).getResolutions()[1];
            if (srcImage.getNDims() > 2) {
                selectedCOM.z *= srcImage.getFileInfo(0).getResolutions()[2];
            }

            String comStr = nf.format(selectedCOM.x) + "\t" + nf.format(selectedCOM.y) + "\t" + nf.format(selectedCOM.z);

            statProperty.setProperty( statProperty.centerDescription, comStr );

            mask = new BitSet( imgBuffer.length );
            selectedVOI.createBinaryMask( mask, srcImage.getExtents()[0], srcImage.getExtents()[1],
               srcImage.getParentFrame().useXOR(), false );

            if ( srcImage.isColorImage() ) {
                for ( int i = 0; i < imgBuffer.length; i += 4 ) {
                    if ( mask.get( i / 4 ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 1] )
                            && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 2] )
                            && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 3] ) ) {
                        sumR += imgBuffer[i + 1];
                        sumG += imgBuffer[i + 2];
                        sumB += imgBuffer[i + 3];
                        nVox++;
                        if ( imgBuffer[i + 1] < minIntenRed ) {
                            minIntenRed = imgBuffer[i + 1];
                        }
                        if ( imgBuffer[i + 1] > maxIntenRed ) {
                            maxIntenRed = imgBuffer[i + 1];
                        }

                        if ( imgBuffer[i + 2] < minIntenGreen ) {
                            minIntenGreen = imgBuffer[i + 2];
                        }
                        if ( imgBuffer[i + 2] > maxIntenGreen ) {
                            maxIntenGreen = imgBuffer[i + 2];
                        }

                        if ( imgBuffer[i + 3] < minIntenBlue ) {
                            minIntenBlue = imgBuffer[i + 3];
                        }
                        if ( imgBuffer[i + 3] > maxIntenBlue ) {
                            maxIntenBlue = imgBuffer[i + 3];
                        }
                    }
                }
                avgIntenR = sumR / nVox;
                avgIntenG = sumG / nVox;
                avgIntenB = sumB / nVox;

                statProperty.setProperty( statProperty.minIntensity + "Red", nf.format( minIntenRed ) );
                statProperty.setProperty( statProperty.maxIntensity + "Red", nf.format( maxIntenRed ) );
                statProperty.setProperty( statProperty.minIntensity + "Green", nf.format( minIntenGreen ) );
                statProperty.setProperty( statProperty.maxIntensity + "Green", nf.format( maxIntenGreen ) );
                statProperty.setProperty( statProperty.minIntensity + "Blue", nf.format( minIntenBlue ) );
                statProperty.setProperty( statProperty.maxIntensity + "Blue", nf.format( maxIntenBlue ) );
                statProperty.setProperty( statProperty.avgIntensity + "Red", nf.format( avgIntenR ) );
                statProperty.setProperty( statProperty.avgIntensity + "Green", nf.format( avgIntenG ) );
                statProperty.setProperty( statProperty.avgIntensity + "Blue", nf.format( avgIntenB ) );
                statProperty.setProperty( statProperty.quantityDescription, nf.format( nVox ) );
            } else {
                for ( int i = 0; i < imgBuffer.length; i++ ) {
                    if ( mask.get( i ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i] ) ) {
                        sum += imgBuffer[i];
                        nVox++;
                        if ( imgBuffer[i] < minIntensity ) {
                            minIntensity = imgBuffer[i];
                        }
                        if ( imgBuffer[i] > maxIntensity ) {
                            maxIntensity = imgBuffer[i];
                        }
                    }
                }
                avgInten = sum / nVox;

                statProperty.setProperty( statProperty.minIntensity, nf.format( minIntensity ) );
                statProperty.setProperty( statProperty.maxIntensity, nf.format( maxIntensity ) );
                statProperty.setProperty( statProperty.avgIntensity, nf.format( avgInten ) );
                statProperty.setProperty( statProperty.quantityDescription, nf.format( nVox ) );
            }

            //calc the perimeter
            totalPerimeter = 0f;
            for ( int q = 0; q < contours.length; q++ ) {
                // System.out.println("algoVOIprops nContours = " + contours[q].size() );
                for (int r = 0; r < contours[q].size(); r++) {
                    totalPerimeter +=( (VOIContour) ( contours[q].elementAt( r ) ) ).calcPerimeter(srcImage.getFileInfo(0).getResolutions()[0],
                        srcImage.getFileInfo(0).getResolutions()[1]);
                }
            }

            statProperty.setProperty( statProperty.perimeterDescription, nf.format(totalPerimeter));
            statProperty.setProperty( statProperty.perimeterDescription + "0;", nf.format(totalPerimeter));


            area = nVox * ( fileInfo[fileInfo.length/2].getResolutions()[0] * fileInfo[fileInfo.length/2].getResolutions()[1] );
            volume = area * fileInfo[fileInfo.length/2].getResolutions()[2];
            statProperty.setProperty( statProperty.areaDescription, nf.format( area ) );
            statProperty.setProperty( statProperty.volumeDescription, nf.format( volume ) );

            // calculate standard deviation
            sum = sumR = sumG = sumB = 0;
            int cnt = 0;

            if ( srcImage.isColorImage() ) {
                for ( int i = 0; i < imgBuffer.length; i += 4 ) {
                    if ( mask.get( i / 4 ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 1] )
                            && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 2] )
                            && !inRange( ignoreMin, ignoreMax, imgBuffer[i + 3] ) ) {
                        sumR += ( ( imgBuffer[i + 1] - avgIntenR ) * ( imgBuffer[i + 1] - avgIntenR ) );
                        sumG += ( ( imgBuffer[i + 2] - avgIntenG ) * ( imgBuffer[i + 2] - avgIntenG ) );
                        sumB += ( ( imgBuffer[i + 3] - avgIntenB ) * ( imgBuffer[i + 3] - avgIntenB ) );
                        cnt++;
                    }
                }
                stdDevR = (float) Math.sqrt( sumR / cnt );
                stdDevG = (float) Math.sqrt( sumG / cnt );
                stdDevB = (float) Math.sqrt( sumB / cnt );
                statProperty.setProperty( statProperty.deviationDescription + "Red", nf.format( stdDevR ) );
                statProperty.setProperty( statProperty.deviationDescription + "Green", nf.format( stdDevG ) );
                statProperty.setProperty( statProperty.deviationDescription + "Blue", nf.format( stdDevB ) );
            } else {
                for ( int i = 0; i < imgBuffer.length; i++ ) {
                    if ( mask.get( i ) && !inRange( ignoreMin, ignoreMax, imgBuffer[i] ) ) {
                        sum += ( ( imgBuffer[i] - avgInten ) * ( imgBuffer[i] - avgInten ) );
                        cnt++;
                    }
                }
                stdDev = (float) Math.sqrt( sum / cnt );
                statProperty.setProperty( statProperty.deviationDescription, nf.format( stdDev ) );
            }
        }
        setCompleted( true );
    }


    /**
     * not for use. should be moved to a better location. does NOT clone the VOIs that it find to be active, and inserts
     * into a new ViewVOIVector. if no VOIs are active, the ViewVOIVector returned is <code>null</code>.
     *
     * @return  DOCUMENT ME!
     */
    private ViewVOIVector getActiveVOIs() {
        ViewVOIVector voiList;

        voiList = new ViewVOIVector();

        int i;

        try {

            for (i = 0; i < srcImage.getVOIs().size(); i++) {

                if (srcImage.getVOIs().VOIAt(i).isActive()) {

                    // voi at i is the active voi
                    voiList.addElement(srcImage.getVOIs().VOIAt(i));
                }
            }
        } catch (ArrayIndexOutOfBoundsException indexException) {

            // got to the end of list and never found an active VOI.
            // return an  empty VOI list.
            return new ViewVOIVector();
        }

        return voiList;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  numberOfVOIs  DOCUMENT ME!
     */
    private void initialiseDataHolders(int numberOfVOIs) {
        propertyList = new Vector(numberOfVOIs);

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
     * method to decide if a particular string is in the list. case insensitive.
     *
     * @param   str   string
     * @param   list  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean isStatistic(String str, ViewList[] list) {

        if (statisticList != null) {
            return statisticList.getSelectedList(str);
        }

        for (int a = 0; a < list.length; a++) {

            if (list[a].getString().equalsIgnoreCase(str) && (list[a].getState() == true)) {
                return true;
            }
        }

        return false;
    }

}
