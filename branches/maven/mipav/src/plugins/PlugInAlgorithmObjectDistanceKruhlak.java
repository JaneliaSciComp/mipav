import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This is simple plugin that finds the distances between 2 different color centers of mass weighted by fluorescence and
 * thresholded within a 2D or 3D voi. Multiple VOIs can be handled. 2D or 3D color images are allowed. Only color values
 * >= threshold will be counted toward the center of mass. The thresholds are specified for each color by the user in
 * the dialog box. Be sure to hit the new VOI button for each new VOI. Note that a single VOI can contain multiple
 * contours so if mulitple curves are used without hitting the new VOI button, then the interiors of all these curves
 * will belong to the same VOI.
 *
 * @version  June 3, 2004
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmObjectDistanceKruhlak.java $ $Revision: 7 $ $Date: 1/25/06
 *           4:59p $</p>
 */
public class PlugInAlgorithmObjectDistanceKruhlak extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int threshold1 = 0;

    /** DOCUMENT ME! */
    private int threshold2 = 0;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useRed = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg      Source image model.
     * @param  useRed      DOCUMENT ME!
     * @param  useGreen    DOCUMENT ME!
     * @param  useBlue     DOCUMENT ME!
     * @param  threshold1  DOCUMENT ME!
     * @param  threshold2  DOCUMENT ME!
     */
    public PlugInAlgorithmObjectDistanceKruhlak(ModelImage srcImg, boolean useRed, boolean useGreen, boolean useBlue,
                                                int threshold1, int threshold2) {
        super(null, srcImg);
        this.useRed = useRed;
        this.useBlue = useBlue;
        this.threshold1 = threshold1;
        this.threshold2 = threshold2;
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
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void calc2D() {
        int length; // total number of data-elements (pixels) in image
        float[] buffer1;
        float[] buffer2;
        int count1;
        int count2;
        float xPos1;
        float yPos1;
        float xPos2;
        float yPos2;
        float centerToCenter;
        int i, j;
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        ViewVOIVector VOIs = null;
        int nVOIs;
        short[] shortMask;
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();
        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        String unitsString = null;

        if ((xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        try {

            // image length is length in 2 dims
            length = xDim * yDim;
            buffer1 = new float[length];
            buffer2 = new float[length];

            if (useRed) {
                srcImage.exportRGBData(1, 0, length, buffer1); // export red data
            } else {
                srcImage.exportRGBData(2, 0, length, buffer1); // export green data
            }

            if (useBlue) {
                srcImage.exportRGBData(3, 0, length, buffer2); // export blue data
            } else {
                srcImage.exportRGBData(2, 0, length, buffer2); // export green data
            }
        } catch (IOException error) {
            buffer1 = null;
            buffer2 = null;
            errorCleanUp("Algorithm ObjectDistanceKruhlak reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer1 = null;
            buffer2 = null;
            errorCleanUp("Algorithm ObjectDistanceKruhlak reports: out of memory", true);

            return;
        }

        fireProgressStateChanged("Processing image ...");

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        shortMask = new short[length];

        for (i = 0; i < length; i++) {
            shortMask[i] = -1;
        }

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(100 * i / nVOIs);

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                shortMask = srcImage.generateVOIMask(shortMask, i);
                count1 = 0;
                count2 = 0;
                xPos1 = 0.0f;
                yPos1 = 0.0f;
                xPos2 = 0.0f;
                yPos2 = 0.0f;

                for (j = 0, y = 0; y < yDim; y++, j += xDim) {

                    for (x = 0; x < xDim; x++) {
                        index = x + j;

                        if (shortMask[index] == i) {

                            if (buffer1[index] >= threshold1) {
                                xPos1 += buffer1[index] * x;
                                yPos1 += buffer1[index] * y;
                                count1 += buffer1[index];
                            }

                            if (buffer2[index] >= threshold2) {
                                xPos2 += buffer2[index] * x;
                                yPos2 += buffer2[index] * y;
                                count2 += buffer2[index];
                            }
                        } // if (shortMask[index] == i)
                    } // for (x = 0; x < xDim; x++)
                } // for (j = 0, y = 0; y < yDim; y++, j += xDim)

                xPos1 /= count1;
                yPos1 /= count1;
                xPos2 /= count2;
                yPos2 /= count2;
                centerToCenter = (float)
                                     Math.sqrt(((xPos1 - xPos2) * (xPos1 - xPos2) * xRes * xRes) +
                                               ((yPos1 - yPos2) * (yPos1 - yPos2) * yRes * yRes));

                if (useRed) {
                    UI.setDataText("VOI ID = " + i + " with red weighted voi center of mass = " + "(" + xPos1 + ", " +
                                   yPos1 + ")\n");
                } else {
                    UI.setDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos1 + ", " +
                                   yPos1 + ")\n");
                }

                if (useBlue) {
                    UI.setDataText("VOI ID = " + i + " with blue weighted voi center of mass = " + "(" + xPos2 + ", " +
                                   yPos2 + ")\n");
                } else {
                    UI.setDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos2 + ", " +
                                   yPos2 + ")\n");
                }

                if (unitsString != null) {
                    UI.setDataText("Center to center distance = " + centerToCenter + " " + unitsString + "\n");
                } else {
                    UI.setDataText("Center to center distance = " + centerToCenter + "\n");
                }
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        fireProgressStateChanged(100);

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void calc3D() {
        int sliceLength;
        int totLength;
        float[] buffer1;
        float[] buffer2;
        int count1;
        int count2;
        float xPos1;
        float yPos1;
        float zPos1;
        float xPos2;
        float yPos2;
        float zPos2;
        float centerToCenter;
        int i, j, k;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        float zRes = srcImage.getResolutions(0)[2];
        ViewVOIVector VOIs = null;
        int nVOIs;
        short[] shortMask;
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();
        int xUnits = srcImage.getUnitsOfMeasure()[0];
        int yUnits = srcImage.getUnitsOfMeasure()[1];
        int zUnits = srcImage.getUnitsOfMeasure()[2];
        String unitsString = null;

        if ((xUnits == yUnits) && (xUnits == zUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        try {
            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;
            buffer1 = new float[totLength];
            buffer2 = new float[totLength];

            if (useRed) {
                srcImage.exportRGBData(1, 0, totLength, buffer1); // export red data
            } else {
                srcImage.exportRGBData(2, 0, totLength, buffer1); // export green data
            }

            if (useBlue) {
                srcImage.exportRGBData(3, 0, totLength, buffer2); // export blue data
            } else {
                srcImage.exportRGBData(2, 0, totLength, buffer2); // export green data
            }
        } catch (IOException error) {
            buffer1 = null;
            buffer2 = null;
            errorCleanUp("Algorithm ObjectDistanceKruhlak reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer1 = null;
            buffer2 = null;
            errorCleanUp("Algorithm ObjectDistanceKruhlak reports: out of memory", true);

            return;
        }

        fireProgressStateChanged("Processing image ...");

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        shortMask = new short[totLength];

        for (i = 0; i < totLength; i++) {
            shortMask[i] = -1;
        }

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
            fireProgressStateChanged(100 * i / nVOIs);

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                shortMask = srcImage.generateVOIMask(shortMask, i);
                count1 = 0;
                count2 = 0;
                xPos1 = 0.0f;
                yPos1 = 0.0f;
                zPos1 = 0.0f;
                xPos2 = 0.0f;
                yPos2 = 0.0f;
                zPos2 = 0.0f;

                for (k = 0, z = 0; z < zDim; z++, k += sliceLength) {

                    for (j = k, y = 0; y < yDim; y++, j += xDim) {

                        for (x = 0; x < xDim; x++) {
                            index = x + j;

                            if (shortMask[index] == i) {

                                if (buffer1[index] >= threshold1) {
                                    xPos1 += buffer1[index] * x;
                                    yPos1 += buffer1[index] * y;
                                    zPos1 += buffer1[index] * z;
                                    count1 += buffer1[index];
                                }

                                if (buffer2[index] >= threshold2) {
                                    xPos2 += buffer2[index] * x;
                                    yPos2 += buffer2[index] * y;
                                    zPos2 += buffer2[index] * z;
                                    count2 += buffer2[index];
                                }
                            } // if (shortMask[index] == i)
                        } // for (x = 0; x < xDim; x++)
                    } // for (j = k, y = 0; y < yDim; y++, j += xDim)
                } // for (k = 0, z = 0; z < zDim; z++, k += sliceLength)

                xPos1 /= count1;
                yPos1 /= count1;
                zPos1 /= count1;
                xPos2 /= count2;
                yPos2 /= count2;
                zPos2 /= count2;
                centerToCenter = (float)
                                     Math.sqrt(((xPos1 - xPos2) * (xPos1 - xPos2) * xRes * xRes) +
                                               ((yPos1 - yPos2) * (yPos1 - yPos2) * yRes * yRes) +
                                               ((zPos1 - zPos2) * (zPos1 - zPos2) * zRes * zRes));

                if (useRed) {
                    UI.setDataText("VOI ID = " + i + " with red weighted voi center of mass = " + "(" + xPos1 + ", " +
                                   yPos1 + ", " + zPos1 + ")\n");
                } else {
                    UI.setDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos1 + ", " +
                                   yPos1 + ", " + zPos1 + ")\n");
                }

                if (useBlue) {
                    UI.setDataText("VOI ID = " + i + " with blue weighted voi center of mass = " + "(" + xPos2 + ", " +
                                   yPos2 + ", " + zPos2 + ")\n");
                    ;
                } else {
                    UI.setDataText("VOI ID = " + i + " with green weighted voi center of mass = " + "(" + xPos2 + ", " +
                                   yPos2 + ", " + zPos2 + ")\n");
                }

                if (unitsString != null) {
                    UI.setDataText("Center to center distance = " + centerToCenter + " " + unitsString + "\n");
                } else {
                    UI.setDataText("Center to center distance = " + centerToCenter + "\n");
                }
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        fireProgressStateChanged(100);

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

}
