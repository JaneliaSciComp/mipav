import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * @version  March 10, 2005
 * @author   William Gandler
 * @see      AlgorithmBase This plug in prints out the two axis lengths of 2D ellipses fitted to the areas of contour
 *           VOIs within single slices and the three axis lengths of 3D ellipses fitted to the volumes of contour VOIs
 *           extending over more than 1 slice. In 3D the algorithm works as follows: 1.) Calculate the first order
 *           centroid or center of mass of the volume contained in the VOI. Pixels are not weighted by gray scale value.
 *           2.) Calculate the normalized, second order moment or tensor matrix. The tensor matrix is a symmetric 3 by 3
 *           matrix. Ixx Ixy Izx Ixy Iyy Iyz Izx Iyz Izz 3.) Solve for the eigenvalues and eigenvectors of the tensor
 *           matrix. 4.) The ellipsoid semiaxes are proportional to the square roots of the eigenvalues, so take the
 *           square roots of the eigenvalues. 5.) Normalize the square roots of the eigenvalues using the actual number
 *           of pixels contained in the VOI. That is, If volume ellipse = (4/3) * PI * sqrt(evalue[0]) * sqrt(evalue[1])
 *           * sqrt(evalue[2]) Then the normalizing factor to multiply each sqrt(evalue) by = (number of pixels/ volume
 *           ellipse)**(1/3) The largest ellipsoid diameter = 2.0 * normalizing factor * sqrt(largest evalue)
 *
 *           <p>Reference: Object Quantification by Simon Xin Wang at
 *           http://www.caip.rutgers.edu/~xswang/object-segment/objhtml/node4.html</p>
 */
public class PlugInAlgorithmFibroid extends AlgorithmBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInAlgorithmFibroid object.
     *
     * @param  srcImg  Source image model.
     */
    public PlugInAlgorithmFibroid(ModelImage srcImg) {
        super(null, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
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
    @SuppressWarnings("null")
    private void calc2D() {

        ViewUserInterface UI = ViewUserInterface.getReference();
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        int xUnits = srcImage.getUnitsOfMeasure()[0];
        int yUnits = srcImage.getUnitsOfMeasure()[1];
        String unitsString = null;
        int i;
        int k;
        float[] pAxis = new float[1];
        float[] eccentricity = new float[1];
        float[] majorAxis = new float[1];
        float[] minorAxis = new float[1];
        double[] angleAxislsq = new double[1];
        double[] eccentricitylsq = new double[1];
        double[] majorAxislsq = new double[1];
        double[] minorAxislsq = new double[1];
        double[] xCenterlsq = new double[1];
        double[] yCenterlsq = new double[1];
        FileInfoBase[] fileInfo;
        String patientName;
        String patientID;
        String studyDate;
        String studyTime;
        String studyDescription;
        String fileDirectory = srcImage.getFileInfo(0).getFileDirectory();

        // Default names if no dicom information is available
        String fileName = "Data.txt";
        String voiName = "defaultVOIs_s" + File.separator;
        File file;
        RandomAccessFile raFile;
        String dataString = null;


        ViewVOIVector VOIs = null;
        int nVOIs;
        Vector<VOIBase> contours;
        int nContours;

        if ((xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        fireProgressStateChanged("Measuring ellipsoid diameters ...");


        fileInfo = srcImage.getFileInfo();

        if (fileInfo[0] instanceof FileInfoDicom) {

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0010") != null) {
                    patientName = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0010"));
                    UI.setDataText("Patient Name = " + patientName + "\n");
                    dataString += "Patient Name = " + patientName + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient Name = \n");
                Preferences.debug("Tag (0010,0010) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0020") != null) {
                    patientID = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0020"));
                    UI.setDataText("Patient ID = " + patientID + "\n");
                    dataString += "Patient ID = " + patientID + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient ID = \n");
                Preferences.debug("Tag (0010,0020) was not found.\n");
            }


            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0020") != null) {
                    studyDate = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0020"));
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    fileName = "Data";
                    voiName = "defaultVOIs_";

                    for (i = 0; i < studyDate.length(); i++) {

                        if ((studyDate.charAt(i) != '/') && (studyDate.charAt(i) != '\0')) {
                            fileName += studyDate.charAt(i);
                            voiName += studyDate.charAt(i);
                        }
                    }
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Date = \n");
                Preferences.debug("Tag (0008,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0030") != null) {
                    studyTime = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0030"));
                    UI.setDataText("Study Time = " + studyTime + "\n");
                    dataString += "Study Time = " + studyTime + "\n";

                    for (i = 0; i < studyTime.length(); i++) {

                        if ((studyTime.charAt(i) != ':') && (studyTime.charAt(i) != '\0')) {
                            fileName += studyTime.charAt(i);
                            voiName += studyTime.charAt(i);
                        }
                    }

                    fileName = fileName + ".txt";
                    voiName = voiName + File.separator;
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Time = \n");
                Preferences.debug("Tag (0008,0030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,1030") != null) {
                    studyDescription = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,1030"));
                    UI.setDataText("Study Description = " + studyDescription + "\n");
                    dataString += "Study Description = " + studyDescription + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Description = \n");
                Preferences.debug("Tag (0008,1030) was not found.\n");
            }
        } // if ( fileInfo[0] instanceof FileInfoDicom )

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        UI.setDataText("FIT WITH FIRST AND SECOND MOMENTS\n");
        dataString += "FIT WITH FIRST AND SECOND MOMENTS\n";

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                contours = VOIs.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = 0; k < nContours; k++) {
                    /*
                    ((VOIContour) (contours[0].elementAt(k))).secondOrderAttributes(xDim, yDim, xRes, yRes, xUnits,
                                                                                    yUnits, pAxis, eccentricity,
                                                                                    majorAxis, minorAxis); */

                    ((VOIContour) (contours.elementAt(k))).secondOrderAttributes(srcImage, pAxis, eccentricity,
                                                                                    majorAxis, minorAxis);

                    if (nContours > 1) {
                        UI.setDataText(" Element number = " + k + "\n");
                        dataString += " Element number = " + k + "\n";
                    }

                    if (unitsString != null) {
                        UI.setDataText(" Axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + " " + unitsString +
                                       "\n");
                        dataString += " Axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + " " + unitsString + "\n";
                    } else {
                        UI.setDataText(" Axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + "\n");
                        dataString += " Axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + "\n";

                    }
                } // for (k = 0; k < nContours; k++)
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        UI.setDataText("\nFIT WITH PERIMITER\n");
        dataString += "\nFIT WITH PERIMITER\n";

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                contours = VOIs.VOIAt(i).getCurves();
                nContours = contours.size();

                for (k = 0; k < nContours; k++) {
                    ((VOIContour) (contours.elementAt(k))).secondOrderAttributeslsq(xRes, yRes, xUnits, yUnits,
                                                                                       angleAxislsq, eccentricitylsq, majorAxislsq,
                                                                                       minorAxislsq, xCenterlsq, yCenterlsq);

                    if (nContours > 1) {
                        UI.setDataText(" Element number = " + k + "\n");
                        dataString += " Element number = " + k + "\n";
                    }

                    if (unitsString != null) {
                        UI.setDataText(" Axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + " " + unitsString +
                                       "\n");
                        dataString += " Axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + " " + unitsString + "\n";
                    } else {
                        UI.setDataText(" Axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + "\n");
                        dataString += " Axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + "\n";

                    }
                } // for (k = 0; k < nContours; k++)
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        file = new File(fileDirectory + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNotFoundException " + e);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e);
        }

        ViewUserInterface.getReference().getFrameContainingImage(srcImage).saveAllVOIsTo(fileDirectory +
                                                                                         File.separator + voiName);
        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings({ "null", "unchecked" })
    private void calc3D() {

        int i, j, k, m, n;
        int x, y, z;
        int offset;
        int index;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
        float zRes = srcImage.getResolutions(0)[2];
        float[] pAxis = new float[1];
        float[] eccentricity = new float[1];
        float[] majorAxis = new float[1];
        float[] minorAxis = new float[1];
        double[] angleAxislsq = new double[1];
        double[] eccentricitylsq = new double[1];
        double[] majorAxislsq = new double[1];
        double[] minorAxislsq = new double[1];
        double[] xCenterlsq = new double[1];
        double[] yCenterlsq = new double[1];
        FileInfoBase[] fileInfo;
        String patientName;
        String patientID;
        String studyDate;
        String studyTime;
        String studyDescription;
        String fileDirectory = srcImage.getFileInfo(0).getFileDirectory();

        // Default names if no dicom information is available
        String fileName = "Data.txt";
        String voiName = "defaultVOIs_s" + File.separator;
        File file;
        RandomAccessFile raFile;
        String dataString = null;


        ViewVOIVector VOIs = null;
        int nVOIs;
        Vector<VOIBase>[] contours;
        int nContours;
        int nSlices;
        BitSet mask;
        int maxElement;
        double xdiff;
        double ydiff;
        double zdiff;
        boolean useRes3D;
        int usedSlices;
        double[][] tensor = new double[3][3];
        double[] eigenvalue = new double[3];
        double[][] eigenvector = new double[3][3];
        double temp;
        float tempf;
        double[] tempCol = new double[3];
        double ellipVol;
        double normFactor;

        ViewUserInterface UI = ViewUserInterface.getReference();

        int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
        String unitsString = null;
        String unitsString3D = null;
        float scaleMax;
        float invMax;
        Polygon gon;
        Vector3f kVoxel;
        float[] ellipseLength = new float[3];

        if ((xUnits == yUnits) && (xUnits == zUnits)) {
            useRes3D = true;
        } else {
            useRes3D = false;
        }

        if ((xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        if ((xUnits == yUnits) && (xUnits == zUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
            unitsString3D = (Unit.getUnitFromLegacyNum(xUnits)).toString();
        }

        fireProgressStateChanged("Measuring ellipsoid diameters ...");


        fileInfo = srcImage.getFileInfo();

        if (fileInfo[0] instanceof FileInfoDicom) {

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0010") != null) {
                    patientName = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0010"));
                    UI.setDataText("Patient Name = " + patientName + "\n");
                    dataString += "Patient Name = " + patientName + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient Name = \n");
                Preferences.debug("Tag (0010,0010) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0020") != null) {
                    patientID = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0020"));
                    UI.setDataText("Patient ID = " + patientID + "\n");
                    dataString += "Patient ID = " + patientID + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient ID = \n");
                Preferences.debug("Tag (0010,0020) was not found.\n");
            }


            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0020") != null) {
                    studyDate = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0020"));
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    fileName = "Data";
                    voiName = "defaultVOIs_";

                    for (i = 0; i < studyDate.length(); i++) {

                        if ((studyDate.charAt(i) != '/') && (studyDate.charAt(i) != '\0')) {
                            fileName += studyDate.charAt(i);
                            voiName += studyDate.charAt(i);
                        }
                    }
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Date = \n");
                Preferences.debug("Tag (0008,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0030") != null) {
                    studyTime = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0030"));
                    UI.setDataText("Study Time = " + studyTime + "\n");
                    dataString += "Study Time = " + studyTime + "\n";
                    fileName = fileName + "_";
                    voiName = voiName + "_";

                    for (i = 0; i < studyTime.length(); i++) {

                        if ((studyTime.charAt(i) != ':') && (studyTime.charAt(i) != '\0')) {
                            fileName += studyTime.charAt(i);
                            voiName += studyTime.charAt(i);
                        }
                    }

                    fileName = fileName + ".txt";
                    voiName = voiName + File.separator;
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Time = \n");
                Preferences.debug("Tag (0008,0030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,1030") != null) {
                    studyDescription = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,1030"));
                    UI.setDataText("Study Description = " + studyDescription + "\n");
                    dataString += "Study Description = " + studyDescription + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Description = \n");
                Preferences.debug("Tag (0008,1030) was not found.\n");
            }
        } // if ( fileInfo[0] instanceof FileInfoDicom )

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        UI.setDataText("FIT WITH FIRST AND SECOND MOMENTS\n");
        dataString += "FIT WITH FIRST AND SECOND MOMENTS\n";

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                contours = VOIs.VOIAt(i).getSortedCurves(srcImage.getExtents()[2]);
                nSlices = contours.length;
                usedSlices = 0;
                maxElement = 0;

                for (z = 0; z < nSlices; z++) {

                    if (contours[z].size() > maxElement) {
                        maxElement = contours[z].size();
                    }

                    if (contours[z].size() >= 1) {
                        usedSlices++;
                    }
                } // for (z = 0; z < nSlices; z++)

                int[] volume = new int[maxElement];

                // centroids
                double[] cx = new double[maxElement];
                double[] cy = new double[maxElement];
                double[] cz = new double[maxElement];

                // Second order moments
                double[] ixx = new double[maxElement];
                double[] iyy = new double[maxElement];
                double[] izz = new double[maxElement];
                double[] iyz = new double[maxElement];
                double[] ixy = new double[maxElement];
                double[] izx = new double[maxElement];

                for (z = 0; z < nSlices; z++) {
                    nContours = contours[z].size();

                    if (nContours >= 1) {
                        UI.setDataText(" Slice number = " + (z + 1) + "\t");
                        dataString += " Slice number = " + (z + 1) + "\t";
                    }

                    for (k = 0; k < nContours; k++) {
                        /*
                        ((VOIContour) (contours[z].elementAt(k))).secondOrderAttributes(xDim, yDim, xRes, yRes, xUnits,
                                                                                        yUnits, pAxis, eccentricity,
                                                                                        majorAxis, minorAxis); */

                        ((VOIContour) (contours[z].elementAt(k))).secondOrderAttributes(srcImage, pAxis, eccentricity,
                                                                                        majorAxis, minorAxis);

                        if (nContours > 1) {
                            UI.setDataText(" Element number = " + k + "\n");
                            dataString += " Element number = " + k + "\n";
                        }

                        if (usedSlices > 1) {
                            mask = VOIs.VOIAt(i).createBinaryMask(xDim, yDim, z, contours[z].elementAt(k));

                            for (y = 0; y < yDim; y++) {
                                offset = y * xDim;

                                for (x = 0; x < xDim; x++) {

                                    if (mask.get(offset + x)) {
                                        volume[k]++;

                                        if (useRes3D) {
                                            cx[k] += x * xRes;
                                            cy[k] += y * yRes;
                                            cz[k] += z * zRes;
                                        } else {
                                            cx[k] += x;
                                            cy[k] += y;
                                            cz[k] += z;
                                        }
                                    }
                                } // for (x = 0; x < xDim; x++)
                            } // for (y = 0; y < yDim; y++)
                        } // if (usedSlices > 1)

                        if (unitsString != null) {
                            UI.setDataText(" 2D axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + " " +
                                           unitsString + "\n");
                            dataString += " 2D axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + " " + unitsString +
                                          "\n";
                        } else {
                            UI.setDataText(" 2D axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + "\n");
                            dataString += " 2D axes    \t= " + majorAxis[0] + ",  " + minorAxis[0] + "\n";
                        }
                    } // for (k = 0; k < nContours; k++)
                } // for (z = 0; z < nSlices; z++)

                if (usedSlices > 1) {

                    for (k = 0; k < maxElement; k++) {
                        cx[k] = cx[k] / volume[k];
                        cy[k] = cy[k] / volume[k];
                        cz[k] = cz[k] / volume[k];
                    } // for (k = 0; k < maxElement; k++)

                    for (z = 0; z < nSlices; z++) {
                        nContours = contours[z].size();

                        for (k = 0; k < nContours; k++) {
                            mask = VOIs.VOIAt(i).createBinaryMask(xDim, yDim, z, contours[z].elementAt(k));

                            for (y = 0; y < yDim; y++) {
                                offset = y * xDim;

                                for (x = 0; x < xDim; x++) {

                                    if (mask.get(offset + x)) {

                                        if (useRes3D) {
                                            xdiff = (x * xRes) - cx[k];
                                            ydiff = (y * yRes) - cy[k];
                                            zdiff = (z * zRes) - cz[k];
                                        } else {
                                            xdiff = x - cx[k];
                                            ydiff = y - cy[k];
                                            zdiff = z - cz[k];
                                        }

                                        ixx[k] += xdiff * xdiff;
                                        iyy[k] += ydiff * ydiff;
                                        izz[k] += zdiff * zdiff;
                                        iyz[k] += ydiff * zdiff;
                                        ixy[k] += xdiff * ydiff;
                                        izx[k] += zdiff * xdiff;
                                    }
                                } // for (x = 0; x < xDim; x++)
                            } // for (y = 0; y < yDim; y++)
                        } // for (k = 0; k < nContours; k++)
                    } // for (z = 0; z < nSlices; z++)

                    for (k = 0; k < maxElement; k++) {
                        ixx[k] = ixx[k] / volume[k];
                        iyy[k] = iyy[k] / volume[k];
                        izz[k] = izz[k] / volume[k];
                        iyz[k] = iyz[k] / volume[k];
                        ixy[k] = ixy[k] / volume[k];
                        izx[k] = izx[k] / volume[k];
                        tensor[0][0] = ixx[k];
                        tensor[0][1] = ixy[k];
                        tensor[0][2] = izx[k];
                        tensor[1][0] = ixy[k];
                        tensor[1][1] = iyy[k];
                        tensor[1][2] = iyz[k];
                        tensor[2][0] = izx[k];
                        tensor[2][1] = iyz[k];
                        tensor[2][2] = izz[k];
                        // In EigenvalueDecomposition the columns represent the
                        // eigenvectors
                        Eigenvalue.decompose(tensor, eigenvector, eigenvalue);

                        // Arrange the eigenvalues and corresponding eigenvectors
                        // in descending order so that e0 >= e1 >= e2
                        for (m = 0; m < 3; m++) {
                            index = m;

                            for (n = m + 1; n < 3; n++) {

                                if (eigenvalue[n] > eigenvalue[m]) {
                                    index = n;
                                }
                            } // for (m = m+1; n < 3; n++)

                            if (index != m) {
                                temp = eigenvalue[m];
                                eigenvalue[m] = eigenvalue[index];
                                eigenvalue[index] = temp;

                                for (n = 0; n < 3; n++) {
                                    tempCol[n] = eigenvector[n][m];
                                    eigenvector[n][m] = eigenvector[n][index];
                                    eigenvector[n][index] = tempCol[n];
                                }
                            } // if (index != m)
                        } // for (m = 0; m < 3; m++)

                        // Semi axes are proportional to the square root of the
                        // eigenvalues
                        for (m = 0; m < 3; m++) {
                            eigenvalue[m] = Math.sqrt(eigenvalue[m]);
                        }

                        // Caclulate an unnormalized volume from the eigenvalues
                        ellipVol = (4.0 / 3.0) * Math.PI * eigenvalue[0] * eigenvalue[1] * eigenvalue[2];

                        // Calculate a normalizing factor with the actual volume
                        if (useRes3D) {
                            normFactor = Math.pow((xRes * yRes * zRes * volume[k] / ellipVol), (1.0 / 3.0));
                        } else {
                            normFactor = Math.pow((volume[k] / ellipVol), (1.0 / 3.0));
                        }

                        // Normalize to obtain the actual semi axes
                        for (m = 0; m < 3; m++) {
                            eigenvalue[m] = normFactor * eigenvalue[m];
                        }

                        if (maxElement > 1) {
                            UI.setDataText(" Element number = " + k + "\n");
                            dataString += " Element number = " + k + "\n";
                        }

                        if (unitsString3D != null) {
                            UI.setDataText(" 3D axes    \t= " + (float) (2.0 * eigenvalue[0]) + ",  " +
                                           (float) (2.0 * eigenvalue[1]) + ",  " + (float) (2.0 * eigenvalue[2]) + " " +
                                           unitsString3D + "\n");
                            dataString += " 3D axes    \t= " + (float) (2.0 * eigenvalue[0]) + ",  " +
                                          (float) (2.0 * eigenvalue[1]) + ",  " + (float) (2.0 * eigenvalue[2]) + " " +
                                          unitsString3D + "\n";
                        } else {
                            UI.setDataText(" 3D axes    \t= " + (float) (2.0 * eigenvalue[0]) + ",  " +
                                           (float) (2.0 * eigenvalue[1]) + ",  " + (float) (2.0 * eigenvalue[2]) +
                                           "\n");
                            dataString += " 3D axes    \t= " + (float) (2.0 * eigenvalue[0]) + ",  " +
                                          (float) (2.0 * eigenvalue[1]) + ",  " + (float) (2.0 * eigenvalue[2]) + "\n";

                        }
                    } // for (k = 0; k < maxElement; k++)
                } // if (usedSlices > 1)
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)


        if (threadStopped) {
            finalize();

            return;
        }

        UI.setDataText("\nFIT WITH PERIMITER\n");
        dataString += "\nFIT WITH PERIMITER\n";

        if (useRes3D) {
            scaleMax = xRes * (xDim - 1);
            scaleMax = Math.max(scaleMax, yRes * (yDim - 1));
            scaleMax = Math.max(scaleMax, zRes * (zDim - 1));
        } else {
            scaleMax = xDim - 1;
            scaleMax = Math.max(scaleMax, yDim - 1);
            scaleMax = Math.max(scaleMax, zDim - 1);
        }

        invMax = 1.0f / scaleMax;

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                contours = VOIs.VOIAt(i).getSortedCurves(srcImage.getExtents()[2]);
                nSlices = contours.length;
                usedSlices = 0;
                maxElement = 0;

                for (z = 0; z < nSlices; z++) {

                    if (contours[z].size() > maxElement) {
                        maxElement = contours[z].size();
                    }

                    if (contours[z].size() >= 1) {
                        usedSlices++;
                    }
                } // for (z = 0; z < nSlices; z++)

                Vector<Vector3f>[] volPoints = new Vector[maxElement];

                for (k = 0; k < maxElement; k++) {
                    volPoints[k] = new Vector<Vector3f>();
                }

                for (z = 0; z < nSlices; z++) {
                    nContours = contours[z].size();

                    if (nContours >= 1) {
                        UI.setDataText(" Slice number = " + (z + 1) + "\t");
                        dataString += " Slice number = " + (z + 1) + "\t";
                    }

                    for (k = 0; k < nContours; k++) {
                        ((VOIContour) (contours[z].elementAt(k))).secondOrderAttributeslsq(xRes, yRes, xUnits, yUnits,
                                                                                           angleAxislsq, eccentricitylsq,
                                                                                           majorAxislsq, minorAxislsq,
                                                                                           xCenterlsq, yCenterlsq);

                        if (nContours > 1) {
                            UI.setDataText(" Element number = " + k + "\n");
                            dataString += " Element number = " + k + "\n";
                        }

                        if (usedSlices > 1) {
                            gon = ((VOIContour) (contours[z].elementAt(k))).exportPolygon();

                            for (j = 0; j < gon.npoints; j++) {

                                if (useRes3D) {
                                    kVoxel = new Vector3f(gon.xpoints[j] * xRes * invMax,
                                                          gon.ypoints[j] * yRes * invMax, z * zRes * invMax);
                                } else {
                                    kVoxel = new Vector3f(gon.xpoints[j] * invMax, gon.ypoints[j] * invMax, z * invMax);
                                }

                                volPoints[k].add(kVoxel);
                            }
                        } // if (usedSlices > 1)

                        if (unitsString != null) {
                            UI.setDataText(" 2D axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + " " +
                                           unitsString + "\n");
                            dataString += " 2D axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + " " + unitsString +
                                          "\n";
                        } else {
                            UI.setDataText(" 2D axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + "\n");
                            dataString += " 2D axes    \t= " + majorAxislsq[0] + ",  " + minorAxislsq[0] + "\n";
                        }
                    } // for (k = 0; k < nContours; k++)
                } // for (z = 0; z < nSlices; z++)

                if (usedSlices > 1) {

                    for (k = 0; k < maxElement; k++) {
                        AlgorithmEllipsoidFit kF = new AlgorithmEllipsoidFit(volPoints[k]);
                        double[] axes = kF.getAxes();

                        ellipseLength[0] = (float) (axes[0] * scaleMax);
                        ellipseLength[1] = (float) (axes[1] * scaleMax);
                        ellipseLength[2] = (float) (axes[2] * scaleMax);

                        // Arrange the ellipse lengths in descending order
                        for (m = 0; m < 3; m++) {
                            index = m;

                            for (n = m + 1; n < 3; n++) {

                                if (ellipseLength[n] > ellipseLength[index]) {
                                    index = n;
                                }
                            } // for (m = m+1; n < 3; n++)

                            if (index != m) {
                                tempf = ellipseLength[m];
                                ellipseLength[m] = ellipseLength[index];
                                ellipseLength[index] = tempf;
                            } // if (index != m)
                        } // for (m = 0; m < 3; m++)

                        if (maxElement > 1) {
                            UI.setDataText(" Element number = " + k + "\n");
                            dataString += " Element number = " + k + "\n";
                        }

                        if (unitsString3D != null) {
                            UI.setDataText(" 3D axes    \t= " + ellipseLength[0] + ",  " + ellipseLength[1] + ",  " +
                                           ellipseLength[2] + " " + unitsString3D + "\n");
                            dataString += " 3D axes    \t= " + ellipseLength[0] + ",  " + ellipseLength[1] + ",  " +
                                          ellipseLength[2] + " " + unitsString3D + "\n";
                        } else {
                            UI.setDataText(" 3D axes    \t= " + ellipseLength[0] + ",  " + ellipseLength[1] + ",  " +
                                           ellipseLength[2] + "\n");
                            dataString += " 3D axes    \t= " + ellipseLength[0] + ",  " + ellipseLength[1] + ",  " +
                                          ellipseLength[2] + "\n";

                        }
                    } // for (k = 0; k < maxElement; k++)
                } // if (usedSlices > 1)
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)


        if (threadStopped) {
            finalize();

            return;
        }

        file = new File(fileDirectory + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNotFoundException " + e);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e);
        }

        ViewUserInterface.getReference().getFrameContainingImage(srcImage).saveAllVOIsTo(fileDirectory +
                                                                                         File.separator + voiName);


        setCompleted(true);
    }
}
