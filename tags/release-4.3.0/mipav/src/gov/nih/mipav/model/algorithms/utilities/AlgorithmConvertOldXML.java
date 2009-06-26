package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Recursively traverses a directory and its subfolders, converting all 3D DICOM files to AVI with MP42 Compression.
 *
 * @author   Ben Link
 * @version  1.0
 */
public class AlgorithmConvertOldXML extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[][] coronal2dfix = {
        { 1, 0, 0 },
        { 0, 0, -1 },
        { 0, 1, 0 }
    };

    /** DOCUMENT ME! */
    private double[][] coronal3dfix = {
        { 1, 0, 0, 0 },
        { 0, 0, -1, 0 },
        { 0, 1, 0, 0 },
        { 0, 0, 0, 1 }
    };

    /** DOCUMENT ME! */
    private String dirPath; // directory to recursively operate in

    /** DOCUMENT ME! */
    private int numConverted = 0; // number of old xml files that were converted

    /** DOCUMENT ME! */
    private String outString = new String();

    /** DOCUMENT ME! */
    private double[][] sagittal2dfix = {
        { 0, 0, -1 },
        { 1, 0, 0 },
        { 0, -1, 0 }
    };

    /** DOCUMENT ME! */
    private double[][] sagittal2dold = {
        { 0, 1, 0 },
        { 0, 0, -1 },
        { -1, 0, 0 }
    };

    /** DOCUMENT ME! */
    private double[][] sagittal3dfix = {
        { 0, 0, -1, 0 },
        { 1, 0, 0, 0 },
        { 0, -1, 0, 0 },
        { 0, 0, 0, 1 }
    };

    /** DOCUMENT ME! */
    private double[][] sagittal3dold = {
        { 0, 1, 0, 0 },
        { 0, 0, -1, 0 },
        { -1, 0, 0, 0 },
        { 0, 0, 0, 1 }
    };

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default Constructor.
     *
     * @param  dir  full pathname of directory to traverse
     */
    public AlgorithmConvertOldXML(String dir) {
        this.dirPath = dir;

        // clip off the trailing file separator
        if (dirPath.endsWith(File.separator)) {
            dirPath = dirPath.substring(0, dirPath.length() - 1);
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        Vector<String> fileVector = new Vector<String>();

        System.err.println("Top directory is: " + dirPath);
        addFilesToVector(dirPath, fileVector);

        int size = fileVector.size();

        System.err.println("Number of XML headers to be tested: " + size);

        for (int i = 0; i < size; i++) {

            // System.err.println("Converting: " + i);
            runConversion(fileVector.elementAt(i));
        }

        System.err.println("Number of XML headers converted: " + numConverted);
    }

    /**
     * Recursively adds file and directory paths to a Vector.
     *
     * @param  name  The name of either file or directory
     * @param  vec   Vector that holds all files to be processed
     */
    private void addFilesToVector(String name, Vector<String> vec) {
        File tempFile = new File(name);

        if (tempFile.isDirectory()) {
            String[] fileList = tempFile.list();

            for (int i = 0; i < fileList.length; i++) {
                addFilesToVector(name + File.separator + fileList[i], vec);
            }
            // vec.add(0, name);
        } else if (name.endsWith(".xml") || name.endsWith(".XML")) {
            vec.add(name);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileName  DOCUMENT ME!
     */
    private void runConversion(String fileName) {

        File xmlFile = new File(fileName);

        BufferedReader bReader = null;
        boolean isOldXML = false;
        boolean isSagittal = false;
        boolean isCoronal = false;

        String inString = null;

        outString = "";

        boolean hasCompression = false;
        boolean includeString = true;

        boolean foundResolution = false;
        int resCount = 0;

        int extents = 0;

        float[] resolutions = null;

        String tempResString;

        int numResToCopy = 1;

        try {
            bReader = new BufferedReader(new FileReader(xmlFile));

            int index = -1;

            inString = bReader.readLine();

            while (inString != null) {
                includeString = true;

                index = inString.indexOf("Start-locations");

                while (index != -1) {
                    isOldXML = true;
                    inString = inString.substring(0, index) + "Origin" +
                               inString.substring(index + 15, inString.length());
                    index = inString.indexOf("Start-locations");
                }

                if (hasCompression && (inString.indexOf("<Compression>") != -1)) {
                    isOldXML = true;
                    includeString = false;
                }

                if (inString.indexOf("<Extents>") != -1) {
                    extents++;

                    if (extents > 2) {

                        try {
                            numResToCopy *= Integer.parseInt(inString.substring(inString.indexOf(">") + 1,
                                                                                inString.lastIndexOf("<")));
                            // System.err.println("NUM RES TO COPY: " + numResToCopy);
                        } catch (Exception e) {
                            System.err.println("error");
                        }
                    }
                }

                if ((inString.indexOf("<Resolutions>") != -1) && (inString.indexOf("</Resolutions>") == -1)) {
                    foundResolution = true;
                }

                if (inString.indexOf("<Resolutions>") != -1) {

                    if (!foundResolution) {
                        isOldXML = true;
                        includeString = false;

                        if (resolutions == null) {
                            resolutions = new float[extents];
                        }

                        tempResString = inString.substring(inString.indexOf(">") + 1, inString.lastIndexOf("<"));

                        // System.err.println(tempResString);
                        // System.err.println("Found tempresstring: " + tempResString);
                        try {
                            resolutions[resCount] = Float.parseFloat(tempResString);
                        } catch (Exception ex) { // ex.printStackTrace();
                        }

                        resCount++;

                        if (resCount == extents) {
                            inString = "";
                            inString += "\t\t<Resolutions>\n";

                            for (int j = 0; j < extents; j++) {
                                inString += "\t\t\t<Resolution>" + Float.toString(resolutions[j]) + "</Resolution>\n";
                            }

                            inString += "\t\t</Resolutions>\n";
                            inString = inString.substring(0, inString.length() - 1);

                            // System.err.println(inString);
                            includeString = true;
                        }
                    }
                }

                if (inString.indexOf("<Compression>") != -1) {
                    hasCompression = true;
                }

                if (!hasCompression && (inString.indexOf("<Orientation>") != -1)) {
                    isOldXML = true;
                    inString = "\t\t<Compression>none</Compression>\n" + inString;
                }

                // do some sagittal matrix fixing (if needed)
                if (inString.indexOf("<Orientation>") != -1) {
                    String tempStr = inString.substring(15, inString.indexOf("</Orientation>"));

                    if (tempStr.equalsIgnoreCase("sagittal")) {
                        isSagittal = true;
                    } else if (tempStr.equalsIgnoreCase("coronal")) {
                        isCoronal = true;
                    }
                }

                // see if the old matrix type is used
                if ((inString.indexOf("<Matrix>") != -1) && (inString.indexOf("</Matrix>") != -1)) {

                    // System.err.println("Found old matrix for: " + fileName + " extents are: " + extents);
                    isOldXML = true;

                    String matValue = "";
                    String tempString = new String(inString);

                    inString = "\t\t<Matrix>\n\t\t\t<Transform-ID>Unknown</Transform-ID>\n";
                    matValue = tempString.substring(tempString.indexOf("<Matrix>") + 8,
                                                    tempString.indexOf("</Matrix>"));
                    inString += "\t\t\t<Data>" + matValue + "</Data>\n";

                    int dims = (extents + 1) * (extents + 1);

                    for (int i = 0; i < (dims - 1); i++) {
                        tempString = bReader.readLine();
                        matValue = tempString.substring(tempString.indexOf("<Matrix>") + 8,
                                                        tempString.indexOf("</Matrix>"));
                        inString += "\t\t\t<Data>" + matValue + "</Data>\n";
                    }

                    inString += "\t\t</Matrix>";
                    // System.err.println(inString);
                }

                // look for sagittal and coronal with incorrect identity matrix
                if ((inString.indexOf("<Matrix>") != -1) && (inString.indexOf("</Matrix>") == -1) &&
                        (isSagittal || isCoronal)) {

                    // System.err.println("IS SAGITTAL OR CORONAL");
                    String matrixLine = new String(inString);
                    String tID = bReader.readLine();

                    int matDim = extents + 1;

                    if (extents > 3) {
                        matDim = 4;
                    }

                    TransMatrix mat = new TransMatrix(matDim);

                    for (int i = 0; i < matDim; i++) {

                        for (int j = 0; j < matDim; j++) {
                            inString = bReader.readLine();

                            // System.err.println(inString.substring(9, inString.indexOf("</Data>")));
                            mat.set(i, j, Double.parseDouble(inString.substring(9, inString.indexOf("</Data>"))));
                        }
                    }

                    if (mat.isIdentity()) {
                        isOldXML = true;

                        if (isSagittal) {

                            if (extents == 2) {
                                mat.copyMatrix(sagittal2dfix);
                            } else if ((extents == 3) || (extents == 4)) {
                                mat.copyMatrix(sagittal3dfix);
                            }
                        } // coronal
                        else {

                            if (extents == 2) {
                                mat.copyMatrix(coronal2dfix);
                            } else if ((extents == 3) || (extents == 4)) {
                                mat.copyMatrix(coronal3dfix);
                            }
                        }

                        outString += matrixLine + "\n";
                        outString += "\t\t\t<Transform-ID>Scanner Anatomical</Transform-ID>\n";

                        for (int i = 0; i < matDim; i++) {

                            for (int j = 0; j < matDim; j++) {
                                outString += "\t\t\t<Data>" + mat.get(i, j) + "</Data>\n";
                            }
                        }

                        // add on the end matrix tag
                        outString += bReader.readLine() + "\n";

                        includeString = false;
                    } else if (isSagittal) {

                        boolean changeMat = false;
                        //double[][] tempDoub = mat.getMatrix();

                        if (extents == 2) {

                            for (int a = 0; a < 2; a++) {

                                for (int b = 0; b < 2; b++) {

                                    if (mat.get(a, b) != sagittal2dold[a][b]) {
                                        return;
                                    }
                                }
                            }
                        } else {

                            for (int a = 0; a < 3; a++) {

                                for (int b = 0; b < 3; b++) {

                                    if (mat.get(a, b) != sagittal3dold[a][b]) {
                                        return;
                                    }
                                }
                            }
                        }

                        isOldXML = true;

                        if (extents == 2) {
                            mat.copyMatrix(sagittal2dfix);
                        } else {
                            mat.copyMatrix(sagittal3dfix);
                        }

                        outString += matrixLine + "\n";
                        outString += "\t\t\t<Transform-ID>Scanner Anatomical</Transform-ID>\n";

                        for (int i = 0; i < matDim; i++) {

                            for (int j = 0; j < matDim; j++) {
                                outString += "\t\t\t<Data>" + mat.get(i, j) + "</Data>\n";
                            }
                        }

                        // add on the end matrix tag
                        outString += bReader.readLine() + "\n";

                        includeString = false;

                    }

                }

                if (includeString) {
                    outString += inString + "\n";
                }

                inString = bReader.readLine();
            }

            bReader.close();
            bReader = null;

            // System.err.println("IS OLD XML: " + isOldXML);

            if (isOldXML) {

                if (!xmlFile.canWrite()) {
                    System.err.println(fileName +
                                       " needs to be converted but is write-protected.  Please modify write access.");

                    return;
                }

                numConverted++;

                PrintWriter pw = new PrintWriter(new FileWriter(xmlFile));

                pw.write(outString);
                pw.close();
            }

        } catch (Exception e) {
            return;
            // blah
        }

    }

}
