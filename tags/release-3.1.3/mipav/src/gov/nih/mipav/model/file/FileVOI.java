package gov.nih.mipav.model.file;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.awt.*;

import java.io.*;

import java.net.*;

import java.util.*;

import javax.swing.*;

import javax.xml.parsers.*;


/**
 * VOI reader/writer. This classes incorporates the ability to write/read VOIs in three formats (MIPAV *.voi, Nuages,
 * and MIPAV *.xml . The first is a simple text format in which the points of the VOIs are stored in text format. The
 * VOIs can be also stored in a second format, also a text format, termed a Nauges. Nauges is a program that forms
 * triangles (i.e., tesselates) from contours. Therefore, if one wishes to render a surface of a VOI, first save the VOI
 * in Nauges format and supply the VOI file to the Nauges executable. MIPAV's lastest file format is based on XML and
 * should be used in the future.
 *
 * <p>Note: A single VOI per file. However, a VOI can be formed from multiple contours.</p>
 *
 * @version  0.1 Feb 24, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      VOI
 */
public class FileVOI extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The tab character (makes writing out files cleaner-looking). */
    private static final String TAB = "\t";

    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /** The charset the XML file is written in. */
    private static final String XML_ENCODING = "UTF-8";

    /** The XML version description header. */
    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";

    /** The MIPAV XML header comment. */
    private static final String VOI_HEADER = "<!-- MIPAV VOI file -->";

    private static final String TEXT_HEADER = "<!-- MIPAV Annotation file -->";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File reference. */
    private File file;

    /** File directory where the VOI is to read or written. */
    private String fileDir;

    /** File name of the VOI. */
    private String fileName;

    /** Image used to reference properties of the image to aid reading and writing of the VOI. */
    private ModelImage image;

    /** Flag indicating whether the VOI should be written in MIPAV's XML format. */
    private boolean isXML = false;

    /** The number of VOIs in the image. */
    private short numVOIs;

    /** File directory where the VOI is to read or written. */
    private int sliceNum;

    /** The current level of tab nesting we are on. Used to auto-indent nested xml tags. */
    private int tabLevel = 0;

    /** File name without the extension. */
    private String trimmedFileName;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * VOI reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      image     image model: needed during the read process to ensure the VOI "fits" in the image space.
     *
     * @exception  IOException  if there is an error making the files
     */
    public FileVOI(String fileName, String fileDir, ModelImage image) throws IOException {

        this.fileName = fileName;

        int idx = fileName.lastIndexOf(".");

        if (idx != -1) {
            trimmedFileName = fileName.substring(0, idx);
        } else {
            trimmedFileName = fileName;
        }

        this.fileDir = fileDir;
        this.image = image;
        this.numVOIs = (short) image.getVOIs().size();

        if (image.getNDims() >= 3) {
            sliceNum = image.getExtents()[2];
        } else {
            sliceNum = 1;
        }

        file = new File(fileDir + fileName);

        if (fileName.endsWith(".voi") || fileName.endsWith(".VOI")) {
            this.isXML = false;
        } else if (fileName.endsWith(".xml")) {
            this.isXML = true;
        } else {
            this.isXML = true;
            fileName += ".xml";
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    public final void closedTag(BufferedWriter bw, String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML
            // charset
            String writeVal = val.trim().replaceAll("&", "&amp;");

            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }

    /**
     * simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  bw     writer to use
     * @param  tag    tag name
     * @param  start  whether this is the start of a container tag
     */
    public final void openTag(BufferedWriter bw, String tag, boolean start) {

        try {

            if (!start) {

                // done with this container
                tabLevel--;
            }

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            if (start) {
                bw.write("<" + tag + ">");

                // indent the contained tags
                tabLevel++;
            } else {
                bw.write("</" + tag + ">");
            }

            bw.newLine();
        } catch (IOException ex) { }
    }

    /**
     * This method read a VOI file that has been saved in the MIPAV VOI format.
     *
     * @param      doLabel  boolean telling to read the files as VOITexts (labels)
     *
     * @return     the VOI read off the disk
     *
     * @exception  IOException  if there is an error reading the file
     */
    public VOI[] readVOI(boolean doLabel) throws IOException {

        String VOIStr;
        VOI[] voi = null;

        if (doLabel) {
            VOIVector voiVector = new VOIVector();

            if (!readAnnotationXML(voiVector)) {
                throw new IOException("Open label(s) failed.");
            } else {
                voi = new VOI[voiVector.size()];

                for (int i = 0; i < voi.length; i++) {
                    voi[i] = voiVector.VOIAt(i);
                }
            }

        } else if (isXML) {
            voi = new VOI[1];

            if (image.getNDims() > 2) {
                voi[0] = new VOI(numVOIs, trimmedFileName, image.getExtents()[2]);
            } else {
                voi[0] = new VOI(numVOIs, trimmedFileName, 1);
            }

            if (image.getNDims() == 2) {

                if (!readXML(voi[0])) {
                    throw (new IOException("Open VOI failed."));
                }
            } else {

                if (!readXML(voi[0])) {

                    if (!readCoordXML(voi[0])) {
                        throw (new IOException("Open VOI failed."));
                    } else {

                        // System.err.println("success");
                    }
                }
            }
        } else {
            voi = new VOI[1];
            raFile = new RandomAccessFile(file, "r");
            VOIStr = raFile.readLine();

            if (VOIStr == null) {
                return null;
            }

            VOIStr.trim();

            if (VOIStr.length() < 14) {

                if (VOIStr.regionMatches(true, 0, "S", 0, 1)) {
                    voi[0] = readNaugeVOI(VOIStr);

                    return voi;
                } else {
                    throw (new IOException("Not a VOI File MIPAV understands."));
                }
            }

            VOIStr = VOIStr.substring(0, 14);

            if (VOIStr.equals("MIPAV VOI FILE")) {
                voi[0] = readContourVOI();
            } else if (VOIStr.equals("MIPAV PTS FILE")) {
                voi[0] = readPointVOI();
            } else {
                throw (new IOException("Not a VOI File MIPAV can read."));
            }
        }

        return voi;

    }

    /**
     * Writes VOIText(s) to a .lbl file (XML based)
     *
     * @param   writeAll  whether or not to write all VOITexts into the same file, or only the selected VOIText
     *
     * @throws  IOException  exception thrown if there is an error writing the file
     */
    public void writeAnnotationXML(boolean writeAll) throws IOException {
        FileWriter fw;
        BufferedWriter bw;

        while (file.exists() == true) {
            int response = JOptionPane.showConfirmDialog(null, file.getName() + " exists. Overwrite?", "File exists",
                                                         JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.YES_OPTION) {
                file.delete();
                file = new File(fileDir + fileName);

                break;
            } else {
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Save label(s) as");
                chooser.setCurrentDirectory(file);

                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "lbl" }));

                int returnVal = chooser.showSaveDialog(null);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    fileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    file = new File(fileDir + fileName);
                } else {
                    return;
                }
            }
        }

        try {
            fw = new FileWriter(file);
            bw = new BufferedWriter(fw);

            bw.write(XML_HEADER);
            bw.newLine();
            bw.write(TEXT_HEADER);
            bw.newLine();

            openTag(bw, "Annotation xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);

            VOIVector VOIs = image.getVOIs();

            int numVOIs = VOIs.size();

            VOI currentVOI = null;
            Vector[] curves = null;
            VOIText vText = null;

            Point3Df arrowPt, textPt, arrowPtScanner, textPtScanner;
            int fontSize, fontDescriptors;
            boolean useMarker;
            String fontName;
            Color voiColor;
            Color voiBackgroundColor;
            String voiString;

            arrowPtScanner = new Point3Df();
            textPtScanner = new Point3Df();

            for (int i = 0; i < numVOIs; i++) {
                currentVOI = VOIs.VOIAt(i);

                if ((currentVOI.getCurveType() == VOI.ANNOTATION) && (writeAll || currentVOI.isActive())) {

                    curves = currentVOI.getCurves();

                    for (int j = 0; j < curves.length; j++) {

                        for (int k = 0; k < curves[j].size(); k++) {

                            openTag(bw, "Label", true);

                            // there's only one per VOI
                            vText = (VOIText) curves[j].elementAt(k);
                            voiString = vText.getText();
                            voiColor = vText.getColor();
                            voiBackgroundColor = vText.getBackgroundColor();
                            textPt = (Point3Df) vText.elementAt(0);
                            arrowPt = (Point3Df) vText.elementAt(1);
                            useMarker = vText.useMarker();
                            fontSize = vText.getFontSize();
                            fontName = vText.getFontName();
                            fontDescriptors = vText.getFontDescriptors();

                            closedTag(bw, "Text", voiString);

                            if (image.getNDims() > 2) {
                                MipavCoordinateSystems.fileToScanner(textPt, textPtScanner, image);
                                MipavCoordinateSystems.fileToScanner(arrowPt, arrowPtScanner, image);
                                closedTag(bw, "TextLocation",
                                          Float.toString(textPtScanner.x) + "," + Float.toString(textPtScanner.y) +
                                          "," + Float.toString(textPtScanner.z));
                                closedTag(bw, "ArrowLocation",
                                          Float.toString(arrowPtScanner.x) + "," + Float.toString(arrowPtScanner.y) +
                                          "," + Float.toString(arrowPtScanner.z));
                                // System.err.println("Text location: " + textPtScanner + ", arrow location: " +
                                // arrowPtScanner);
                            } else {
                                textPt.z = j;
                                arrowPt.z = j;
                                closedTag(bw, "TextLocation",
                                          Float.toString(textPt.x) + "," + Float.toString(textPt.y) + "," +
                                          Float.toString(textPt.z));
                                closedTag(bw, "ArrowLocation",
                                          Float.toString(arrowPt.x) + "," + Float.toString(arrowPt.y) + "," +
                                          Float.toString(arrowPt.z));
                            }

                            closedTag(bw, "UseMarker", Boolean.toString(useMarker));

                            closedTag(bw, "Color",
                                      Integer.toString(voiColor.getAlpha()) + "," +
                                      Integer.toString(voiColor.getRed()) + "," +
                                      Integer.toString(voiColor.getGreen()) + "," +
                                      Integer.toString(voiColor.getBlue()));
                            closedTag(bw, "BackgroundColor",
                                      Integer.toString(voiBackgroundColor.getAlpha()) + "," +
                                      Integer.toString(voiBackgroundColor.getRed()) + "," +
                                      Integer.toString(voiBackgroundColor.getGreen()) + "," +
                                      Integer.toString(voiBackgroundColor.getBlue()));
                            closedTag(bw, "FontName", fontName);
                            closedTag(bw, "FontSize", Integer.toString(fontSize));

                            if (fontDescriptors == Font.BOLD) {
                                closedTag(bw, "FontStyle", "BOLD");
                            } else if (fontDescriptors == Font.ITALIC) {
                                closedTag(bw, "FontStyle", "ITALIC");
                            } else if (fontDescriptors == (Font.BOLD + Font.ITALIC)) {
                                closedTag(bw, "FontStyle", "BOLDITALIC");
                            } else {
                                closedTag(bw, "FontStyle", "");
                            }

                            openTag(bw, "Label", false);
                        }
                    }


                }
            }

            openTag(bw, "Annotation", false);
            bw.close();

        } catch (Exception e) { }
    }

    /**
     * Writes a single VOI to a file in Nauge format.
     *
     * @param      voi  VOI to write
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeNaugeVOI(VOI voi) throws IOException {
        int i, j, k;
        int nSlices;
        int nPts;
        int nContours;
        float[] x;
        float[] y;
        float[] z;

        Vector[] contours;

        // Should add code to open dialog if file exists!!!
        if (file.exists() == true) {

            // dialog overwrite voi.file ?
            // yes
            raFile.close();
            file.delete();
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "rw");
            // no
            // open save as file dialog
        }

        contours = voi.getCurves();
        nSlices = voi.getCurves().length;

        if (image.getNDims() > 2) {
            raFile.writeBytes("S " + Integer.toString(image.getExtents()[2]) + "\n");
            nSlices = image.getExtents()[2];
        } else {
            raFile.writeBytes("S " + Integer.toString(1) + "\n");
            nSlices = 1;
        }

        for (i = 0; i < nSlices; i++) {
            nContours = contours[i].size();

            for (j = 0, nPts = 0; j < nContours; j++) {
                nPts += ((Vector) (contours[i].elementAt(j))).size();
            }

            raFile.writeBytes("V " + nPts + " z " + i + "\n");

            if (nContours > 0) {

                for (j = 0; j < nContours; j++) {
                    raFile.writeBytes("{\n");
                    nPts = ((Vector) (contours[i].elementAt(j))).size();

                    x = new float[nPts];
                    y = new float[nPts];
                    z = new float[nPts];
                    ((VOIContour) (contours[i].elementAt(j))).exportArrays(x, y, z);

                    for (k = 0; k < nPts; k++) {
                        raFile.writeBytes(Float.toString(x[k]) + " " + Float.toString(y[k]) + "\n");
                    }

                    raFile.writeBytes("}\n");
                }
            }
        }

        raFile.close();
    }

    /**
     * Writes a single VOI.POINT to a file MIPAV format.
     *
     * @param      voi  VOI.POINT to write
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writePointVOI(VOI voi) throws IOException {
        int i, j, k;
        int curveType;
        int count, length;
        int nContours;
        float[] x;
        float[] y;
        float[] z;
        Color color;

        Vector[] contours;

        if (file.exists() == true) {
            raFile.close();
            file.delete();
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "rw");
        } else {
            raFile = new RandomAccessFile(file, "rw");
        }

        raFile.writeBytes("MIPAV PTS FILE\r\n");

        contours = voi.getCurves();
        length = voi.getCurves().length;
        curveType = voi.getCurveType();
        color = voi.getColor();

        for (i = 0, count = 0; i < length; i++) {

            if (contours[i].size() > 0) {
                count++;
            }
        }

        raFile.writeBytes(Integer.toString(curveType) + "\t\t# curveType of the VOI\r\n");
        raFile.writeBytes(Integer.toString(color.getRed()) + "\t\t# color of VOI - red component\r\n");
        raFile.writeBytes(Integer.toString(color.getGreen()) + "\t\t# color of VOI - green component\r\n");
        raFile.writeBytes(Integer.toString(color.getBlue()) + "\t\t# color of VOI - blue component\r\n");
        raFile.writeBytes(Integer.toString(color.getAlpha()) + "\t\t# color of VOI - alpha component\r\n");
        raFile.writeBytes(Integer.toString(count) + "\t\t# number of slices for the VOI\r\n");

        i = 0;

        while (i <= count) {

            for (j = 0; j < contours.length; j++) {
                nContours = contours[j].size();

                if (nContours > 0) {
                    raFile.writeBytes(Integer.toString(j) + "\t\t# slice number\r\n");
                    raFile.writeBytes(Integer.toString(nContours) + "\t\t# number of contours in slice\r\n");
                }

                for (k = 0; k < nContours; k++) {

                    if (Integer.parseInt(((VOIPoint) contours[j].elementAt(k)).getLabel()) == (i - 1)) {
                        raFile.writeBytes(Integer.toString(1) + "\t\t# number of pts in contour\r\n");

                        x = new float[1];
                        y = new float[1];
                        z = new float[1];
                        ((VOIPoint) (contours[j].elementAt(k))).exportArrays(x, y, z);

                        raFile.writeBytes(Float.toString(x[k]) + " " + Float.toString(y[k]) + "\r\n");
                        i++;
                    }
                }
            }

        }

        /**
         * for ( i = 0, count = 0; i < length; i++ ) { nContours = contours[i].size(); if ( nContours > 0 ) {
         * raFile.writeBytes( Integer.toString( i ) + "\t\t# slice number\r\n" ); raFile.writeBytes( Integer.toString(
         * nContours ) + "\t\t# number of contours in slice\r\n" ); for ( j = 0; j < nContours; j++ ) { nPts = (
         * (Vector) ( contours[i].elementAt( j ) ) ).size(); raFile.writeBytes( Integer.toString( nPts ) + "\t\t# number
         * of pts in contour\r\n" ); x = new float[nPts]; y = new float[nPts]; z = new float[nPts]; ( (VOIPoint) (
         * contours[i].elementAt( j ) ) ).exportArrays( x, y, z ); for ( k = 0; k < nPts; k++ ) { raFile.writeBytes(
         * Float.toString( x[k] ) + " " + Float.toString( y[k] ) + "\r\n" ); } } } }
         */
        raFile.writeBytes(Integer.toString(voi.getUID()) + "\t\t# unique ID of the VOI\r\n");

        raFile.close();
    }

    /**
     * Writes a single VOI to a file MIPAV format.
     *
     * @param      voi              VOI to write
     * @param      saveAllContours  if true save all contours, not just the active ones
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeVOI(VOI voi, boolean saveAllContours) throws IOException {
        int i, j, k;
        int count, length;
        int curveType;
        int nPts;
        int nContours, nActiveContours;
        float[] x;
        float[] y;
        float[] z;
        Color color;

        Vector[] contours;

        if (isXML) {
            writeXML(voi, saveAllContours);

            return;
        }

        if (file.exists() == true) {
            file.delete();
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "rw");
        } else {
            raFile = new RandomAccessFile(file, "rw");
        }

        raFile.writeBytes("MIPAV VOI FILE\r\n");

        contours = voi.getCurves();
        length = voi.getCurves().length;
        curveType = voi.getCurveType();
        color = voi.getColor();

        for (i = 0, count = 0; i < length; i++) {
            nContours = contours[i].size();

            for (j = 0; j < nContours; j++) {

                if (saveAllContours || ((VOIBase) contours[i].elementAt(j)).isActive()) {
                    count++;

                    break;
                }
            }
        }

        raFile.writeBytes(Integer.toString(curveType) + "\t\t# curveType of the VOI\r\n");
        raFile.writeBytes(Integer.toString(color.getRed()) + "\t\t# color of VOI - red component\r\n");
        raFile.writeBytes(Integer.toString(color.getGreen()) + "\t\t# color of VOI - green component\r\n");
        raFile.writeBytes(Integer.toString(color.getBlue()) + "\t\t# color of VOI - blue component\r\n");
        raFile.writeBytes(Integer.toString(color.getAlpha()) + "\t\t# color of VOI - alpha component\r\n");
        raFile.writeBytes(Integer.toString(count) + "\t\t# number of slices for the VOI\r\n");

        for (i = 0, count = 0; i < length; i++) {
            nContours = contours[i].size();
            nActiveContours = 0;

            if (saveAllContours) {
                nActiveContours = nContours;
            } else {

                for (j = 0; j < nContours; j++) {

                    if (((VOIBase) contours[i].elementAt(j)).isActive()) {
                        nActiveContours++;
                    }
                }
            }

            if (nActiveContours > 0) {
                raFile.writeBytes(Integer.toString(i) + "\t\t# slice number\r\n");
                raFile.writeBytes(Integer.toString(nActiveContours) + "\t\t# number of contours in slice\r\n");

                for (j = 0; j < nContours; j++) {

                    if (saveAllContours || ((VOIBase) contours[i].elementAt(j)).isActive()) {
                        nPts = ((Vector) (contours[i].elementAt(j))).size();
                        raFile.writeBytes(Integer.toString(nPts) + "\t\t# number of pts in contour\r\n");
                        x = new float[nPts];
                        y = new float[nPts];
                        z = new float[nPts];
                        ((VOIBase) (contours[i].elementAt(j))).exportArrays(x, y, z);

                        for (k = 0; k < nPts; k++) {
                            raFile.writeBytes(Float.toString(x[k]) + " " + Float.toString(y[k]) + "\r\n");
                        }
                    }
                }
            }
        }

        raFile.writeBytes(Integer.toString(voi.getUID()) + "\t\t# unique ID of the VOI\r\n");

        raFile.close();
    }

    /**
     * Writes VOI to an XML formatted file.
     *
     * @param   voi              VOI to be saved
     * @param   saveAllContours  if true save all contours, not just the active ones
     *
     * @throws  IOException  exception thrown if there is an error writing the file
     */
    public void writeXML(VOI voi, boolean saveAllContours) throws IOException {
        int i, j, k, m;
        int length;
        int nPts;
        int nContours, nActiveContours;
        float[] x = new float[100];
        float[] y = new float[100];
        float[] z = new float[100];
        Vector[] contours;

        FileWriter fw;
        BufferedWriter bw;

        while (file.exists() == true) {
            int response = JOptionPane.showConfirmDialog(null, file.getName() + " exists. Overwrite?", "File exists",
                                                         JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.YES_OPTION) {
                file.delete();
                file = new File(fileDir + fileName);

                break;
            } else {
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Save VOI as");
                chooser.setCurrentDirectory(file);

                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { "xml", "voi" }));

                int returnVal = chooser.showSaveDialog(null);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    fileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    file = new File(fileDir + fileName);
                } else {
                    return;
                }
            }
        }

        try {

            fw = new FileWriter(file);
            bw = new BufferedWriter(fw);

            contours = voi.getCurves();
            length = voi.getCurves().length;

            bw.write(XML_HEADER);
            bw.newLine();
            bw.write(VOI_HEADER);
            bw.newLine();

            openTag(bw, "VOI xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);
            closedTag(bw, "Unique-ID", Integer.toString(voi.getUID()));
            closedTag(bw, "Curve-type", Integer.toString(voi.getCurveType()));
            closedTag(bw, "Color",
                      Integer.toString(voi.getColor().getAlpha()) + "," + Integer.toString(voi.getColor().getRed()) +
                      "," + Integer.toString(voi.getColor().getGreen()) + "," +
                      Integer.toString(voi.getColor().getBlue()));

            closedTag(bw, "Thickness", Integer.toString(voi.getThickness()));
            

            if ((voi.getCurveType() == VOI.CONTOUR) || (voi.getCurveType() == VOI.POLYLINE) ||
                    (voi.getCurveType() == VOI.POINT)) {

                // run through once and count how many TOTAL contours are there
                int totalContours = 0;

                long totalPoints = 0;
                int index;

                for (i = 0; i < length; i++) {
                    nContours = contours[i].size();
                    totalContours += nContours;

                    for (index = 0; index < nContours; index++) {
                        totalPoints += ((VOIBase) contours[i].elementAt(index)).size();
                    }

                }

                // System.err.println("TOTAL CONTOURS of VOI " + voi.getName() + ": " + totalContours);
                // System.err.println("Number of points for all contours combined: " + totalPoints);

                Vector pointVector = new Vector();


                // add all contours to a vector for sorting

                for (i = 0; i < contours.length; i++) {
                    nContours = contours[i].size();

                    for (k = 0; k < nContours; k++) {

                        if ((saveAllContours || ((VOIBase) contours[i].elementAt(k)).isActive())) {
                            pointVector.add(new VOISortItem((VOIBase) contours[i].elementAt(k),
                                                            Integer.parseInt(((VOIBase) contours[i].elementAt(k))
                                                                                 .getLabel()), i));

                        }
                    }
                }

                Collections.sort(pointVector, new VOIComparator());

                int pSize = pointVector.size();
                VOISortItem tempVOIItem = null;
                VOIBase tempBase = null;

                for (i = 0; i < pSize; i++) {

                    tempVOIItem = (VOISortItem) pointVector.elementAt(i);
                    tempBase = tempVOIItem.getVOIBase();

                    openTag(bw, "Contour", true);

                    // save old format if image dim is 2
                    if (image.getNDims() == 2) {
                        closedTag(bw, "Slice-number", Integer.toString(tempVOIItem.getSlice()));
                    }

                    nPts = tempBase.size();

                    if (nPts > x.length) {
                        x = new float[nPts];
                        y = new float[nPts];
                        z = new float[nPts];
                    }

                    tempBase.exportArrays(x, y, z);

                    if (image.getNDims() > 2) {
                        Point3Df ptIn = new Point3Df();
                        Point3Df ptOut = new Point3Df();
                        int slice = tempVOIItem.getSlice();

                        for (m = 0; m < nPts; m++) {
                            ptIn.x = x[m];
                            ptIn.y = y[m];
                            ptIn.z = slice;
                            MipavCoordinateSystems.fileToScanner(ptIn, ptOut, image);

                            // System.err.println("Pt in: " + ptIn + ", Pt out: " + ptOut);
                            closedTag(bw, "Pt",
                                      Float.toString(ptOut.x) + "," + Float.toString(ptOut.y) + "," +
                                      Float.toString(ptOut.z));
                        }
                    } else {
                        // image dim is 2.... save to old format

                        for (m = 0; m < nPts; m++) {
                            closedTag(bw, "Pt", Float.toString(x[m]) + "," + Float.toString(y[m]));
                        }

                    }

                    openTag(bw, "Contour", false);
                }

                pointVector.removeAllElements();
                pointVector = null;

            } else {

                for (i = 0; i < length; i++) {
                    nContours = contours[i].size();
                    nActiveContours = 0;

                    if (saveAllContours) {
                        nActiveContours = nContours;
                    } else {

                        for (j = 0; j < nContours; j++) {

                            if (((VOIBase) contours[i].elementAt(j)).isActive()) {
                                nActiveContours++;
                            }
                        }
                    }

                    if (nActiveContours > 0) {

                        for (j = 0; j < nContours; j++) {

                            if (saveAllContours || ((VOIBase) contours[i].elementAt(j)).isActive()) {
                                openTag(bw, "Contour", true);

                                // save old format if image dim is 2
                                if (image.getNDims() == 2) {
                                    closedTag(bw, "Slice-number", Integer.toString(i));
                                }

                                nPts = ((Vector) (contours[i].elementAt(j))).size();

                                if (x.length < nPts) {
                                    x = new float[nPts];
                                    y = new float[nPts];
                                    z = new float[nPts];
                                }

                                ((VOIBase) (contours[i].elementAt(j))).exportArrays(x, y, z);

                                if (image.getNDims() > 2) {
                                    Point3Df ptIn = new Point3Df();
                                    Point3Df ptOut = new Point3Df();
                                    int slice = i;

                                    for (m = 0; m < nPts; m++) {
                                        ptIn.x = x[m];
                                        ptIn.y = y[m];
                                        ptIn.z = slice;
                                        MipavCoordinateSystems.fileToScanner(ptIn, ptOut, image);
                                        closedTag(bw, "Pt",
                                                  Float.toString(ptOut.x) + "," + Float.toString(ptOut.y) + "," +
                                                  Float.toString(ptOut.z));
                                    }
                                } else {
                                    // image is 2d

                                    for (k = 0; k < nPts; k++) {
                                        closedTag(bw, "Pt", Float.toString(x[k]) + "," + Float.toString(y[k]));
                                    }
                                }

                                openTag(bw, "Contour", false);
                            }
                        }
                    }
                }
            }

            openTag(bw, "VOI", false);
            bw.close();
        } catch (Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
            e.printStackTrace();
        }
    }

    /**
     * Converts string point (x,y) to Point2Df point.
     *
     * @param  str  string to convert
     * @param  pt   point to return
     */
    private void decodeLine(String str, Point2Df pt) {
        int index;
        String xStr, yStr;

        index = str.indexOf(" ");
        xStr = str.substring(0, index).trim();
        pt.x = (Float.valueOf(xStr).floatValue());

        yStr = str.substring(index + 1).trim();
        pt.y = (Float.valueOf(yStr).floatValue());
    }

    /**
     * Reads in an annotation xml (.lbl) file and puts all VOITexts into a VOIVector (to be added to the image)
     *
     * @param   voiVector  a VOIVector for holding the VOIs before they are transferred to the image
     *
     * @return  boolean
     */
    private boolean readAnnotationXML(VOIVector voiVector) {

        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            URL xsdURL = getClass().getClassLoader().getResource("text.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new XMLAnnotationHandler(voiVector));

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + fileName));
        } catch (Exception error) {
            error.printStackTrace();
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    /**
     * This method read a VOI file that has been saved in the MIPAV VOI format.
     *
     * @return     the new VOI read from the file
     *
     * @exception  IOException  if there is an error reading the file
     */
    private VOI readContourVOI() throws IOException {
        int i, j, k;
        int nSlices, nContours, nPts;
        int sliceNo;
        float[] x, y, z;
        Point2Df pt2D = new Point2Df();
        VOI newVOI;
        int maxSlice;
        int curveType;
        int red, green, blue, alpha;

        curveType = Integer.valueOf(readLine()).intValue();

        if (curveType == VOI.POINT) {
            throw (new IOException(" VOI file error: trying to read point VOI as a contour."));
        }

        red = Integer.valueOf(readLine()).intValue();
        green = Integer.valueOf(readLine()).intValue();
        blue = Integer.valueOf(readLine()).intValue();
        alpha = Integer.valueOf(readLine()).intValue();

        nSlices = Integer.valueOf(readLine()).intValue();

        if ((nSlices <= 0) || (nSlices > 2096)) {
            throw (new IOException(" VOI file corrupted: too many slices"));
        }

        if (image.getNDims() > 2) {
            newVOI = new VOI((short) image.getVOIs().size(), trimmedFileName, image.getExtents()[2], curveType, -1.0f);
            maxSlice = image.getExtents()[2];
        } else {
            newVOI = new VOI((short) image.getVOIs().size(), trimmedFileName, 1, curveType, -1.0f);
            maxSlice = 1;
        }

        Preferences.debug(" n = " + nSlices + " image slice = " + maxSlice + "\n", Preferences.DEBUG_FILEIO);

        if (nSlices > maxSlice) {
            throw new IOException(" VOI file corrupted: too many slices");
        }

        for (i = 0; i < nSlices; i++) {
            sliceNo = Integer.valueOf(readLine()).intValue();

            if (sliceNo < maxSlice) {
                nContours = Integer.valueOf(readLine()).intValue();

                if ((nContours <= 0) || (nContours > 2096)) {
                    throw (new IOException(" VOI file corrupted: too many contours per slice"));
                }

                for (j = 0; j < nContours; j++) {
                    nPts = Integer.valueOf(readLine()).intValue();
                    x = new float[nPts];
                    y = new float[nPts];
                    z = new float[nPts];

                    for (k = 0; k < nPts; k++) {
                        decodeLine(readLine(), pt2D);
                        x[k] = pt2D.x;
                        y[k] = pt2D.y;
                        z[k] = (float) sliceNo;
                    }

                    newVOI.importCurve(x, y, z, sliceNo);
                }
            } else {
                raFile.close();
                throw (new IOException(" VOI file/image mismatch: VOI slice no. exceeds slices in image."));
            }
        }

        raFile.close();

        // set the color of the voi
        try {
            newVOI.setColor(new Color(red, green, blue, alpha));
        } catch (Exception e) {
            Preferences.debug("readContourVOI: unable to set color of newVOI.", Preferences.DEBUG_FILEIO);
        }

        return (newVOI);
    }

    /**
     * Reads in a VOI from a file using the new VOI schema based on Scanner coordinates rather than pixel space
     *
     * @param   voi  VOI to be read in
     *
     * @return  boolean whether or not the VOI read in successfully
     */
    private boolean readCoordXML(VOI voi) {
        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            URL xsdURL = getClass().getClassLoader().getResource("voi_coord.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new XMLCoordHandler(voi));

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + fileName));
        } catch (Exception error) {
            error.printStackTrace();
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    /**
     * Reads a line of the file and strips comments indicated by the # symbol.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString;
        int index;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null) {
            return null;
        }

        index = tempString.indexOf("#");

        if (index != -1) {
            tempString = tempString.substring(0, index - 1);
        }

        return tempString.trim();
    }

    /**
     * This method read a VOI file that has been saved in the Nauge VOI format.
     *
     * @param      firstStr  first line of the Nauge VOI file
     *
     * @return     the VOI
     *
     * @exception  IOException  if there is an error reading the file
     */
    private VOI readNaugeVOI(String firstStr) throws IOException {
        int i, j, k;
        int indexZ;
        int nSlices, nPts;
        int sliceNo;
        String VOIStr, tempStr;
        float[] x, y, z;
        float[] xTemp, yTemp;
        Point2Df pt2D = new Point2Df();
        VOI newVOI;
        int maxSlice;

        xTemp = new float[10000];
        yTemp = new float[10000];

        tempStr = firstStr.substring(1).trim();
        nSlices = Integer.valueOf(tempStr).intValue();

        if ((nSlices <= 0) || (nSlices > 2096)) {
            throw (new IOException(" VOI file corrupted: too many slices"));
        }

        if (image.getNDims() > 2) {
            newVOI = new VOI((short) image.getVOIs().size(), trimmedFileName, image.getExtents()[2], VOI.CONTOUR,
                             -1.0f);
            maxSlice = image.getExtents()[2];
        } else {
            newVOI = new VOI((short) image.getVOIs().size(), trimmedFileName, 1, VOI.CONTOUR, -1.0f);
            maxSlice = 1;
        }

        VOIStr = raFile.readLine();

        if (!VOIStr.regionMatches(true, 0, "V", 0, 1)) {
            throw (new IOException("Error reading VOI file"));
        }

        for (i = 0; i < nSlices; i++) {
            indexZ = VOIStr.indexOf("z");
            tempStr = VOIStr.substring(1, indexZ - 1).trim();
            nPts = Integer.valueOf(tempStr).intValue();

            tempStr = VOIStr.substring(indexZ + 1).trim();
            sliceNo = Math.abs(Float.valueOf(tempStr).intValue());

            if ((sliceNo < maxSlice) && (nPts > 0)) {

                while ((VOIStr = readLine()) != null) {

                    if (VOIStr.regionMatches(true, 0, "V", 0, 1)) {
                        break;
                    } else if (VOIStr.regionMatches(true, 0, "{", 0, 1)) {
                        k = 0;

                        while (!((VOIStr = readLine()).regionMatches(true, 0, "}", 0, 1))) {
                            decodeLine(VOIStr, pt2D);

                            if ((pt2D.x < 0) || (pt2D.y < 0) || (pt2D.x >= image.getExtents()[0]) ||
                                    (pt2D.y >= image.getExtents()[1])) {
                                throw (new IOException("VOI points exceed image bounds"));
                            }

                            xTemp[k] = pt2D.x;
                            yTemp[k] = pt2D.y;
                            k++;
                        }

                        x = new float[k];
                        y = new float[k];
                        z = new float[k];

                        for (j = 0; j < k; j++) {
                            x[j] = xTemp[j];
                            y[j] = yTemp[j];
                            z[j] = sliceNo;
                        }

                        newVOI.importCurve(x, y, z, sliceNo);
                    } else {
                        throw (new IOException("Error reading VOI file"));
                    }
                }
            } else if (sliceNo >= (maxSlice - 1)) {
                raFile.close();

                return (newVOI);
            } else {
                VOIStr = readLine();
            }
        }

        raFile.close();

        return (newVOI);
    }

    /**
     * This method read a VOI.POINT file that has been saved in the MIPAV PTS format.
     *
     * @return     the new VOI read from the file
     *
     * @exception  IOException  if there is an error reading the file
     */
    private VOI readPointVOI() throws IOException {
        int i, j, k;
        int nSlices, nContours, nPts;
        int sliceNo;
        float[] x, y, z;
        Point2Df pt2D = new Point2Df();
        VOI newVOI;
        short id = 7777; /* different from any IDs in ViewJComponentEditImage */
        int curveType;
        int red, green, blue, alpha;
        int uid;

        curveType = Integer.valueOf(readLine()).intValue();

        if (curveType != VOI.POINT) {
            throw (new IOException(" VOI file error: trying to read non-point VOI as a point."));
        }

        red = Integer.valueOf(readLine()).intValue();
        green = Integer.valueOf(readLine()).intValue();
        blue = Integer.valueOf(readLine()).intValue();
        alpha = Integer.valueOf(readLine()).intValue();

        nSlices = Integer.valueOf(readLine()).intValue();

        if ((nSlices <= 0) || (nSlices > sliceNum)) {
            throw (new IOException(" VOI file corrupted: too many slices"));
        }

        newVOI = new VOI(id, "RegPoints", sliceNum, VOI.POINT, -1.0f);

        for (i = 0; i < nSlices; i++) {
            sliceNo = Integer.valueOf(readLine()).intValue();

            if (sliceNo < sliceNum) {
                nContours = Integer.valueOf(readLine()).intValue();

                if ((nContours <= 0) || (nContours > 2096)) {
                    throw (new IOException(" VOI file corrupted: too many contours per slice"));
                }

                for (j = 0; j < nContours; j++) {
                    nPts = Integer.valueOf(readLine()).intValue();
                    x = new float[nPts];
                    y = new float[nPts];
                    z = new float[nPts];

                    for (k = 0; k < nPts; k++) {
                        decodeLine(readLine(), pt2D);
                        x[k] = pt2D.x;
                        y[k] = pt2D.y;
                        z[k] = (float) sliceNo;
                    }

                    newVOI.importCurve(x, y, z, sliceNo);
                }
            }
        }

        try {
            uid = Integer.valueOf(readLine()).intValue();
            newVOI.setUID(uid);
        } catch (IOException ex) {
            Preferences.debug(ex + "\n", Preferences.DEBUG_FILEIO);
        }

        raFile.close();

        // set the color of the voi
        try {
            newVOI.setColor(new Color(red, green, blue, alpha));
        } catch (Exception e) {
            Preferences.debug("readContourVOI: unable to set color of newVOI.\n", Preferences.DEBUG_FILEIO);
        }

        return (newVOI);
    }

    /**
     * Reads in the older VOI XML schema (using pixel coordinates)
     *
     * @param   voi  VOI
     *
     * @return  boolean
     */
    private boolean readXML(VOI voi) {

        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            URL xsdURL = getClass().getClassLoader().getResource("voi.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new MyXMLHandler(voi));

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(fileDir + fileName));
        } catch (Exception error) {
            // MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Handle events generated while parsing the XML file.
     */
    private class MyXMLHandler extends DefaultHandler {

        /** The contours of the VOI we are building. */
        private Vector contourVector;

        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();

        /** The slice the VOI contour should be on. */
        private int sliceNumber = 0;

        /** The VOI that we are building from the XML. */
        private VOI voi;

        /**
         * Construct our custom XML data handler.
         *
         * @param  voi  the VOI we should build from the XML file data
         */
        public MyXMLHandler(VOI voi) {
            this.voi = voi;
            contourVector = new Vector();
        }

        /**
         * DOCUMENT ME!
         *
         * @param  ch      char[]
         * @param  start   int
         * @param  length  int
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            // don't need to de-entity-ize the string because the parser does
            // that automatically
            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is
         * generally saved to the image info.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            currentKey = localName;

            if (currentKey.equals("Unique-ID")) {
                voi.setUID(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Curve-type")) {
                voi.setCurveType(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Color")) {
                int a = 0, r = 0, g = 0, b = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    a = Integer.parseInt(st.nextToken());
                    r = Integer.parseInt(st.nextToken());
                    g = Integer.parseInt(st.nextToken());
                    b = Integer.parseInt(st.nextToken());

                    voi.setColor(new Color(r, g, b, a));
                } catch (NumberFormatException ex) {
                    Preferences.debug("Point is incorrectly formatted: " + ex.toString() + "\n",
                                      Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("Thickness")) {
            	voi.setThickness(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Slice-number")) {
                sliceNumber = Integer.parseInt(elementBuffer);
            } else if (currentKey.equals("Pt")) {
                float x = 0f, y = 0f;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    contourVector.addElement(new Point2Df(x, y));
                } catch (NumberFormatException nfex) {
                    Preferences.debug("Error reading pt: " + nfex.toString() + "\n", Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("Contour")) {

                // finished adding points to contour.. now add to VOI
                int index;
                float[] x, y, z;

                x = new float[contourVector.size()];
                y = new float[contourVector.size()];
                z = new float[contourVector.size()];


                for (index = 0; index < contourVector.size(); index++) {
                    x[index] = ((Point2Df) contourVector.elementAt(index)).x;
                    y[index] = ((Point2Df) contourVector.elementAt(index)).y;
                    z[index] = (float) sliceNumber;
                }

                voi.importCurve(x, y, z, sliceNumber);
            }

        }

        /**
         * Parser calls this for each element in a document.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         * @param   atts          attributes for the current tag
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {
            currentKey = localName;
            elementBuffer = "";

            if (currentKey.equals("Contour")) {
                contourVector.clear();
            }
            // else if (currentKey.equals("VOI")) {
            // voi.setName(Integer.toString(voi.getUID()));
            // }
        }

    }

    /**
     * DOCUMENT ME!
     */
    private class VOIComparator implements Comparator {

        /**
         * DOCUMENT ME!
         *
         * @param   o1  DOCUMENT ME!
         * @param   o2  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int compare(Object o1, Object o2) {
            int a = ((VOISortItem) o1).getIndex();
            int b = ((VOISortItem) o2).getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }

    /**
     * DOCUMENT ME!
     */
    private class VOISortItem {

        /** DOCUMENT ME! */
        private int index;

        /** DOCUMENT ME! */
        private int slice;

        /** DOCUMENT ME! */
        private VOIBase vBase;

        /**
         * Creates a new VOISortItem object.
         *
         * @param  base  DOCUMENT ME!
         * @param  idx   DOCUMENT ME!
         * @param  z     DOCUMENT ME!
         */
        public VOISortItem(VOIBase base, int idx, int z) {
            vBase = base;
            index = idx;
            slice = z;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getSlice() {
            return slice;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public VOIBase getVOIBase() {
            return vBase;
        }


    }

    /**
     * Handle events generated while parsing the Annotation XML file.
     */
    private class XMLAnnotationHandler extends DefaultHandler {

        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();

        /** name of the font */
        private String fontName = null;

        /** size of the font */
        private int fontSize = 0;

        /** style of the font (bold/italic etc) */
        private int fontStyle = 0;

        /** id for the VOI */
        private short id = -1;

        /** the slice where the VOI will be located */
        private int slice = 0;

        /** The VOI that we are building from the XML. */
        private VOI voi;

        /** a temporary VOIText holder which will be added to the VOIVector */
        private VOIText voiText = null;

        /** Holds all the VOITexts read in */
        private VOIVector voiVector;

        /** the Z dimension of the image*/
        private int zDim = 1;


        /**
         * Construct our custom XML data handler.
         *
         * @param  voiVector  the VOI we should build from the XML file data
         */
        public XMLAnnotationHandler(VOIVector voiVector) {
            this.voiVector = voiVector;

            if (image.getNDims() > 2) {
                zDim = image.getExtents()[2];
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param  ch      char[]
         * @param  start   int
         * @param  length  int
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            // don't need to de-entity-ize the string because the parser does
            // that automatically
            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is
         * generally saved to the image info.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            currentKey = localName;

            if (currentKey.equals("Label")) {
                voiText.setFontName(fontName);
                voiText.setFontSize(fontSize);
                voiText.setFontDescriptors(fontStyle);

                voi.importCurve(voiText, slice);
                voiVector.addVOI(voi);
            }

            if (currentKey.equals("Unique-ID")) {
                voi.setUID(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Text")) {
                voiText.setText(elementBuffer);
            } else if (currentKey.equals("TextLocation") || currentKey.equals("ArrowLocation")) {
                float x = 0f, y = 0f, z = 0f;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    z = Float.parseFloat(st.nextToken());
                    // System.err.println("X: " + x + ", Y: " + y + ", Z: " + z);
                } catch (NumberFormatException nfex) {
                    Preferences.debug("Error reading pt: " + nfex.toString() + "\n", Preferences.DEBUG_FILEIO);
                }

                Point3Df ptIn = new Point3Df(x, y, z);
                Point3Df ptOut = new Point3Df();

                if (image.getNDims() > 2) {
                    int xDim = image.getExtents()[0];
                    int yDim = image.getExtents()[1];
                    int zDim = image.getExtents()[2];

                    Preferences.debug("New contour: " + "\n", Preferences.DEBUG_FILEIO);

                    MipavCoordinateSystems.scannerToFile(ptIn, ptOut, image);


                    if ((ptOut.x > xDim) || (ptOut.y > yDim) || (ptOut.z >= zDim) || (ptOut.x < 0) || (ptOut.y < 0) ||
                            (ptOut.z < 0)) {
                        MipavUtil.displayWarning("VOI on file out of image bounds:  Open VOI aborted");

                        return;
                    }

                    slice = (int) ptOut.z;

                    voiText.addElement(ptOut);
                } else {
                    voiText.addElement(ptIn);
                }

            } else if (currentKey.equals("UseMarker")) {
                voiText.setUseMarker(Boolean.valueOf(elementBuffer).booleanValue());
            } else if (currentKey.equals("Color") || currentKey.equals("BackgroundColor")) {

                int a = 0, r = 0, g = 0, b = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    a = Integer.parseInt(st.nextToken());
                    r = Integer.parseInt(st.nextToken());
                    g = Integer.parseInt(st.nextToken());
                    b = Integer.parseInt(st.nextToken());

                    if (currentKey.equals("Color")) {
                        voi.setColor(new Color(r, g, b, a));
                        voiText.setColor(new Color(r, g, b, a));
                    } else {
                        voiText.setBackgroundColor(new Color(r, g, b, a));
                    }

                } catch (NumberFormatException ex) {
                    Preferences.debug("Point is incorrectly formatted: " + ex.toString() + "\n",
                                      Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("FontName")) {
                fontName = new String(elementBuffer);
            } else if (currentKey.equals("FontSize")) {
                fontSize = Integer.parseInt(elementBuffer);
            } else if (currentKey.equals("FontStyle")) {

                if (elementBuffer.indexOf("BOLD") != -1) {
                    fontStyle += Font.BOLD;
                }

                if (elementBuffer.indexOf("ITALIC") != -1) {
                    fontStyle += Font.ITALIC;
                }
            }

        }

        /**
         * Parser calls this for each element in a document.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         * @param   atts          attributes for the current tag
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {
            currentKey = localName;
            elementBuffer = "";

            if (currentKey.equals("Label")) {
            	id++;
                voi = new VOI(id, "Label_" + id, zDim, VOI.ANNOTATION, 0f);
                voiText = new VOIText();

            }
        }

    }

    /**
     * Handle events generated while parsing the XML file.
     */
    private class XMLCoordHandler extends DefaultHandler {

        /** The contours of the VOI we are building. */
        private Vector contourVector;

        /** The current XML tag we are parsing. */
        private String currentKey;

        /** The data for the current element being parsed. */
        private String elementBuffer = new String();

        /** The VOI that we are building from the XML. */
        private VOI voi;

        /**
         * Construct our custom XML data handler.
         *
         * @param  voi  the VOI we should build from the XML file data
         */
        public XMLCoordHandler(VOI voi) {
            this.voi = voi;
            contourVector = new Vector();
        }

        /**
         * DOCUMENT ME!
         *
         * @param  ch      char[]
         * @param  start   int
         * @param  length  int
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            // don't need to de-entity-ize the string because the parser does
            // that automatically
            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is
         * generally saved to the image info.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            currentKey = localName;

            if (currentKey.equals("Unique-ID")) {
                voi.setUID(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Curve-type")) {
                voi.setCurveType(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Color")) {
                int a = 0, r = 0, g = 0, b = 0;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    a = Integer.parseInt(st.nextToken());
                    r = Integer.parseInt(st.nextToken());
                    g = Integer.parseInt(st.nextToken());
                    b = Integer.parseInt(st.nextToken());

                    voi.setColor(new Color(r, g, b, a));
                } catch (NumberFormatException ex) {
                    Preferences.debug("Point is incorrectly formatted: " + ex.toString() + "\n",
                                      Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("Thickness")) {
            	voi.setThickness(Integer.parseInt(elementBuffer));
            } else if (currentKey.equals("Pt")) {
                float x = 0f, y = 0f, z = 0f;
                StringTokenizer st = new StringTokenizer(elementBuffer, ",");

                try {
                    x = Float.parseFloat(st.nextToken());
                    y = Float.parseFloat(st.nextToken());
                    z = Float.parseFloat(st.nextToken());
                    contourVector.addElement(new Point3Df(x, y, z));
                    // System.err.println("X: " + x + ", Y: " + y + ", Z: " + z);
                } catch (NumberFormatException nfex) {
                    Preferences.debug("Error reading pt: " + nfex.toString() + "\n", Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("Contour")) {

                // finished adding points to contour.. now add to VOI
                int index;
                float[] x, y, z;

                x = new float[contourVector.size()];
                y = new float[contourVector.size()];
                z = new float[contourVector.size()];

                Point3Df ptIn = new Point3Df();
                Point3Df ptOut = new Point3Df();

                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];
                int zDim = image.getExtents()[2];

                Preferences.debug("New contour: " + "\n", Preferences.DEBUG_FILEIO);

                for (index = 0; index < contourVector.size(); index++) {
                    ptIn = (Point3Df) contourVector.elementAt(index);

                    // System.err.println("\tScanner coord: " + ptIn);
                    MipavCoordinateSystems.scannerToFile(ptIn, ptOut, image);

                    x[index] = MipavMath.round(ptOut.x);
                    y[index] = MipavMath.round(ptOut.y);
                    z[index] = MipavMath.round(ptOut.z);

                    // System.err.println("POINT OUT (scanner to file): " + x[index] + ", " + y[index] + ", " +
                    // z[index]);
                    Preferences.debug("\tScanner coord: " + ptIn + ", File coord: " + x[index] + ", " + y[index] +
                                      ", " + z[index] + "\n", Preferences.DEBUG_FILEIO);

                    if ((ptOut.x > xDim) || (ptOut.y > yDim) || (ptOut.z >= zDim)) {
                        MipavUtil.displayWarning("VOI on file out of image bounds:  Open VOI aborted");

                        return;
                    }
                }

                voi.importCurve(x, y, z, (int) ptOut.z);
            }

        }

        /**
         * Parser calls this for each element in a document.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         * @param   atts          attributes for the current tag
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {
            currentKey = localName;
            elementBuffer = "";

            if (currentKey.equals("Contour")) {
                contourVector.clear();
            }
            // else if (currentKey.equals("VOI")) {
            // voi.setName(Integer.toString(voi.getUID()));
            // }
        }

    }


}
