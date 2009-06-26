package gov.nih.mipav.model.file;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;

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
public class FileVOI extends FileXML {

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

    /** File reference, it and its components are able to be modified by extending classes. */
    protected File file;

    /** File directory where the VOI is to read or written. */
    protected String fileDir;

    /** File name of the VOI. */
    protected String fileName;

    /** Image used to reference properties of the image to aid reading and writing of the VOI. */
    private ModelImage image;

    /** Flag indicating whether the VOI should be written in MIPAV's XML format. */
    private boolean isXML = false;

    /** The number of VOIs in the image. */
    private short numVOIs;

    /** File directory where the VOI is to read or written. */
    private int sliceNum;

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
    	super(fileName, fileDir);
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

                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".lbl" }));

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

            openTag("Annotation xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);

            VOIVector VOIs = image.getVOIs();

            int numVOIs = VOIs.size();

            VOI currentVOI = null; 
            Vector[] curves = null;
            VOIText vText = null;

            Vector3f arrowPt, textPt, arrowPtScanner, textPtScanner;
            int fontSize, fontDescriptors;
            boolean useMarker, doNote;
            String fontName;
            Color voiColor;
            Color voiBackgroundColor;
            String voiString;
            String noteString = new String();

            arrowPtScanner = new Vector3f();
            textPtScanner = new Vector3f();

            for (int i = 0; i < numVOIs; i++) {
                currentVOI = VOIs.VOIAt(i);

                if ((currentVOI.getCurveType() == VOI.ANNOTATION) && (writeAll || currentVOI.isActive())) {

                    curves = currentVOI.getCurves();

                    for (int j = 0; j < curves.length; j++) {

                        for (int k = 0; k < curves[j].size(); k++) {

                            openTag("Label", true);

                            // there's only one per VOI
                            vText = (VOIText) curves[j].elementAt(k);
                            voiString = vText.getText();
                            noteString = vText.getNote();
                        	doNote = !noteString.equals(JDialogAnnotation.DEFAULT_NOTES) && noteString.length() > 0;
                        	
                            voiColor = vText.getColor();
                            voiBackgroundColor = vText.getBackgroundColor();
                            textPt = vText.elementAt(0);
                            arrowPt = vText.elementAt(1);
                            useMarker = vText.useMarker();
                            fontSize = vText.getFontSize();
                            fontName = vText.getFontName();
                            fontDescriptors = vText.getFontDescriptors();

                            closedTag( "Text", voiString);
                            if(doNote)
                            	closedTag( "Note", noteString);

                            if (image.getNDims() > 2) {
                                MipavCoordinateSystems.fileToScanner(textPt, textPtScanner, image);
                                MipavCoordinateSystems.fileToScanner(arrowPt, arrowPtScanner, image);
                                closedTag( "TextLocation",
                                          Float.toString(textPtScanner.X) + "," + Float.toString(textPtScanner.Y) +
                                          "," + Float.toString(textPtScanner.Z));
                                closedTag( "ArrowLocation",
                                          Float.toString(arrowPtScanner.X) + "," + Float.toString(arrowPtScanner.Y) +
                                          "," + Float.toString(arrowPtScanner.Z));
                                // System.err.println("Text location: " + textPtScanner + ", arrow location: " +
                                // arrowPtScanner);
                            } else {
                                textPt.Z = j;
                                arrowPt.Z = j;
                                closedTag( "TextLocation",
                                          Float.toString(textPt.X) + "," + Float.toString(textPt.Y) + "," +
                                          Float.toString(textPt.Z));
                                closedTag( "ArrowLocation",
                                          Float.toString(arrowPt.X) + "," + Float.toString(arrowPt.Y) + "," +
                                          Float.toString(arrowPt.Z));
                            }

                            closedTag( "UseMarker", Boolean.toString(useMarker));

                            closedTag( "Color",
                                      Integer.toString(voiColor.getAlpha()) + "," +
                                      Integer.toString(voiColor.getRed()) + "," +
                                      Integer.toString(voiColor.getGreen()) + "," +
                                      Integer.toString(voiColor.getBlue()));
                            closedTag( "BackgroundColor",
                                      Integer.toString(voiBackgroundColor.getAlpha()) + "," +
                                      Integer.toString(voiBackgroundColor.getRed()) + "," +
                                      Integer.toString(voiBackgroundColor.getGreen()) + "," +
                                      Integer.toString(voiBackgroundColor.getBlue()));
                            closedTag( "FontName", fontName);
                            closedTag( "FontSize", Integer.toString(fontSize));

                            if (fontDescriptors == Font.BOLD) {
                                closedTag( "FontStyle", "BOLD");
                            } else if (fontDescriptors == Font.ITALIC) {
                                closedTag( "FontStyle", "ITALIC");
                            } else if (fontDescriptors == (Font.BOLD + Font.ITALIC)) {
                                closedTag( "FontStyle", "BOLDITALIC");
                            } else {
                                closedTag( "FontStyle", "");
                            }

                            openTag("Label", false);
                        }
                    }


                }
            }

            openTag("Annotation", false);
            bw.close();

        } catch (Exception e) { }
    }
    
    /**
     * Writes VOIText(s) to a .lbl file (XML based)
     *
     * @param   writeAllInSameFile  whether or not to write all VOITexts into the same file, or only the selected VOIText
     *
     * @throws  IOException  exception thrown if there is an error writing the file
     */
    public void writeAnnotationInVoiAsXML(boolean writeAllInSameFile, boolean writeAllFromImage) throws IOException {
        FileWriter fw;
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

                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".lbl" }));

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

            openTag("Annotation xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);

            VOIVector VOIs = image.getVOIs();

            int numVOIs = VOIs.size();

            VOI currentVOI = null;
            Vector[] curves = null;
            VOIText vText = null;

            Vector3f arrowPt, textPt, arrowPtScanner, textPtScanner;
            int fontSize, fontDescriptors;
            boolean useMarker, doNote = false;
            String fontName;
            Color voiColor;
            Color voiBackgroundColor;
            String voiString;
            String noteString = new String();

            arrowPtScanner = new Vector3f();
            textPtScanner = new Vector3f();

            for (int i = 0; i < numVOIs; i++) {
                currentVOI = VOIs.VOIAt(i);

                if (currentVOI.getCurveType() == VOI.ANNOTATION && (writeAllFromImage || currentVOI.isActive())) {

                    curves = currentVOI.getCurves();

                    for (int j = 0; j < curves.length; j++) {

                        for (int k = 0; k < curves[j].size(); k++) {

                        	vText = (VOIText) curves[j].elementAt(k);
                        	voiString = vText.getText();
                        	noteString = vText.getNote();
                        	doNote = !noteString.equals(JDialogAnnotation.DEFAULT_NOTES) && noteString.length() > 0;
                        	
                        	//System.out.println("VOI Name: "+voiString+"\tfile: "+file.getName());
                        	if(writeAllInSameFile || 
                        			(!writeAllInSameFile && voiString.equals(file.getName().substring(0, file.getName().lastIndexOf('.'))))) {
                        		
	                        	openTag("Label", true);
	                        	
	                            // there's only one per VOI, but 
	                            voiColor = vText.getColor();
	                            voiBackgroundColor = vText.getBackgroundColor();
	                            textPt = vText.elementAt(0);
	                            arrowPt =  vText.elementAt(1);
	                            useMarker = vText.useMarker();
	                            fontSize = vText.getFontSize();
	                            fontName = vText.getFontName();
	                            fontDescriptors = vText.getFontDescriptors();
	
	                            closedTag( "Text", voiString);
	                            if(doNote)
	                            	closedTag( "Note", noteString);
	
	                            if (image.getNDims() > 2) {
	                                MipavCoordinateSystems.fileToScanner(textPt, textPtScanner, image);
	                                MipavCoordinateSystems.fileToScanner(arrowPt, arrowPtScanner, image);
	                                closedTag( "TextLocation",
	                                          Float.toString(textPtScanner.X) + "," + Float.toString(textPtScanner.Y) +
	                                          "," + Float.toString(textPtScanner.Z));
	                                closedTag( "ArrowLocation",
	                                          Float.toString(arrowPtScanner.X) + "," + Float.toString(arrowPtScanner.Y) +
	                                          "," + Float.toString(arrowPtScanner.Z));
	                                // System.err.println("Text location: " + textPtScanner + ", arrow location: " +
	                                // arrowPtScanner);
	                            } else {
	                                textPt.Z = j;
	                                arrowPt.Z = j;
	                                closedTag( "TextLocation",
	                                          Float.toString(textPt.X) + "," + Float.toString(textPt.Y) + "," +
	                                          Float.toString(textPt.Z));
	                                closedTag( "ArrowLocation",
	                                          Float.toString(arrowPt.X) + "," + Float.toString(arrowPt.Y) + "," +
	                                          Float.toString(arrowPt.Z));
	                            }
	
	                            closedTag( "UseMarker", Boolean.toString(useMarker));
	
	                            closedTag( "Color",
	                                      Integer.toString(voiColor.getAlpha()) + "," +
	                                      Integer.toString(voiColor.getRed()) + "," +
	                                      Integer.toString(voiColor.getGreen()) + "," +
	                                      Integer.toString(voiColor.getBlue()));
	                            closedTag( "BackgroundColor",
	                                      Integer.toString(voiBackgroundColor.getAlpha()) + "," +
	                                      Integer.toString(voiBackgroundColor.getRed()) + "," +
	                                      Integer.toString(voiBackgroundColor.getGreen()) + "," +
	                                      Integer.toString(voiBackgroundColor.getBlue()));
	                            closedTag( "FontName", fontName);
	                            closedTag( "FontSize", Integer.toString(fontSize));
	
	                            if (fontDescriptors == Font.BOLD) {
	                                closedTag( "FontStyle", "BOLD");
	                            } else if (fontDescriptors == Font.ITALIC) {
	                                closedTag( "FontStyle", "ITALIC");
	                            } else if (fontDescriptors == (Font.BOLD + Font.ITALIC)) {
	                                closedTag( "FontStyle", "BOLDITALIC");
	                            } else {
	                                closedTag( "FontStyle", "");
	                            }
	
	                            openTag("Label", false);
                        	}
                        }
                    }


                }
            }

            openTag("Annotation", false);
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
        //TODO: Make sure other local instances of bw do not cause NullPointerExceptions
        //BufferedWriter already exists in FileXML.   FileVOI extends FileXML as of version 2209
        //single copy is necessary for correct implementation of openTag(), closeTag(), etc
        //BufferedWriter bw;

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

                chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[]{".xml", ".voi"}));

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

            openTag("VOI xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);
            closedTag( "Unique-ID", Integer.toString(voi.getUID()));
            closedTag( "Curve-type", Integer.toString(voi.getCurveType()));
            closedTag( "Color",
                      Integer.toString(voi.getColor().getAlpha()) + "," + Integer.toString(voi.getColor().getRed()) +
                      "," + Integer.toString(voi.getColor().getGreen()) + "," +
                      Integer.toString(voi.getColor().getBlue()));

            closedTag( "Thickness", Integer.toString(voi.getThickness()));
            

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

                boolean saveAsLPS = Preferences.is(Preferences.PREF_VOI_LPS_SAVE);
                //System.err.println("SaveAsLPS: " + saveAsLPS);
                boolean is2D = (image.getNDims() == 2);
                
                for (i = 0; i < pSize; i++) {

                    tempVOIItem = (VOISortItem) pointVector.elementAt(i);
                    tempBase = tempVOIItem.getVOIBase();

                    openTag("Contour", true);

                    // save old format if image dim is 2
                    if (is2D || !saveAsLPS) {
                        closedTag( "Slice-number", Integer.toString(tempVOIItem.getSlice()));
                    }

                    nPts = tempBase.size();

                    if (nPts > x.length) {
                        x = new float[nPts];
                        y = new float[nPts];
                        z = new float[nPts];
                    }

                    tempBase.exportArrays(x, y, z);

                    if (!is2D && saveAsLPS) {
                        Vector3f ptIn = new Vector3f();
                        Vector3f ptOut = new Vector3f();
                        int slice = tempVOIItem.getSlice();

                        for (m = 0; m < nPts; m++) {
                            ptIn.X = x[m];
                            ptIn.Y = y[m];
                            ptIn.Z = slice;
                            MipavCoordinateSystems.fileToScanner(ptIn, ptOut, image);

                            // System.err.println("Pt in: " + ptIn + ", Pt out: " + ptOut);
                            closedTag( "Pt",
                                      Float.toString(ptOut.X) + "," + Float.toString(ptOut.Y) + "," +
                                      Float.toString(ptOut.Z));
                        }
                    } else {
                        // image dim is 2 (or SaveAsLPS false).... save to old format

                        for (m = 0; m < nPts; m++) {
                            closedTag( "Pt", Float.toString(x[m]) + "," + Float.toString(y[m]));
                        }

                    }

                    openTag("Contour", false);
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
                                openTag("Contour", true);

                                // save old format if image dim is 2
                                if (image.getNDims() == 2) {
                                    closedTag( "Slice-number", Integer.toString(i));
                                }

                                nPts = ((Vector) (contours[i].elementAt(j))).size();

                                if (x.length < nPts) {
                                    x = new float[nPts];
                                    y = new float[nPts];
                                    z = new float[nPts];
                                }

                                ((VOIBase) (contours[i].elementAt(j))).exportArrays(x, y, z);

                                if (image.getNDims() > 2) {
                                    Vector3f ptIn = new Vector3f();
                                    Vector3f ptOut = new Vector3f();
                                    int slice = i;

                                    for (m = 0; m < nPts; m++) {
                                        ptIn.X = x[m];
                                        ptIn.Y = y[m];
                                        ptIn.Z = slice;
                                        MipavCoordinateSystems.fileToScanner(ptIn, ptOut, image);
                                        closedTag( "Pt",
                                                  Float.toString(ptOut.X) + "," + Float.toString(ptOut.Y) + "," +
                                                  Float.toString(ptOut.Z));
                                    }
                                } else {
                                    // image is 2d

                                    for (k = 0; k < nPts; k++) {
                                        closedTag( "Pt", Float.toString(x[k]) + "," + Float.toString(y[k]));
                                    }
                                }

                                openTag("Contour", false);
                            }
                        }
                    }
                }
            }

            openTag("VOI", false);
            bw.close();
        } catch (Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
            e.printStackTrace();
        }
    }

    /**
     * Converts string point (x,y) to Vector2f point.
     *
     * @param  str  string to convert
     * @param  pt   point to return
     */
    private void decodeLine(String str, Vector2f pt) {
        int index;
        String xStr, yStr;

        index = str.indexOf(" ");
        xStr = str.substring(0, index).trim();
        pt.X = (Float.valueOf(xStr).floatValue());

        yStr = str.substring(index + 1).trim();
        pt.Y = (Float.valueOf(yStr).floatValue());
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
        Vector2f pt2D = new Vector2f();
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
                        x[k] = pt2D.X;
                        y[k] = pt2D.Y;
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
        Vector2f pt2D = new Vector2f();
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

                            if ((pt2D.X < 0) || (pt2D.Y < 0) || (pt2D.X >= image.getExtents()[0]) ||
                                    (pt2D.Y >= image.getExtents()[1])) {
                                throw (new IOException("VOI points exceed image bounds"));
                            }

                            xTemp[k] = pt2D.X;
                            yTemp[k] = pt2D.Y;
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
        Vector2f pt2D = new Vector2f();
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
                        x[k] = pt2D.X;
                        y[k] = pt2D.Y;
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
                    contourVector.addElement(new Vector2f(x, y));
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
                    x[index] = ((Vector2f) contourVector.elementAt(index)).X;
                    y[index] = ((Vector2f) contourVector.elementAt(index)).Y;
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
            } else if (currentKey.equals("Note")) {
            	voiText.setNote(elementBuffer);
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

                Vector3f ptIn = new Vector3f(x, y, z);
                Vector3f ptOut = new Vector3f();

                if (image.getNDims() > 2) {
                    int xDim = image.getExtents()[0];
                    int yDim = image.getExtents()[1];
                    int zDim = image.getExtents()[2];

                    Preferences.debug("New contour: " + "\n", Preferences.DEBUG_FILEIO);

                    MipavCoordinateSystems.scannerToFile(ptIn, ptOut, image);

                    if ((ptOut.X > xDim) || (ptOut.Y > yDim) || (ptOut.Z >= zDim)) {
                        MipavUtil.displayWarning("VOI in file out of image bounds:  Open VOI aborted");

                        return;
                    }
                    
                    //A mistake has occured in reading FileInfoBase.ORI_L2R_TYPE, all must now be flipped
                    if((ptOut.X < 0) && (ptOut.Y < 0) && (ptOut.Z < 0)) {

                    	ptOut.X = -ptOut.X;
                    	ptOut.Y = -ptOut.Y;
                    	ptOut.Z = -ptOut.Z;
                    }
                    
                    slice = (int) ptOut.Z;
                    System.out.println("Point: "+slice);

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
                    contourVector.addElement(new Vector3f(x, y, z));
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

                Vector3f ptIn = new Vector3f();
                Vector3f ptOut = new Vector3f();

                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];
                int zDim = image.getExtents()[2];

                Preferences.debug("New contour: " + "\n", Preferences.DEBUG_FILEIO);

                for (index = 0; index < contourVector.size(); index++) {
                    ptIn = (Vector3f) contourVector.elementAt(index);

                    // System.err.println("\tScanner coord: " + ptIn);
                    MipavCoordinateSystems.scannerToFile(ptIn, ptOut, image);

                    x[index] = MipavMath.round(Math.abs(ptOut.X));
                    y[index] = MipavMath.round(Math.abs(ptOut.Y));
                    z[index] = MipavMath.round(Math.abs(ptOut.Z));

                    // System.err.println("POINT OUT (scanner to file): " + x[index] + ", " + y[index] + ", " +
                    // z[index]);
                    Preferences.debug("\tScanner coord: " + ptIn + ", File coord: " + x[index] + ", " + y[index] +
                                      ", " + z[index] + "\n", Preferences.DEBUG_FILEIO);

                    if ((ptOut.X > xDim) || (ptOut.Y > yDim) || (ptOut.Z >= zDim)) {
                        throw new SAXException("VOI on file out of image bounds");
                    }
                }

                voi.importCurve(x, y, z, (int)z[0]);
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
