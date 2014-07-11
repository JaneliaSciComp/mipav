package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;
import java.net.URL;
import java.util.*;

import javax.swing.*;
import javax.xml.parsers.*;

import org.apache.xerces.jaxp.JAXPConstants;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Simple dialog to save the 3 VOIs ( Sagittal, Coronal, Axial ) for each view, and merge them into one cloud points.
 * The cloud points is written as a .ply file, which can be read from MeshLab software.
 * 
 * @version 09 Jan, 2009
 * @author Ruida Cheng
 */
public class JDialogSaveMergedVOIs extends JDialogBase {

    /*
     * Label for each VOI and ply file.
     */
    public JLabel labelAxialVOI, labelSagittalVOI, labelCoronalVOI, labelPlyFile;

    /**
     * Text field for each VOI and ply file.
     */
    public JTextField textFieldAxialVOI, textFieldSagittalVOI, textFieldCoronalVOI, textFieldPlyFile;

    /**
     * Choose button for each VOI and ply file.
     */
    public JButton buttonAxial, buttonSagittal, buttonCoronal, buttonPly;

    /*
     * VOIs panel to hold the VOI inputs and ply input.
     */
    public JPanel VOIsPanel;

    /**
     * Button panel to hold the OK button, Cancel button, and Help button.
     */
    public JPanel buttonPanel;

    /**
     * Msg box to show the Prostate surface analysis related info.
     */
    public JPanel msgPanel;

    /**
     * Axial VOI instance
     */
    private final InstanceVOI AxialVOIs;

    /**
     * Sagittal VOI instance
     */
    private final InstanceVOI SagittalVOIs;

    /**
     * Coronal VOI instance
     */
    private final InstanceVOI CoronalVOIs;

    /**
     * Ply file instance. This instance only uses the file dir and file name.
     */
    private final InstanceVOI PlyInstance;

    /**
     * Constructor for Merging the 3 VOIs and save into one cloudy points file.
     * 
     * @param theParentFrame
     */
    public JDialogSaveMergedVOIs(final Frame theParentFrame) {
        super(theParentFrame, false);

        init();
        AxialVOIs = new InstanceVOI(".xml", textFieldAxialVOI);
        SagittalVOIs = new InstanceVOI(".xml", textFieldSagittalVOI);
        CoronalVOIs = new InstanceVOI(".xml", textFieldCoronalVOI);
        PlyInstance = new InstanceVOI(".ply", textFieldPlyFile);

    }

    /**
     * handler the button click evens.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("ChooseAxial")) {
            AxialVOIs.selectFile();
        } else if (command.equals("ChooseSagittal")) {
            SagittalVOIs.selectFile();
        } else if (command.equals("ChooseCoronal")) {
            CoronalVOIs.selectFile();
        } else if (command.equals("ChoosePly")) {
            PlyInstance.selectFile();
        } else if (command.equals("Save")) {
            AxialVOIs.readXML();
            SagittalVOIs.readXML();
            CoronalVOIs.readXML();
            writePlyFile();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {

        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * Write the cloud points from the 3 VOIs into one .ply file. The .ply file is readable for MeshLab.
     */
    public void writePlyFile() {
        int i;
        FileWriter fwp;
        File filePly = null;
        PrintWriter plyFileWriter;
        final int ptSize = AxialVOIs.myContourVector.size() + SagittalVOIs.myContourVector.size()
                + CoronalVOIs.myContourVector.size();

        try {
            filePly = new File(PlyInstance.directory + PlyInstance.fileName);

            fwp = new FileWriter(filePly);
            plyFileWriter = new PrintWriter(fwp);
            // write header
            plyFileWriter.println("ply"); // object is ModelTriangleMesh
            plyFileWriter.println("format ascii 1.0");
            plyFileWriter.println("element vertex " + ptSize);
            plyFileWriter.println("property float32 x");
            plyFileWriter.println("property float32 y");
            plyFileWriter.println("property float32 z");
            plyFileWriter.println("element face " + 0);
            plyFileWriter.println("property list uint8 int32 vertex_indices");
            plyFileWriter.println("end_header");

            Vector3f v;
            for (i = 0; i < AxialVOIs.myContourVector.size(); i++) {
                v = (Vector3f) AxialVOIs.myContourVector.get(i);
                plyFileWriter.print(v.X);
                plyFileWriter.print(" ");
                plyFileWriter.print(v.Y);
                plyFileWriter.print(" ");
                plyFileWriter.println(v.Z);
            }
            for (i = 0; i < SagittalVOIs.myContourVector.size(); i++) {
                v = (Vector3f) SagittalVOIs.myContourVector.get(i);
                plyFileWriter.print(v.X);
                plyFileWriter.print(" ");
                plyFileWriter.print(v.Y);
                plyFileWriter.print(" ");
                plyFileWriter.println(v.Z);
            }
            for (i = 0; i < CoronalVOIs.myContourVector.size(); i++) {
                v = (Vector3f) CoronalVOIs.myContourVector.get(i);
                plyFileWriter.print(v.X);
                plyFileWriter.print(" ");
                plyFileWriter.print(v.Y);
                plyFileWriter.print(" ");
                plyFileWriter.println(v.Z);
            }

            plyFileWriter.close();
            dispose();
        } catch (final Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
            e.printStackTrace();
        }

    }

    /**
     * Sets up GUI and displays the dialog.
     */
    private void init() {
        setTitle("Merge VOIs and Save");
        //setResizable(false);
        cancelFlag = false;

        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;

        // message Panel
        msgPanel = new JPanel(new GridLayout(4, 1));
        final JLabel msg1 = new JLabel(
                "This panel is used for the prostate surface analysis. When the axial, sagittal, and coronal VOIs\n");
        final JLabel msg2 = new JLabel(
                "are extracted from each corresponding images, this panel merges the 3 VOIs into one cloudy\n");
        final JLabel msg3 = new JLabel(
                "points dataset. And, the dataset is saved as points data file of the .ply file format.\n\n");
        gbc.gridy = 1;
        msgPanel.add(msg1, gbc);
        gbc.gridy = 2;
        msgPanel.add(msg2, gbc);
        gbc.gridy = 3;
        msgPanel.add(msg3, gbc);

        VOIsPanel = new JPanel();
        VOIsPanel.setLayout(new GridLayout(4, 3));
        VOIsPanel.setBorder(buildTitledBorder("Choose the 3 VOIs (Axial, Sagittal, Coronal)"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        // Axial
        labelAxialVOI = new JLabel("Axial :");
        labelAxialVOI.setFont(serif12);
        labelAxialVOI.setForeground(Color.black);

        VOIsPanel.add(labelAxialVOI, gbc);

        textFieldAxialVOI = new JTextField(20);
        textFieldAxialVOI.setFont(serif12);

        gbc.gridx = 1;
        VOIsPanel.add(textFieldAxialVOI, gbc);

        buttonAxial = new JButton("Choose");
        buttonAxial.addActionListener(this);
        buttonAxial.setActionCommand("ChooseAxial");
        buttonAxial.setFont(serif12B);
        buttonAxial.setPreferredSize(MipavUtil.defaultButtonSize);

        gbc.gridx = 2;
        VOIsPanel.add(buttonAxial, gbc);

        // Sagittal
        gbc.gridy = 1;
        labelSagittalVOI = new JLabel("Sagittal :");
        labelSagittalVOI.setFont(serif12);
        labelSagittalVOI.setForeground(Color.black);

        VOIsPanel.add(labelSagittalVOI, gbc);

        textFieldSagittalVOI = new JTextField(20);
        textFieldSagittalVOI.setFont(serif12);

        gbc.gridx = 1;
        VOIsPanel.add(textFieldSagittalVOI, gbc);

        buttonSagittal = new JButton("Choose");
        buttonSagittal.addActionListener(this);
        buttonSagittal.setActionCommand("ChooseSagittal");
        buttonSagittal.setFont(serif12B);
        buttonSagittal.setPreferredSize(MipavUtil.defaultButtonSize);

        gbc.gridx = 2;
        VOIsPanel.add(buttonSagittal, gbc);

        // Coronal
        gbc.gridy = 2;
        labelCoronalVOI = new JLabel("Coronal :");
        labelCoronalVOI.setFont(serif12);
        labelCoronalVOI.setForeground(Color.black);

        VOIsPanel.add(labelCoronalVOI, gbc);

        textFieldCoronalVOI = new JTextField(20);
        textFieldCoronalVOI.setFont(serif12);

        gbc.gridx = 1;
        VOIsPanel.add(textFieldCoronalVOI, gbc);

        buttonCoronal = new JButton("Choose");
        buttonCoronal.addActionListener(this);
        buttonCoronal.setActionCommand("ChooseCoronal");
        buttonCoronal.setFont(serif12B);
        buttonCoronal.setPreferredSize(MipavUtil.defaultButtonSize);

        gbc.gridx = 2;
        VOIsPanel.add(buttonCoronal, gbc);

        // PlyFile Panel
        gbc.gridy = 3;
        labelPlyFile = new JLabel(".ply file :");
        labelPlyFile.setFont(serif12);
        labelPlyFile.setForeground(Color.black);

        VOIsPanel.add(labelPlyFile, gbc);

        textFieldPlyFile = new JTextField(20);
        textFieldPlyFile.setFont(serif12);

        gbc.gridx = 1;
        VOIsPanel.add(textFieldPlyFile, gbc);

        buttonPly = new JButton("Choose");
        buttonPly.addActionListener(this);
        buttonPly.setActionCommand("ChoosePly");
        buttonPly.setFont(serif12B);
        buttonPly.setPreferredSize(MipavUtil.defaultButtonSize);
        gbc.gridx = 2;
        VOIsPanel.add(buttonPly, gbc);

        // button Panel
        buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1, 3));
        gbc.gridx = 0;
        gbc.gridy = 0;
        buttonPanel.add(buildOKButton(), gbc);
        gbc.gridy = 1;
        buttonPanel.add(buildCancelButton(), gbc);
        gbc.gridy = 2;
        buttonPanel.add(buildHelpButton(), gbc);

        mainPanel.add(msgPanel);
        mainPanel.add(VOIsPanel);
        mainPanel.add(buttonPanel);

        getContentPane().add(mainPanel);
        pack();
        setVisible(true);

    }

    /**
     * Builds button panel consisting of OK, Cancel and Help buttons.
     * 
     * @return JPanel that has ok, cancel, and help buttons
     */
    protected JPanel buildButtons() {
        final JPanel buttonPanel = new JPanel();

        buttonPanel.add(buildOKButton());
        buttonPanel.add(buildCancelButton());
        buttonPanel.add(buildHelpButton());

        return buttonPanel;
    }

    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     * 
     * @return JButton ok button
     */
    protected JButton buildOKButton() {
        OKButton = new JButton("Save");
        OKButton.addActionListener(this);
        OKButton.setActionCommand("Save");

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    /**
     * Builds the cancel button. Sets it internally as well return the just-built button.
     * 
     * @return JButton cancel button
     */
    protected JButton buildCancelButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        cancelButton.setActionCommand("Cancel");

        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);

        return cancelButton;
    }

    /**
     * Builds the help button. Sets it internally as well return the just-built button.
     * 
     * @return JButton help button
     */
    protected JButton buildHelpButton() {
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);
        helpButton.setActionCommand("Help");
        helpButton.setToolTipText("Find help for this screen.");
        helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setFont(serif12B);

        return helpButton;
    }

}

/**
 * The VOI instance holds the file name and directory name for each type of the VOIs (Axial, Sagittal, Coronal)
 * Additionally, myContourVector is the chunk vector to hold the 3D coordinate points of VOIs for each type. We assume
 * only read XML type ( voi_coord.xsd ) voi file.
 */
class InstanceVOI {

    // xml, ply file name.
    String fileName;

    // xml, ply file directory
    String directory;

    // .xml or .ply filtering in file chooser
    String file_suffix;

    // Textfield for each VOIs.
    JTextField textField;

    // File chooser for selecting the VOI file and the ply file.
    JFileChooser chooser = new JFileChooser();

    // default user interface.
    private final ViewUserInterface UI;

    // XML file parser handler
    MyXMLHandler handler;

    // chunk vector to hold all the points coordinates reading from VOI xml file.
    Vector<Vector3f> myContourVector = new Vector<Vector3f>();

    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /*
     * Constructor to hold the basic attributes in InstanceVOI.
     */
    public InstanceVOI(final String _suffix, final JTextField _textField) {

        file_suffix = _suffix;
        textField = _textField;
        UI = ViewUserInterface.getReference();
    }

    /**
     * Select either VOI file or Ply file.
     */
    public void selectFile() {
        chooser.setDialogTitle("Open VOI");

        if (UI.getDefaultDirectory() != null) {
            final File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {file_suffix}));

        final int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            // UI.setDefaultDirectory(directory);
            textField.setText(fileName);
        } else {
            return;
        }

    }

    /**
     * Reads in the older VOI XML schema (using pixel coordinates)
     * 
     * @param voi VOI
     * 
     * @return boolean
     */
    public boolean readXML() {

        final SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);
        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            final SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(JAXPConstants.JAXP_SCHEMA_LANGUAGE, InstanceVOI.W3C_XML_SCHEMA);

            final URL xsdURL = getClass().getClassLoader().getResource("voi_coord.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find VOI XML schema.");

                return false;
            }

            saxParser.setProperty(JAXPConstants.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            final XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            handler = new MyXMLHandler();
            xmlReader.setContentHandler(handler);

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(directory + fileName));

            myContourVector = handler.getContourVector();

        } catch (final Exception error) {
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        // myContourVector = handler.getContourVector();
        return true;
    }

}

/**
 * Handle events generated while parsing the XML file. This MyXMLHandler is a special one. We just need to save the Pts
 * from the VOI xml file into one chunk vector. We don't need to save the point coordinates into the VOI structure.
 * 
 */
class MyXMLHandler extends DefaultHandler {

    /** The contours of the VOI we are building. */
    private final Vector<Vector3f> contourVector;

    /** The current XML tag we are parsing. */
    private String currentKey;

    /** The data for the current element being parsed. */
    private String elementBuffer = new String();

    /** The slice the VOI contour should be on. */
    @SuppressWarnings("unused")
    private int sliceNumber = 0;

    /**
     * Construct our custom XML data handler.
     * 
     * @param voi the VOI we should build from the XML file data
     */
    public MyXMLHandler() {
        contourVector = new Vector<Vector3f>();
    }

    /**
     * Get the whole contour vector generated from the xml file.
     * 
     * @return contour vector
     */
    public Vector<Vector3f> getContourVector() {
        return contourVector;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param ch char[]
     * @param start int
     * @param length int
     */
    public void characters(final char[] ch, final int start, final int length) {
        final String s = new String(ch, start, length);

        // don't need to de-entity-ize the string because the parser does
        // that automatically
        if (s.trim().length() != 0) {
            elementBuffer += s;
        }
    }

    /**
     * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is generally
     * saved to the image info.
     * 
     * @param namespaceURI the namespace (not used)
     * @param localName the current tag we are parsing
     * @param qName ? (not used)
     * 
     * @throws SAXException if there is a problem with the parser
     */
    public void endElement(final String namespaceURI, final String localName, final String qName) throws SAXException {
        currentKey = localName;

        if (currentKey.equals("Unique-ID")) {
            Integer.parseInt(elementBuffer);
        } else if (currentKey.equals("Curve-type")) {
            Integer.parseInt(elementBuffer);
        } else if (currentKey.equals("Color")) {
        	@SuppressWarnings("unused")
            int a = 0, r = 0, g = 0, b = 0;
            final StringTokenizer st = new StringTokenizer(elementBuffer, ",");

            try {
                a = Integer.parseInt(st.nextToken());
                r = Integer.parseInt(st.nextToken());
                g = Integer.parseInt(st.nextToken());
                b = Integer.parseInt(st.nextToken());

                // voi.setColor(new Color(r, g, b, a));
            } catch (final NumberFormatException ex) {
                Preferences.debug("Point is incorrectly formatted: " + ex.toString() + "\n", Preferences.DEBUG_FILEIO);
            }
        } else if (currentKey.equals("Thickness")) {
            Integer.parseInt(elementBuffer);
        } else if (currentKey.equals("Slice-number")) {
            sliceNumber = Integer.parseInt(elementBuffer);
        } else if (currentKey.equals("Pt")) {
            float x = 0f, y = 0f, z = 0f;
            final StringTokenizer st = new StringTokenizer(elementBuffer, ",");

            try {
                x = Float.parseFloat(st.nextToken());
                y = Float.parseFloat(st.nextToken());
                z = Float.parseFloat(st.nextToken());
                contourVector.addElement(new Vector3f(x, y, z));
            } catch (final NumberFormatException nfex) {
                Preferences.debug("Error reading pt: " + nfex.toString() + "\n", Preferences.DEBUG_FILEIO);
            }
        } else if (currentKey.equals("Contour")) {

            // Omit this part. Since, we just need to read the Pt from the XML file and save those points
            // into a vector array.
            // finished adding points to contour.. now add to VOI
            /*
             * int index; float[] x, y, z;
             * 
             * x = new float[contourVector.size()]; y = new float[contourVector.size()]; z = new
             * float[contourVector.size()];
             * 
             * 
             * for (index = 0; index < contourVector.size(); index++) { x[index] = ((Vector3f)
             * contourVector.elementAt(index)).X; y[index] = ((Vector3f) contourVector.elementAt(index)).Y; z[index] =
             * ((Vector3f) contourVector.elementAt(index)).Z; }
             */
        }

    }

    /**
     * Parser calls this for each element in a document.
     * 
     * @param namespaceURI the namespace (not used)
     * @param localName the current tag we are parsing
     * @param qName ? (not used)
     * @param atts attributes for the current tag
     * 
     * @throws SAXException if there is a problem with the parser
     */
    public void startElement(final String namespaceURI, final String localName, final String qName,
            final Attributes atts) throws SAXException {
        currentKey = localName;
        elementBuffer = "";

        if (currentKey.equals("Contour")) {
            // contourVector.clear();
        }
        // else if (currentKey.equals("VOI")) {
        // voi.setName(Integer.toString(voi.getUID()));
        // }
    }

}

/**
 * Error handler to report errors and warnings from the XML parser. Implements standard SAX ErrorHandler methods, see
 * SAX docs for more info.
 * 
 * @see FileOME
 * @see FileVOI
 * @see FileXML
 */
class XMLErrorHandler implements ErrorHandler {

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Handles parse exception errors by passing the parse exception up as a SAXException.
     * 
     * @param spe the parse exception
     * 
     * @throws SAXException passed up with the parse exception info
     */
    public void error(final SAXParseException spe) throws SAXException {
        final String message = "Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }

    /**
     * Handles parse exception fatal errors by passing the parse exception up as a SAXException.
     * 
     * @param spe the parse exception
     * 
     * @throws SAXException passed up with the parse exception info
     */
    public void fatalError(final SAXParseException spe) throws SAXException {
        final String message = "Fatal Error: " + getParseExceptionInfo(spe);

        throw new SAXException(message);
    }

    /**
     * Handles parse exception warnings by outputting them to the debug window.
     * 
     * @param spe the parse exception
     * 
     * @throws SAXException not reported for warnings
     */
    public void warning(final SAXParseException spe) throws SAXException {
        Preferences.debug("Warning: " + getParseExceptionInfo(spe), Preferences.DEBUG_FILEIO);
    }

    /**
     * Returns a string describing parse exception details.
     * 
     * @param spe the parse exception
     * 
     * @return a string containing information about the exception
     */
    private String getParseExceptionInfo(final SAXParseException spe) {
        String systemId = spe.getSystemId();

        if (systemId == null) {
            systemId = "null";
        }

        final String info = "URI=" + systemId + " Line=" + spe.getLineNumber() + ": " + spe.getMessage();

        return info;
    }
}
