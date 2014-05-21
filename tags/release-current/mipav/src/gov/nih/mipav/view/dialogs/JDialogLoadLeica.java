package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to remove separate slices in an image. Dialog asks which slices the user wishes to remove; it
 * provides buttons to mark all slices for removal and to de-select any slices from image removal; it gives options to
 * remove or to cancel. Allows 3D or 4D images; 2D images would not make sense with this operation.**(as of 25 Oct, does
 * not yet rename removed slice image when saving)**(as of 1 November, does not yet process the more complicated DICOM
 * images completely.
 */
public class JDialogLoadLeica extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5662541302525039992L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final File headerFile;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private JList seriesList;

    /** DOCUMENT ME! */
    private final boolean successful = false; // indicates status of algorithm

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogLoadLeica object.
     * 
     * @param headerFile DOCUMENT ME!
     */
    public JDialogLoadLeica(final File headerFile) {
        super(false);
        this.headerFile = headerFile;
        init();
    }

    /**
     * Creates a new JDialogLoadLeica object.
     * 
     * @param theParentFrame DOCUMENT ME!
     * @param headerFile DOCUMENT ME!
     */
    public JDialogLoadLeica(final Frame theParentFrame, final File headerFile) {
        super(theParentFrame, false);
        this.headerFile = headerFile;
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("Open")) {

            if (seriesList.getSelectedValue() != null) {
                final LeicaSeries ser = (LeicaSeries) seriesList.getSelectedValue();
                loadLeica(ser);
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Details")) {
            // MipavUtil.showHelp("10079");
            //MipavUtil.showWebHelp("Inserting_slices_into_image_datasets#Removing_images_.28slices.29_from_datasets");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Accessor that returns the whether or not the algorithm completed successfully.
     * 
     * @return DOCUMENT ME!
     */
    public boolean isSuccessful() {
        return successful;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private boolean buildList() {
        final Vector<LeicaSeries> vec = new Vector<LeicaSeries>();
        BufferedReader br = null;

        try {
            br = new BufferedReader(new FileReader(headerFile));

            final boolean keepGoing = true;
            boolean lutFinished = false;
            String currentLine;

            LeicaSeries series = null;
            int[] dim = null;
            float[] res = null;
            int[] units = null;
            int[] channels = null;
            int currentDim = 0;

            while (keepGoing) {
                currentLine = br.readLine();

                // System.err.println(currentLine);
                if (currentLine == null) {

                    if ( (series != null) && !vec.contains(series) && seriesExists(series)) {
                        vec.add(series);
                    }

                    break;
                } else if (currentLine.startsWith("DIMENSION DESCRIPTION")) {
                    series = new LeicaSeries();
                    lutFinished = false;
                } else if (currentLine.startsWith("Number of Dimensions:")) {
                    currentDim = 0;
                    currentLine = currentLine.substring(21).trim();
                    dim = new int[Integer.parseInt(currentLine)];

                    // if number of dimensions is 3 (x by y by channels),
                    // go ahead and "new" the resolutions to [2]
                    if (dim.length == 3) {
                        res = new float[2];
                        units = new int[2];
                    }
                } else if (currentLine.startsWith("Dimension_")) {

                    // go to next line to find logical size
                    currentLine = br.readLine();

                    currentLine = currentLine.substring(13).trim();
                    dim[currentDim] = Integer.parseInt(currentLine);

                    // if we are on dim[2], we now know the # of channels
                    if (currentDim == 2) {
                        channels = new int[dim[currentDim]];
                    } else if (currentDim == 3) {

                        // if Dimension_3 is 1, this is a 2D series, otherwise it is 3D (or 2.5D)
                        if (dim[3] == 1) {
                            res = new float[2];
                            units = new int[2];
                        } else {
                            res = new float[3];
                            units = new int[3];
                        }
                    }

                    currentDim++;

                    if (currentDim == dim.length) {
                        series.setExtents(dim);
                    }
                } else if (currentLine.startsWith("Series Name:")) {
                    currentLine = currentLine.substring(12).trim();
                    series.setName(currentLine);
                } else if (currentLine.startsWith("Description:")) {
                    currentLine = currentLine.substring(12).trim();
                    series.setDescription(currentLine);
                } else if (currentLine.startsWith("Voxel-Width")) {
                    currentLine = currentLine.substring(11).trim();

                    final String unitsStr = currentLine.substring(currentLine.indexOf("[") + 1, currentLine
                            .indexOf("]"));

                    if (unitsStr.equalsIgnoreCase("\u00B5" + "m")) {
                        units[0] = Unit.MICROMETERS.getLegacyNum();
                    }

                    currentLine = currentLine.substring(currentLine.indexOf("]") + 1, currentLine.length()).trim();
                    res[0] = Float.parseFloat(currentLine);
                } else if (currentLine.startsWith("Voxel-Height")) {
                    currentLine = currentLine.substring(12).trim();

                    final String unitsStr = currentLine.substring(currentLine.indexOf("[") + 1, currentLine
                            .indexOf("]"));

                    if (unitsStr.equalsIgnoreCase("\u00B5" + "m")) {
                        units[1] = Unit.MICROMETERS.getLegacyNum();
                    }

                    currentLine = currentLine.substring(currentLine.indexOf("]") + 1, currentLine.length()).trim();

                    res[1] = Float.parseFloat(currentLine);

                    if (res.length == 2) {
                        series.setResolutions(res);
                        series.setUnits(units);
                    }
                } else if (currentLine.startsWith("Voxel-Depth") && (dim.length > 3)) {
                    currentLine = currentLine.substring(11).trim();

                    if (currentLine.indexOf("[") != -1) {
                        final String unitsStr = currentLine.substring(currentLine.indexOf("[") + 1, currentLine
                                .indexOf("]"));
                        currentLine = currentLine.substring(currentLine.indexOf("]") + 1, currentLine.length()).trim();

                        if (unitsStr.equalsIgnoreCase("\u00B5" + "m")) {
                            units[2] = Unit.MICROMETERS.getLegacyNum();
                        }

                        res[2] = Float.parseFloat(currentLine);
                        series.setResolutions(res);
                        series.setUnits(units);
                    }
                } else if (currentLine.startsWith("LUT_") && !lutFinished) {
                    currentLine = currentLine.substring(4).trim();

                    final int lutNum = Integer.parseInt(currentLine);

                    // ignore LUT info listed past the # of channels listed in dim[]
                    if (lutNum < dim[2]) {
                        currentLine = br.readLine();
                        currentLine = currentLine.substring(5).trim();

                        if (currentLine.equalsIgnoreCase("red")) {
                            channels[lutNum] = 0;
                        } else if (currentLine.equalsIgnoreCase("green")) {
                            channels[lutNum] = 1;
                        } else if (currentLine.equalsIgnoreCase("blue")) {
                            channels[lutNum] = 2;
                        } else if (currentLine.equalsIgnoreCase("gray")) {
                            channels[lutNum] = -1;
                        }
                    }

                    if (lutNum == (dim[2] - 1)) {
                        series.setChannels(channels);
                        lutFinished = true;
                    }

                } else if (currentLine.startsWith("Number of Images:")) {

                    // it seems the first listing of this "number of images" does not
                    // pertain to the first series... remains to be seen
                    if (series != null) {
                        currentLine = currentLine.substring(17).trim();
                        series.setNumImages(Integer.parseInt(currentLine));
                    }

                } else if (currentLine
                        .startsWith("*************************************** NEXT IMAGE *********************************")) {

                    if (seriesExists(series) && sortLeicaFiles(series)) {
                        vec.add(series);
                    }
                }
            }
        } catch (final IOException ex) {
            MipavUtil.displayError("Leica header parsing failed.");

            return false;
        } catch (final Exception ex) {
            MipavUtil.displayError("Leica header parsing failed.");

            return false;
        }

        seriesList = new JList(vec) {

            // This method is called as the cursor moves within the list.
            public String getToolTipText(final MouseEvent evt) {

                // Get item index
                final int index = locationToIndex(evt.getPoint());

                // Get item
                final LeicaSeries series = (LeicaSeries) getModel().getElementAt(index);

                // Build tool tip string
                String tipString = new String();
                tipString += "X Dim: " + series.getExtents()[0] + ", Y Dim: " + series.getExtents()[1];

                if (series.getResolutions().length > 2) {
                    tipString += ", Z Dim: " + series.getExtents()[3];
                }

                tipString += ", Image Type: ";

                if (series.getResolutions().length > 2) {
                    tipString += "RGB";
                } else {
                    tipString += "UShort";
                }

                tipString += ", X Res: " + series.getResolutions()[0] + ", Y Res: " + series.getResolutions()[1];

                if (series.getResolutions().length > 2) {
                    tipString += ", Z Res: " + series.getResolutions()[2];
                }

                return tipString;
            }
        };

        final MouseListener seriesListener = new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {

                if (e.getClickCount() == 2) {
                    // int index = seriesList.locationToIndex(e.getPoint());

                    actionPerformed(new ActionEvent(OKButton, 0, "Open"));
                }
            }
        };
        seriesList.addMouseListener(seriesListener);

        seriesList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        seriesList.setListData(vec);

        return true;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        final JPanel mainPanel = new JPanel(new BorderLayout()); // everything gets placed on this panel

        setTitle("Select Leica series");
        setForeground(Color.black);

        if (buildList()) {

            // make the list scroll if there are enough checkboxes
            scrollPane = new JScrollPane(seriesList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            mainPanel.add(scrollPane);
            mainPanel.setBorder(buildTitledBorder("Available Leica series listed in header"));
            mainPanel.setPreferredSize(new Dimension(210, 390));

            final JPanel buttonPanel = new JPanel(new FlowLayout());
            buttonPanel.add(buildButtons());
            OKButton.setText("Open");
            cancelButton.setText("Cancel");
            helpButton.setVisible(false);
            // helpButton.setText("Cancel");

            final JPanel panel = new JPanel(new BorderLayout());
            panel.add(mainPanel); // put the main panel into the center of the dialog
            panel.add(buttonPanel, BorderLayout.SOUTH);
            panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            getContentPane().add(panel);
            pack();
            setSize(350, 474);
            setVisible(true); // let someone see the dialog.
        } else {
            dispose();
            setVisible(false);
        }
    }

    /**
     * Loads the selected Leica series into a new ViewJFrameImage uses the FileTiff to read each Tiff image and
     * construct the appropriate model image based on the LeicaSeries attributes which were read in from the text
     * header.
     * 
     * @param series LeicaSeries
     */
    private void loadLeica(final LeicaSeries series) {

        ModelImage image = null;
        ModelLUT lut = null;
        FileTiff tempTiff = null;

        try {

            if (series.getNumImages() == 1) {
                String tempPath, tempDir, tempName;

                // get the file name and directory
                tempPath = (String) series.getFileNames().elementAt(0);
                tempDir = tempPath.substring(0, tempPath.lastIndexOf(File.separator) + 1);
                tempName = tempPath.substring(tempPath.lastIndexOf(File.separator) + 1, tempPath.length());
                tempTiff = new FileTiff(tempName, tempDir);

                image = tempTiff.readImage(false, false);
                lut = tempTiff.getModelLUT();
                image.getFileInfo()[0].setResolutions(new float[] {series.getResolutions()[0],
                        series.getResolutions()[1]});
                image.getFileInfo()[0].setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), 0);
                image.setImageName(series.getName());
            } else {
                tempTiff = new FileTiff("", "");
                image = tempTiff.readLeicaSeries(series);
            }

            if (image != null) {
                image.calcMinMax();
                new ViewJFrameImage(image, lut, null, false);
            }
        } catch (final IOException ioex) {
            ioex.printStackTrace();
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param leica DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private boolean seriesExists(final LeicaSeries leica) {
        int fileCounter = 0;

        final File directoryFile = headerFile.getParentFile();

        if (directoryFile.isDirectory()) {

            final File[] allFiles = directoryFile.listFiles(new FileFilter() {
                public boolean accept(File f) {

                    if (f.getPath().endsWith("tif") || f.getParent().endsWith("TIF")) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });

            // create a Vector to store each file name in the series
            // which will be passed into the series object for actual
            // Tiff file reading
            final Vector<String> fileNames = new Vector<String>(leica.getNumImages(), 0);

            // see if they contain the name + "_"
            for (final File element : allFiles) {
           
                if (element.getPath().indexOf(leica.getName() + "_") != -1) {
                    fileNames.add(fileCounter, element.getPath());
                    fileCounter++;
                }
            }

            if (fileNames.size() == leica.getNumImages()) {
                leica.setFileNames(fileNames);
            }

            return (fileCounter == leica.getNumImages());

        }

        return false;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param series DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private boolean sortLeicaFiles(final LeicaSeries series) {

        final Vector<String> fileNames = series.getFileNames();

        if (fileNames != null) {
            final LeicaFileComparator lComp = new LeicaFileComparator(series.getChannels(), series.getName());
            Collections.sort(fileNames, lComp);

            if (lComp.getGrayIndices() != null) {
                final int size = fileNames.size();

                int numToRemove = lComp.getGrayIndices().length;

                if (series.getExtents().length > 3) {
                    numToRemove *= series.getExtents()[3];
                }

                for (int i = size - 1; i >= (size - numToRemove); i--) {
                    fileNames.remove(i);
                }
            }
        } else {
            return false;
        }

        return true;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public class LeicaFileComparator implements Comparator<String> {

        /** DOCUMENT ME! */
        private final int[] channelOrder;

        /** DOCUMENT ME! */
        private int[] grayIndices = null;

        /** DOCUMENT ME! */
        private final String seriesName;

        /**
         * Creates a new LeicaFileComparator object.
         * 
         * @param channelOrder DOCUMENT ME!
         * @param seriesName DOCUMENT ME!
         */
        public LeicaFileComparator(final int[] channelOrder, final String seriesName) {
            this.channelOrder = channelOrder;
            this.seriesName = seriesName;

            // set the gray index if there is a gray (unused) channel
            final Vector<Integer> grayVector = new Vector<Integer>();

            for (int i = 0; i < channelOrder.length; i++) {

                if (channelOrder[i] == -1) {
                    grayVector.add(new Integer(i));
                }
            }

            if (grayVector.size() > 0) {
                grayIndices = new int[grayVector.size()];

                for (int i = 0; i < grayIndices.length; i++) {
                    grayIndices[i] = ((Integer) grayVector.elementAt(i)).intValue();
                }
            }

        }

        /**
         * DOCUMENT ME!
         * 
         * @param a DOCUMENT ME!
         * @param b DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final String a, String b) {

            // Strip everything but the z and channel information from the file names;
            String aEnd = a;
            String bEnd = b;
            aEnd = aEnd.substring(aEnd.indexOf(seriesName + "_") + seriesName.length() + 1, aEnd.length() - 4);
            bEnd = bEnd.substring(bEnd.indexOf(seriesName + "_") + seriesName.length() + 1, bEnd.length() - 4);

            // now use these chopped strings to parse the z and channel values

            int zA, zB, channelA, channelB;

            if (aEnd.indexOf("_") != -1) {
                zA = Integer.parseInt(aEnd.substring(1, aEnd.indexOf("_")));
                zB = Integer.parseInt(bEnd.substring(1, bEnd.indexOf("_")));

                channelA = Integer.parseInt(aEnd.substring(aEnd.indexOf("_") + 3, aEnd.length()));
                channelB = Integer.parseInt(bEnd.substring(bEnd.indexOf("_") + 3, bEnd.length()));

                // if either or both is gray index... return the result comparison
                if (grayIndices != null) {
                    boolean aGray = false;
                    boolean bGray = false;

                    for (final int element : grayIndices) {

                        if (channelA == element) {
                            aGray = true;
                        }

                        if (channelB == element) {
                            bGray = true;
                        }
                    }

                    if (aGray) {

                        if ( !bGray) {
                            return 1;
                        } else {
                            return 0;
                        }
                    } else if (bGray) {

                        if ( !aGray) {
                            return -1;
                        } else {
                            return 0;
                        }
                    }
                }

                // now compare the z for each
                if (zA > zB) {
                    return 1;
                } else if (zA < zB) {
                    return -1;
                } else if (zA == zB) {

                    if (channelOrder[channelA] < channelOrder[channelB]) {
                        return -1;
                    } else if (channelOrder[channelA] > channelOrder[channelB]) {
                        return 1;
                    }
                }

            } else {
                channelA = Integer.parseInt(aEnd.substring(aEnd.indexOf("ch") + 2, aEnd.length()));
                channelB = Integer.parseInt(bEnd.substring(bEnd.indexOf("ch") + 2, bEnd.length()));

                if (channelOrder[channelA] < channelOrder[channelB]) {
                    return -1;
                } else if (channelOrder[channelA] > channelOrder[channelB]) {
                    return 1;
                }
            }

            return 0;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int[] getGrayIndices() {
            return this.grayIndices;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public class LeicaSeries extends JComponent {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1101169071873919736L;

        /** DOCUMENT ME! */
        private int[] channels;

        /** DOCUMENT ME! */
        private int[] extents;

        /** DOCUMENT ME! */
        private Vector<String> fileNames = null;

        /** DOCUMENT ME! */
        private int numImages;

        /** DOCUMENT ME! */
        private float[] resolutions;

        /** DOCUMENT ME! */
        private String seriesName, description;

        /** DOCUMENT ME! */
        private int[] units;

        /**
         * Creates a new LeicaSeries object.
         */
        public LeicaSeries() {}

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int[] getChannels() {
            return this.channels;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public String getDescription() {
            return this.description;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int[] getExtents() {
            return this.extents;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Vector<String> getFileNames() {
            return this.fileNames;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public String getName() {
            return this.seriesName;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getNumImages() {
            return this.numImages;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public float[] getResolutions() {
            return this.resolutions;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int[] getUnits() {
            return this.units;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param channels DOCUMENT ME!
         */
        public void setChannels(final int[] channels) {
            this.channels = channels;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param description DOCUMENT ME!
         */
        public void setDescription(final String description) {
            this.description = description;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param extents DOCUMENT ME!
         */
        public void setExtents(final int[] extents) {
            this.extents = extents;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param names DOCUMENT ME!
         */
        public void setFileNames(final Vector<String> names) {
            this.fileNames = names;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param name DOCUMENT ME!
         */
        public void setName(final String name) {
            this.seriesName = name;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param num DOCUMENT ME!
         */
        public void setNumImages(final int num) {
            this.numImages = num;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param resolutions DOCUMENT ME!
         */
        public void setResolutions(final float[] resolutions) {
            this.resolutions = resolutions;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param units DOCUMENT ME!
         */
        public void setUnits(final int[] units) {
            this.units = units;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public String toString() {
            return new String(seriesName);
        }
    }

}
