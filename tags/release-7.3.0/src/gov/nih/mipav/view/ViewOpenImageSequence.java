package gov.nih.mipav.view;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.JDialogSelectChannelSequence;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.NumberFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.*;
import javax.swing.table.*;


// import com.mentorgen.tools.profile.runtime.Profile;

/**
 * The purpose of this class is to present the user with a window enabling him/her to open a sequence of TIFF files
 * based on timepoint-channel-slice ordering. This is useful for opening a dataset captured by a machine where the order
 * of the files on disk may not necessarily be the correct sequence from the machine. Also shows a preview of each
 * image.
 */
public class ViewOpenImageSequence extends JFrame implements ActionListener, PreviewImageContainer, MouseListener,
        KeyListener, ChangeListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5007701357886039L;

    /** DOCUMENT ME! */
    protected static final String CANCEL = "Close";

    /** DOCUMENT ME! */
    protected static final String OK = "Open";

    /** DOCUMENT ME! */
    protected static final String APPLY = "Apply";

    /** DOCUMENT ME! */
    protected static final String BROWSE = "Browse";

    /** DOCUMENT ME! */
    protected static final String FILTER = "Crop";

    /** DOCUMENT ME! */
    protected static final String REMOVE = "Remove";

    /** DOCUMENT ME! */
    protected static final String CONFIGURE_CHANNELS = "Configure channel order";

    /** DOCUMENT ME! */
    protected static final String ENABLE = "Enable";

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JSlider brightnessSlider;

    /** DOCUMENT ME! */
    protected AlternatingTableCellRenderer cellRenderer;

    /** DOCUMENT ME! */
    protected JCheckBox chkForceUBYTE;

    /** DOCUMENT ME! */
    protected JComboBox cmbFilter;

    /** DOCUMENT ME! */
    protected JSlider contrastSlider;

    /** DOCUMENT ME! */
    protected File currentPath;

    /** DOCUMENT ME! */
    protected JCheckBox enableCheckbox;

    /** DOCUMENT ME! */
    protected File[] fileListData;

    /** DOCUMENT ME! */
    // protected JList filenameList;
    /** DOCUMENT ME! */
    protected JLabel lblOrigDim;

    /** DOCUMENT ME! */
    protected String previewFilename;

    /** DOCUMENT ME! */
    protected JPanel previewPanel;

    /** Radio button for CTZ sequence */
    protected JRadioButton radCTZ = new JRadioButton("C-T-Z");

    /** Radio button for CZT sequence */
    protected JRadioButton radCZT = new JRadioButton("C-Z-T");

    /** Radio button for TCZ sequence */
    protected JRadioButton radTCZ = new JRadioButton("T-C-Z");

    /** Radio button for TZC sequence */
    protected JRadioButton radTZC = new JRadioButton("T-Z-C");

    /** Radio button for ZCT sequence */
    protected JRadioButton radZCT = new JRadioButton("Z-C-T");

    /** Radio button for ZTC sequence */
    protected JRadioButton radZTC = new JRadioButton("Z-T-C");

    /** DOCUMENT ME! */
    protected JTable table;

    /** DOCUMENT ME! */
    protected UneditableTableModel tableModel;

    /** DOCUMENT ME! */
    protected JTextField txtChannels;

    /** DOCUMENT ME! */
    protected JTextField txtDirectory;

    /** DOCUMENT ME! */
    protected JTextField txtHeight;

    /** DOCUMENT ME! */
    protected JTextField txtSlices;

    /** DOCUMENT ME! */
    protected JTextField txtTimePoints;

    /** DOCUMENT ME! */
    protected JTextField txtWidth;

    /** Int denoting CZT sequence */
    protected final int CZT = 3;

    /** Int denoting TCZ sequence */
    protected final int TCZ = 5;

    /** Int denoting CTZ sequence */
    protected final int CTZ = 2;

    /** Int denoting TZC sequence */
    protected final int TZC = 4;

    /** Int denoting ZCT sequence */
    protected final int ZCT = 0;

    /** Int denoting ZTC sequence */
    protected final int ZTC = 1;

    /** DOCUMENT ME! */
    int[] channelMap;

    /** DOCUMENT ME! */
    private SortingTableModel filenameTableModel;

    /** DOCUMENT ME! */
    private TableSorter filenameTableSorter;

    /** DOCUMENT ME! */
    private JTable filenameTable;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ViewOpenImageSequence object.
     */
    public ViewOpenImageSequence() {
        setSize(new Dimension(800, 760));

        MipavUtil.centerOnScreen(this);

        setTitle("Open image sequence");

        channelMap = new int[] {1, 2, 3, 0};

        buildUserInterface();

        setDefaults();

        setVisible(true);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals(ViewOpenImageSequence.BROWSE)) {
            final JFileChooser fileChooser = new JFileChooser(getLastOpenSequencePath());
            fileChooser.setMultiSelectionEnabled(false);

            if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
                final File selectedFile = fileChooser.getSelectedFile();

                txtDirectory.setText(selectedFile.getAbsolutePath());

                if (selectedFile.isDirectory()) {
                    fileListData = selectedFile.listFiles();
                    currentPath = selectedFile;
                } else {
                    currentPath = selectedFile.getParentFile();
                    fileListData = currentPath.listFiles();
                }

                // remember this path for the next time this component is used
                Preferences.setLastOpenSequencePath(currentPath.getAbsolutePath());

                Arrays.sort(fileListData);

                Vector<String> fileListVector = new Vector<String>();

                for (final File element : fileListData) {

                    if (element.isFile()) // only add files to the file list
                    {
                        fileListVector.addElement(element.getName());
                    }
                }

                // filter out files that don't have same file extension as selected file
                fileListVector = filterFileExtension(fileListVector, selectedFile);

                for (int i = 0; i < fileListVector.size(); i++) {
                    final Vector<String> newRow = new Vector<String>();
                    newRow.addElement(fileListVector.get(i));
                    filenameTableModel.addRow(newRow);
                }

                while (table.getModel().getRowCount() > 0) // reset table
                {
                    ((DefaultTableModel) table.getModel()).removeRow(0);
                }

                if (filenameTableModel.getRowCount() > 0) { // auto-select first entry
                    filenameTable.setRowSelectionInterval(0, 0);
                    makePreview(currentPath.getAbsolutePath() + File.separator, (String) filenameTable.getValueAt(0, 0));
                }
            }
        } else if (command.equals(ViewOpenImageSequence.APPLY)) {

            if (dimensionsSanityCheck() == false) {
                MipavUtil.displayError("One or more 'Dimensions' values invalid.");

                return;
            }

            if (filenameTable.getRowCount() == 0) {
                return;
            }
            // save dimensions parameters for next time
            Preferences.setLastOpenSequenceParams(txtSlices.getText(), txtChannels.getText(), txtTimePoints.getText(),
                    String.valueOf(getSelectedSequence()));

            // format the table according to the dimensions parameters
            formatTable(getSelectedSequence());
        } else if (command.equals(ViewOpenImageSequence.FILTER)) {

            // constrain list data to selected items
            keepSelected();
        } else if (command.equals(ViewOpenImageSequence.REMOVE)) {

            // crop list data from selected items
            removeSelected();
        } else if (command.equals(ViewOpenImageSequence.CANCEL)) {
            dispose();
        } else if (command.equals(ViewOpenImageSequence.OK)) {
            Dimension subsampleDimension = null;

            if (dimensionsSanityCheck() == false) {
                String dimFormat = "";
                switch (getSelectedSequence()) {

                    case ZCT:
                        dimFormat = radZCT.getText();
                        break;

                    case ZTC:
                        dimFormat = radZTC.getText();
                        break;

                    case TZC:
                        dimFormat = radTZC.getText();
                        break;

                    case TCZ:
                        dimFormat = radTCZ.getText();
                        break;

                    case CTZ:
                        dimFormat = radCTZ.getText();
                        break;

                    case CZT:
                        dimFormat = radCZT.getText();
                        break;
                }

                MipavUtil.displayError("One or more " + dimFormat + " dimensions values invalid.");
                return;
            }

            if (enableCheckbox.isSelected() && (subsamplingSanityCheck() == false)) {
                MipavUtil.displayError("Subsampling dimensions are invalid.");

                return;
            }

            if (enableCheckbox.isSelected()) {
                subsampleDimension = new Dimension(Integer.parseInt(txtWidth.getText()), Integer.parseInt(txtHeight
                        .getText()));
            }

            final int numChannels = Integer.parseInt(txtChannels.getText());
            final int numSlices = Integer.parseInt(txtSlices.getText());
            final int numTimePoints = Integer.parseInt(txtTimePoints.getText());

            openImage(numChannels, numSlices, numTimePoints, subsampleDimension); // based on dimensions parameters
        } else if (command.equals(ViewOpenImageSequence.CONFIGURE_CHANNELS)) {
            new JDialogSelectChannelSequence(this, channelMap); // to change order of color channels
        } else if (command.equals(ViewOpenImageSequence.ENABLE)) {
            txtHeight.setEnabled(enableCheckbox.isSelected());
            txtWidth.setEnabled(enableCheckbox.isSelected());
            chkForceUBYTE.setEnabled(enableCheckbox.isSelected());
        }
    }

    /**
     * Method is required by PreviewImageContainer interface to draw a preview image.
     * 
     * @return Dimension - indicating the size of the preview image area.
     */
    public Dimension getPanelSize() {
        return previewPanel.getSize();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void keyPressed(final KeyEvent event) {

    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void keyReleased(final KeyEvent event) {

        if (event.getSource() == table) {
            if (table.getSelectedColumn() != 0) {
                final String currentPath = fileListData[0].getParentFile().getAbsolutePath() + File.separatorChar;
                final String filename = (String) table.getModel().getValueAt(table.getSelectedRow(),
                        table.getSelectedColumn());

                makePreview(currentPath, filename);
            }
        }

    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void keyTyped(final KeyEvent event) {}

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void mouseClicked(final MouseEvent event) {}

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void mouseEntered(final MouseEvent event) {}

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void mouseExited(final MouseEvent event) {}

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void mousePressed(final MouseEvent event) {

        if (event.getButton() == MouseEvent.BUTTON1) {
            if (event.getSource() == table) {
                if (table.getSelectedColumn() != 0) {
                    final String currentPath = fileListData[0].getParentFile().getAbsolutePath() + File.separatorChar;
                    final String filename = (String) table.getModel().getValueAt(table.getSelectedRow(),
                            table.getSelectedColumn());

                    makePreview(currentPath, filename);
                }
            }
            if (event.getSource() == filenameTable) {
                final int selectedIndex = filenameTable.getSelectedRow();
                makePreview(currentPath.getAbsolutePath() + File.separator, (String) filenameTable.getValueAt(
                        selectedIndex, 0));
            }

        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void mouseReleased(final MouseEvent event) {

        if (event.getButton() == MouseEvent.BUTTON3) {
            final JPopupMenu popupMenu = new JPopupMenu();

            final JMenuItem menuItem = new JMenuItem(ViewOpenImageSequence.CONFIGURE_CHANNELS);
            menuItem.addActionListener(this);
            menuItem.setActionCommand(ViewOpenImageSequence.CONFIGURE_CHANNELS);
            popupMenu.add(menuItem);

            popupMenu.show((Component) event.getSource(), event.getX(), event.getY());
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param channelMap DOCUMENT ME!
     */
    public void newChannelMap(final int[] channelMap) {
        this.channelMap = channelMap;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param event DOCUMENT ME!
     */
    public void stateChanged(final ChangeEvent event) {
        final Component[] components = previewPanel.getComponents();

        for (final Component element : components) {

            if (element instanceof ViewJComponentPreviewImage) {
                final float contrast = (float) Math.pow(10.0, contrastSlider.getValue() / 200.0);

                ((ViewJComponentPreviewImage) element).setSliceBrightness(brightnessSlider.getValue(), contrast);
            }
        }
    }

    /**
     * Arrange the table in CTZ order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeCTZ(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < numChannels; k++) {
                    final Vector<String> zSliceVector = new Vector<String>();

                    for (int j = 0; j < numSlices; j++) {
                        /*
                         * nish zSliceVector.addElement(filenameList.getModel().getElementAt((j * numTimePoints *
                         * numChannels) + (numChannels * i) + k));
                         */
                        zSliceVector.addElement((String) filenameTable.getValueAt( (j * numTimePoints * numChannels)
                                + (numChannels * i) + k, 0));

                    }

                    timePointVector.addAll(zSliceVector);
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeCTZ - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * Arrange the table in CZT order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeCZT(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < numChannels; k++) {
                    final Vector<String> zSliceVector = new Vector<String>();

                    for (int j = 0; j < (numSlices * numChannels); j += numChannels) {
                        /*
                         * nish zSliceVector.addElement(filenameList.getModel().getElementAt(j + k + (numSlices *
                         * numChannels * i)));
                         */

                        zSliceVector.addElement((String) filenameTable.getValueAt(
                                j + k + (numSlices * numChannels * i), 0));

                    }

                    timePointVector.addAll(zSliceVector);
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeCZT - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * Arrange the table in TCZ order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeTCZ(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < numChannels; k++) {
                    final Vector<String> zSliceVector = new Vector<String>();

                    for (int j = 0; j < numSlices; j++) {
                        /*
                         * nish zSliceVector.addElement(filenameList.getModel().getElementAt((j * numTimePoints *
                         * numChannels) + (k * numTimePoints) + i));
                         */

                        zSliceVector.addElement((String) filenameTable.getValueAt( (j * numTimePoints * numChannels)
                                + (k * numTimePoints) + i, 0));
                    }

                    timePointVector.addAll(zSliceVector);
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeTCZ - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * Arrange the table in TZC order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeTZC(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < (numChannels * numSlices); k++) {
                    /*
                     * nish timePointVector.addElement(filenameList.getModel().getElementAt((k * numTimePoints) + i));
                     */

                    timePointVector.addElement((String) filenameTable.getValueAt( (k * numTimePoints) + i, 0));
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeTCZ - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * Arrange the table in ZCT order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeZCT(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < numChannels; k++) {
                    final Vector<String> zSliceVector = new Vector<String>();

                    for (int j = 0; j < numSlices; j++) {
                        /*
                         * nish zSliceVector.addElement(filenameList.getModel().getElementAt((j + (k * numSlices)) + (i *
                         * numSlices * numChannels)));
                         */

                        zSliceVector.addElement((String) filenameTable.getValueAt( (j + (k * numSlices))
                                + (i * numSlices * numChannels), 0));
                    }

                    timePointVector.addAll(zSliceVector);
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeZCT - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * Arrange the table in ZTC order.
     * 
     * @param numSlices int - the number of slices the data represents
     * @param numChannels int - the number of channels the data represents
     * @param numTimePoints int - the number of time points the data represents
     */
    protected void arrangeZTC(final int numSlices, final int numChannels, final int numTimePoints) {
        final Vector<String> channelVector = new Vector<String>();

        for (int i = 0; i < (numSlices * numChannels); i++) {

            if ( (i % numSlices) == 0) {
                channelVector.addElement("" + ( (i / numSlices) + 1));
            } else {
                channelVector.addElement("");
            }
        }

        tableModel.addColumn("Channel", channelVector);

        try {

            for (int i = 0; i < numTimePoints; i++) {
                final Vector<String> timePointVector = new Vector<String>();

                for (int k = 0; k < numChannels; k++) {
                    final Vector<String> zSliceVector = new Vector<String>();

                    for (int j = 0; j < numSlices; j++) {
                        /*
                         * nish zSliceVector.addElement(filenameList.getModel().getElementAt(j + (numSlices * i) +
                         * (numTimePoints * numSlices * k)));
                         */

                        zSliceVector.addElement((String) filenameTable.getValueAt(j + (numSlices * i)
                                + (numTimePoints * numSlices * k), 0));
                    }

                    timePointVector.addAll(zSliceVector);
                }

                tableModel.addColumn("Time point " + (i + 1), timePointVector);
            }
        } catch (final ArrayIndexOutOfBoundsException aioobe) {
            MipavUtil
                    .displayError("Index out of bounds in arrangeZTC - parameters indicate a dimension larger than available range of data.");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildBrightnessContrastPanel() {
        final int origBrightness = 0;
        brightnessSlider = new JSlider(SwingConstants.HORIZONTAL, -255, 255, origBrightness);

        brightnessSlider.setMajorTickSpacing(102);
        brightnessSlider.setPaintTicks(true);
        brightnessSlider.setEnabled(true);
        brightnessSlider.addChangeListener(this);

        final JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(MipavUtil.font12);

        final JLabel current = new JLabel(String.valueOf(origBrightness));
        current.setForeground(Color.black);
        current.setFont(MipavUtil.font12B);

        final JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(MipavUtil.font12);

        final JPanel sliderPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightnessSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbc.weighty = 1;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(BorderFactory.createTitledBorder("Level"));

        final int origContrast = 1;
        contrastSlider = new JSlider(SwingConstants.HORIZONTAL, -200, 200, (int) (Math.round(86.85889638 * Math
                .log(origContrast))));

        contrastSlider.setMajorTickSpacing(80);
        contrastSlider.setPaintTicks(true);
        contrastSlider.setEnabled(true);
        contrastSlider.addChangeListener(this);

        final JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(MipavUtil.font12);

        final NumberFormat nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        final JLabel current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
        current2.setForeground(Color.black);
        current2.setFont(MipavUtil.font12B);

        final JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(MipavUtil.font12);

        final JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contrastSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gbc.weighty = 1;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(BorderFactory.createTitledBorder("Window"));

        final JPanel centerPanel = new JPanel(new GridBagLayout());
        centerPanel.setBackground(Color.yellow);

        final GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        gbc2.weighty = 1;
        centerPanel.add(sliderPanel, gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        final JPanel brightnessContrastPanel = new JPanel(new BorderLayout());
        brightnessContrastPanel.add(centerPanel, BorderLayout.NORTH);
        brightnessContrastPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        return brightnessContrastPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildCenterPanel() {
        final GridBagConstraints gbConstraints = new GridBagConstraints();
        final GridBagLayout gbLayout = new GridBagLayout();

        final JPanel centerPanel = new JPanel(gbLayout);

        /** adding the 'Dimensions' panel * */
        final JPanel dimensionsSubPanel = buildDimensionsPanel();
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 0;
        gbConstraints.weightx = 1;
        gbConstraints.weighty = 0;
        gbConstraints.gridwidth = 2;
        gbConstraints.gridheight = 2;
        gbConstraints.insets = new Insets(4, 0, 1, 4);
        gbConstraints.anchor = GridBagConstraints.NORTHWEST;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbLayout.setConstraints(dimensionsSubPanel, gbConstraints);
        centerPanel.add(dimensionsSubPanel);

        /** done adding 'Dimensions' panel * */

        /** adding the 'Sequences' panel * */
        final JPanel sequenceSubPanel = buildSequencesPanel();
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 3;
        gbConstraints.insets = new Insets(1, 0, 1, 4);
        gbConstraints.anchor = GridBagConstraints.NORTHWEST;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbLayout.setConstraints(sequenceSubPanel, gbConstraints);
        centerPanel.add(sequenceSubPanel);

        /** done adding the 'Sequences' panel * */

        /** adding the 'Subsample' panel * */
        final JPanel subsamplePanel = buildSubsamplePanel();
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 5;
        gbConstraints.insets = new Insets(1, 0, 1, 4);
        gbConstraints.anchor = GridBagConstraints.NORTHWEST;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbLayout.setConstraints(subsamplePanel, gbConstraints);
        centerPanel.add(subsamplePanel);

        /** done adding the 'Subsample' panel * */

        // this section has been moved to buildSubsamplePanel()
        // gbConstraints.gridy = 7;
        // gbConstraints.gridheight = 1;
        // chkForceUBYTE = new JCheckBox("Force 8-bit");
        // chkForceUBYTE.setToolTipText("Force result grayscale images into unsigned byte, color images into ARGB");
        // gbLayout.setConstraints(chkForceUBYTE, gbConstraints);
        // centerPanel.add(chkForceUBYTE);
        // adding the 'Apply' button
        final JButton btnApply = new JButton("Apply >>");
        btnApply.setActionCommand(ViewOpenImageSequence.APPLY);
        btnApply.addActionListener(this);
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 8;
        gbConstraints.gridheight = 1;
        gbConstraints.gridwidth = 1;
        gbConstraints.weightx = 1;
        gbConstraints.anchor = GridBagConstraints.NORTHEAST;
        gbConstraints.insets = new Insets(1, 0, 4, 6);
        gbConstraints.fill = GridBagConstraints.NONE;
        gbLayout.setConstraints(btnApply, gbConstraints);
        centerPanel.add(btnApply);
        // done adding the 'Apply' button

        // add the image preview panel
        previewPanel = new JPanel();
        gbConstraints.gridy = 9;
        gbConstraints.gridheight = 10;
        gbConstraints.weighty = 20;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.insets = new Insets(0, 0, 0, 4);
        gbConstraints.anchor = GridBagConstraints.NORTH;
        gbConstraints.gridwidth = 2;
        gbLayout.setConstraints(previewPanel, gbConstraints);
        centerPanel.add(previewPanel);
        // done adding the image preview panel

        final JPanel infoPanel = new JPanel();
        lblOrigDim = new JLabel();
        lblOrigDim.setFont(MipavUtil.font10);
        infoPanel.add(lblOrigDim);
        gbConstraints.gridy = 19;
        gbConstraints.gridheight = 1;
        gbConstraints.weighty = 1;
        gbConstraints.insets = new Insets(0, 0, 0, 4);
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbConstraints.anchor = GridBagConstraints.SOUTH;
        gbLayout.setConstraints(infoPanel, gbConstraints);
        centerPanel.add(infoPanel);

        // add the brightness/contrast panel
        final JPanel bcPanel = buildBrightnessContrastPanel();
        gbConstraints.gridy = 20;
        gbConstraints.gridheight = 1;
        gbConstraints.weighty = 1;
        gbConstraints.anchor = GridBagConstraints.SOUTH;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbLayout.setConstraints(bcPanel, gbConstraints);
        centerPanel.add(bcPanel);
        // done adding the brightness/contrast panel

        return centerPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildDimensionsPanel() {
        final GridBagLayout centerPanelGBLayout = new GridBagLayout();
        final GridBagConstraints centerPanelGBConstraints = new GridBagConstraints();

        final JPanel centerSubPanel = new JPanel(centerPanelGBLayout);
        centerSubPanel.setBorder(new TitledBorder(BorderFactory.createEtchedBorder(), "Dimensions"));

        txtSlices = new JTextField();
        txtChannels = new JTextField();
        txtTimePoints = new JTextField();

        final JLabel lblSlices = new JLabel(" Slices (Z)");
        final JLabel lblChannels = new JLabel(" Channels (C)");
        final JLabel lblTimePoints = new JLabel(" Time points (T)");

        /** adding the 'Slices' textfield * */
        centerPanelGBConstraints.gridwidth = 1;
        centerPanelGBConstraints.weightx = 1;
        centerPanelGBConstraints.anchor = GridBagConstraints.WEST;
        centerPanelGBConstraints.fill = GridBagConstraints.HORIZONTAL;
        centerPanelGBLayout.setConstraints(txtSlices, centerPanelGBConstraints);
        centerSubPanel.add(txtSlices);

        /** adding the 'Slices' label * */
        centerPanelGBConstraints.gridx = 1;
        centerPanelGBConstraints.gridwidth = 2;
        centerPanelGBConstraints.fill = GridBagConstraints.NONE;
        centerPanelGBLayout.setConstraints(lblSlices, centerPanelGBConstraints);
        centerSubPanel.add(lblSlices);

        /** adding the 'Channels' textfield * */
        centerPanelGBConstraints.gridx = 0;
        centerPanelGBConstraints.gridy = 1;
        centerPanelGBConstraints.weightx = 1;
        centerPanelGBConstraints.gridwidth = 1;
        centerPanelGBConstraints.fill = GridBagConstraints.HORIZONTAL;
        centerPanelGBLayout.setConstraints(txtChannels, centerPanelGBConstraints);
        centerSubPanel.add(txtChannels);

        /** adding the 'Channels' label * */
        centerPanelGBConstraints.gridx = 1;
        centerPanelGBConstraints.gridwidth = 2;
        centerPanelGBConstraints.fill = GridBagConstraints.NONE;
        centerPanelGBLayout.setConstraints(lblChannels, centerPanelGBConstraints);
        centerSubPanel.add(lblChannels);

        /** adding the 'Time points' textfield * */
        centerPanelGBConstraints.gridx = 0;
        centerPanelGBConstraints.gridy = 2;
        centerPanelGBConstraints.gridwidth = 1;
        centerPanelGBConstraints.weightx = 1;
        centerPanelGBConstraints.fill = GridBagConstraints.HORIZONTAL;
        centerPanelGBLayout.setConstraints(txtTimePoints, centerPanelGBConstraints);
        centerSubPanel.add(txtTimePoints);

        /** adding the 'Time points' label * */
        centerPanelGBConstraints.gridx = 1;
        centerPanelGBConstraints.gridwidth = 2;
        centerPanelGBConstraints.fill = GridBagConstraints.NONE;
        centerPanelGBLayout.setConstraints(lblTimePoints, centerPanelGBConstraints);
        centerSubPanel.add(lblTimePoints);

        return centerSubPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildLeftSubPanel() {
        final JPanel leftSubPanel = new JPanel(new BorderLayout());
        leftSubPanel.setBorder(new TitledBorder(BorderFactory.createEtchedBorder(), "File list"));

        filenameTableModel = new SortingTableModel();
        filenameTableSorter = new TableSorter(filenameTableModel);
        filenameTable = new JTable(filenameTableSorter);
        filenameTable.addKeyListener(this);

        filenameTableSorter.setColumnComparator(new String().getClass(), TableSorter.LEXICAL_NUMS_COMPARATOR);
        filenameTableSorter.setTableHeader(filenameTable.getTableHeader());
        filenameTable.addMouseListener(this);
        filenameTableModel.addColumn("File Name");
        filenameTable.setDefaultRenderer(new String().getClass(), new MIPAVTableCellRenderer());

        /** adding the filename scroll pane * */
        // filenameList = new JList();
        // JScrollPane scrollPane = new JScrollPane(filenameList);
        // filenameList.addListSelectionListener(this);
        // filenameList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        final JScrollPane scrollPane = new JScrollPane(filenameTable);
        leftSubPanel.add(scrollPane, BorderLayout.CENTER);

        /** end adding the filename scroll pane * */

        final JPanel southPanel = new JPanel();

        /** adding the filter button * */
        final JButton btnFilter = new JButton(ViewOpenImageSequence.FILTER);
        btnFilter.addActionListener(this);
        btnFilter.setActionCommand(ViewOpenImageSequence.FILTER);
        btnFilter.setToolTipText("Constrain list to selected files");
        southPanel.add(btnFilter);

        final JButton btnRemove = new JButton(ViewOpenImageSequence.REMOVE);
        btnRemove.addActionListener(this);
        btnRemove.setActionCommand(ViewOpenImageSequence.REMOVE);
        btnRemove.setToolTipText("Remove selected files from list");
        southPanel.add(btnRemove);

        leftSubPanel.add(southPanel, BorderLayout.SOUTH);

        /** end adding the filter button * */

        return leftSubPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildOKCancelPanel() {
        final GridBagLayout gbLayout = new GridBagLayout();
        final GridBagConstraints gbConstraints = new GridBagConstraints();

        final JPanel subPanel = new JPanel(gbLayout);

        final JButton btnOK = new JButton(ViewOpenImageSequence.OK);
        btnOK.addActionListener(this);
        btnOK.setActionCommand(ViewOpenImageSequence.OK);
        gbConstraints.anchor = GridBagConstraints.EAST;
        gbConstraints.weightx = 1;
        gbConstraints.insets = new Insets(0, 0, 0, 2);
        gbLayout.setConstraints(btnOK, gbConstraints);
        subPanel.add(btnOK);

        final JButton btnCancel = new JButton(ViewOpenImageSequence.CANCEL);
        btnCancel.addActionListener(this);
        btnCancel.setActionCommand(ViewOpenImageSequence.CANCEL);
        gbConstraints.weightx = 0;
        gbConstraints.gridx = 1;
        gbLayout.setConstraints(btnCancel, gbConstraints);
        subPanel.add(btnCancel);

        return subPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildSequencesPanel() {
        final JPanel sequenceSubPanel = new JPanel(new GridLayout(3, 2));
        sequenceSubPanel.setBorder(new TitledBorder(BorderFactory.createEtchedBorder(), "Sequences"));

        final ButtonGroup buttonGroup = new ButtonGroup();

        radZCT.setSelected(true);

        buttonGroup.add(radZCT);
        buttonGroup.add(radZTC);
        buttonGroup.add(radCZT);
        buttonGroup.add(radCTZ);
        buttonGroup.add(radTZC);
        buttonGroup.add(radTCZ);

        sequenceSubPanel.add(radZCT);
        sequenceSubPanel.add(radZTC);
        sequenceSubPanel.add(radCZT);
        sequenceSubPanel.add(radCTZ);
        sequenceSubPanel.add(radTZC);
        sequenceSubPanel.add(radTCZ);

        return sequenceSubPanel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected JPanel buildSubsamplePanel() {
        final GridBagLayout gbLayout = new GridBagLayout();
        final GridBagConstraints gbConstraints = new GridBagConstraints();

        final JPanel subsamplePanel = new JPanel(gbLayout);
        subsamplePanel.setBorder(new TitledBorder(BorderFactory.createEtchedBorder(), "Subsampling"));

        gbConstraints.gridx = 1;
        gbConstraints.anchor = GridBagConstraints.WEST;
        enableCheckbox = new JCheckBox(ViewOpenImageSequence.ENABLE);
        enableCheckbox.addActionListener(this);
        enableCheckbox.setActionCommand(ViewOpenImageSequence.ENABLE);
        gbLayout.setConstraints(enableCheckbox, gbConstraints);
        subsamplePanel.add(enableCheckbox);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 1;
        gbConstraints.anchor = GridBagConstraints.EAST;

        final JLabel lblWidth = new JLabel("Width: ");
        gbLayout.setConstraints(lblWidth, gbConstraints);
        subsamplePanel.add(lblWidth);

        gbConstraints.gridx = 1;
        gbConstraints.anchor = GridBagConstraints.WEST;
        txtWidth = new JTextField(5);
        txtWidth.setEnabled(enableCheckbox.isSelected());
        gbLayout.setConstraints(txtWidth, gbConstraints);
        subsamplePanel.add(txtWidth);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 2;
        gbConstraints.anchor = GridBagConstraints.EAST;

        final JLabel lblHeight = new JLabel("Height: ");
        gbLayout.setConstraints(lblHeight, gbConstraints);
        subsamplePanel.add(lblHeight);

        gbConstraints.gridx = 1;
        gbConstraints.anchor = GridBagConstraints.WEST;
        txtHeight = new JTextField(5);
        txtHeight.setEnabled(enableCheckbox.isSelected());
        gbLayout.setConstraints(txtHeight, gbConstraints);
        subsamplePanel.add(txtHeight);

        gbConstraints.gridy++;
        chkForceUBYTE = new JCheckBox("Force 8-bit");
        chkForceUBYTE.setToolTipText("Force result images to use 8-bit channels");
        chkForceUBYTE.setEnabled(enableCheckbox.isSelected());
        gbLayout.setConstraints(chkForceUBYTE, gbConstraints);
        subsamplePanel.add(chkForceUBYTE);

        return subsamplePanel;
    }

    /**
     * DOCUMENT ME!
     */
    protected void buildUserInterface() {

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        final GridBagLayout gbLayout = new GridBagLayout();
        final GridBagConstraints gbConstraints = new GridBagConstraints();

        getContentPane().setLayout(gbLayout);

        /** adding the 'Browse' button * */
        final JButton btnBrowse = new JButton("Browse");
        btnBrowse.setActionCommand("Browse");
        btnBrowse.addActionListener(this);
        gbConstraints.anchor = GridBagConstraints.NORTHWEST;
        gbConstraints.insets = new Insets(4, 4, 0, 4);
        gbConstraints.gridwidth = 1;
        gbLayout.setConstraints(btnBrowse, gbConstraints);
        getContentPane().add(btnBrowse);

        /** end adding 'Browse' button * */

        // adding the 'directory' textfield
        txtDirectory = new JTextField();
        txtDirectory.setEditable(false);
        txtDirectory.setPreferredSize(new Dimension(txtDirectory.getPreferredSize().width,
                btnBrowse.getPreferredSize().height));

        gbConstraints.insets = new Insets(4, 4, 4, 4);
        gbConstraints.gridx = 1;
        gbConstraints.gridy = 0;
        gbConstraints.gridheight = 1;
        gbConstraints.weighty = 0;
        gbConstraints.weightx = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbConstraints.gridwidth = GridBagConstraints.REMAINDER;
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbLayout.setConstraints(txtDirectory, gbConstraints);
        getContentPane().add(txtDirectory);
        // end adding the 'directory' textfield

        // instantiate the table
        tableModel = new UneditableTableModel();
        table = new JTable(tableModel);
        cellRenderer = new AlternatingTableCellRenderer();
        cellRenderer.setColor2(new Color(204, 204, 255));
        table.addMouseListener(this);
        table.addKeyListener(this);
        table.setDefaultRenderer(Object.class, cellRenderer);
        table.setIntercellSpacing(new Dimension(1, 0));

        // table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        table.setShowGrid(false);
        table.setShowVerticalLines(true);
        table.setShowHorizontalLines(false);
        table.getTableHeader().setReorderingAllowed(false);

        final JScrollPane scrollPane = new JScrollPane(table);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        // end table instantiation

        final JSplitPane splitPanelChild = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, buildCenterPanel(), scrollPane);
        splitPanelChild.setDividerLocation(150);

        final JSplitPane splitPanelParent = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, buildLeftSubPanel(),
                splitPanelChild);
        splitPanelParent.setDividerLocation(200);
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 1;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.gridwidth = GridBagConstraints.REMAINDER;
        gbConstraints.gridheight = 6;
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbLayout.setConstraints(splitPanelParent, gbConstraints);
        getContentPane().add(splitPanelParent);

        final JPanel okCancelPanel = buildOKCancelPanel();
        gbConstraints.gridy = 8;
        gbConstraints.gridheight = 1;
        gbConstraints.weighty = 0;
        gbConstraints.insets = new Insets(0, 0, 2, 0);
        gbConstraints.gridwidth = GridBagConstraints.REMAINDER;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbLayout.setConstraints(okCancelPanel, gbConstraints);
        getContentPane().add(okCancelPanel);
    }

    /**
     * Method checks the dimensions parameters entered by the user. Returns true if valid, false otherwise.
     * 
     * @return boolean - if the dimension parameters are valid
     */
    protected boolean dimensionsSanityCheck() {
        final String slices = txtSlices.getText();
        final String channels = txtChannels.getText();
        final String timePoints = txtTimePoints.getText();

        try {
            final int numSlices = Integer.parseInt(slices);

            if (numSlices < 1) {
                return false;
            }

            final int numChannels = Integer.parseInt(channels);

            if (numChannels < 1) {
                return false;
            }

            final int numTimePoints = Integer.parseInt(timePoints);

            if (numTimePoints < 1) {
                return false;
            }

            return true;
        } catch (final NumberFormatException nfe) {
            return false;
        }
    }

    /**
     * Method filters out the rawFileList based of the file extension of the parameter selectedFile. This ensures that
     * the resulting Vector contains files only of the same extensions as selectedFile.
     * 
     * @param rawFileList Vector - a Vector of File objects
     * @param selectedFile File - the file whose extension will be used as a filter for rawFileList
     * 
     * @return Vector - a new Vector containing the filtered list of String objects representing filenames
     */
    protected Vector<String> filterFileExtension(Vector<String> rawFileList, File selectedFile) {

        if ( (rawFileList == null) || (selectedFile == null)) {
            return rawFileList;
        }

        String filename = selectedFile.getName();

        final int lastIndex = filename.lastIndexOf('.');

        if (lastIndex == -1) {
            return rawFileList;
        }

        final String extension = filename.substring(lastIndex);

        final Vector<String> filteredList = new Vector<String>();

        for (int i = 0; i < rawFileList.size(); i++) {
            filename = rawFileList.elementAt(i);

            if (filename.endsWith(extension)) {
                filteredList.addElement(filename);
            }
        }

        rawFileList = null;
        selectedFile = null;

        return filteredList;
    }

    /**
     * Ensures the table is drawn in the way specified by the dimensions parameters and the selected sequence.
     * 
     * @param selectedSequence int - the Z-T-C ordering as selected by the user
     */
    @SuppressWarnings("unchecked")
    protected void formatTable(final int selectedSequence) {
        tableModel.setColumnIdentifiers(new Vector()); // clear all columns

        tableModel.setRowCount(0); // clear all rows

        final int numSlices = Integer.parseInt(txtSlices.getText());
        final int numChannels = Integer.parseInt(txtChannels.getText());
        final int numTimePoints = Integer.parseInt(txtTimePoints.getText());

        cellRenderer.setAlternateRowCount(numSlices); // displays every other set of row in a different color

        switch (selectedSequence) {

            case ZCT:
                arrangeZCT(numSlices, numChannels, numTimePoints);
                break;

            case ZTC:
                arrangeZTC(numSlices, numChannels, numTimePoints);
                break;

            case TZC:
                arrangeTZC(numSlices, numChannels, numTimePoints);
                break;

            case TCZ:
                arrangeTCZ(numSlices, numChannels, numTimePoints);
                break;

            case CTZ:
                arrangeCTZ(numSlices, numChannels, numTimePoints);
                break;

            case CZT:
                arrangeCZT(numSlices, numChannels, numTimePoints);
                break;
        }

        table.getTableHeader().setReorderingAllowed(false); // do not allow reordering of table columns

        tableModel.fireTableStructureChanged(); // alert the table model to redraw itself
        tableModel.fireTableDataChanged();

        setColumnWidths(); // cosmetic method to set the widths of the columns according to the width of the text they
        // contain
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected File[] getFileList() {
        final Vector<String> imageList = new Vector<String>();

        for (int i = 1; i < tableModel.getColumnCount(); i++) {

            for (int j = 0; j < tableModel.getRowCount(); j++) {
                imageList.addElement((String) tableModel.getValueAt(j, i));
            }
        }

        final File[] fileList = new File[imageList.size()];

        for (int i = 0; i < imageList.size(); i++) {
            fileList[i] = new File(currentPath.getAbsolutePath() + File.separatorChar + imageList.elementAt(i));
        }

        return fileList;
    }

    /**
     * Reads the preferences file to determine the path that was last used in opening a file sequence.
     * 
     * @return File - the File object representing the directory last used in opening a file sequence.
     */
    protected File getLastOpenSequencePath() {
        final String srsPathName = Preferences.getLastOpenSequencePath();
        File srsPath;

        if (srsPathName != null) {
            srsPath = new File(srsPathName);

            if ( (srsPath.exists() == false) || (srsPath.canRead() == false)) {
                final String home = System.getProperty("user.home");
                //System.out.println("home: " + home);
                srsPath = new File(home);
            }
        } else {
            srsPath = new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR));

            if ( (srsPath.exists() == false) || (srsPath.canRead() == false)) {
                srsPath = new File(System.getProperty("user.home"));
            }
        }

        return srsPath;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected int getSelectedSequence() {

        if (radZCT.isSelected() == true) {
            return ZCT;
        } else if (radZTC.isSelected() == true) {
            return ZTC;
        } else if (radCTZ.isSelected() == true) {
            return CTZ;
        } else if (radCZT.isSelected() == true) {
            return CZT;
        } else if (radTZC.isSelected() == true) {
            return TZC;
        } else {
            return TCZ;
        }
    }

    /**
     * Will read a series of images and put them into a frame.
     * 
     * @param numChannels int - valid values are 1, 2, 3, and 4, which indicates the number of channels the image will
     *            have. "1" means a grayscale image
     * @param numSlices DOCUMENT ME!
     * @param numTimePoints DOCUMENT ME!
     * @param subsampleDimension DOCUMENT ME!
     */
    protected void openImage(final int numChannels, final int numSlices, final int numTimePoints,
            final Dimension subsampleDimension) {
        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);

        final File[] fileList = getFileList();

        if ( (fileList == null) || (fileList.length < 1)) {
            MipavUtil.displayError("Cannot open images because no images are selected.");

            return;
        }

        ModelImage resultImage = null;

        // Profile.clear();
        // Profile.start();

        // long time = System.currentTimeMillis();

        if (numChannels == 1) {
            resultImage = fileIO.readOrderedGrayscale(fileList, true, subsampleDimension, chkForceUBYTE.isSelected()
                    && enableCheckbox.isSelected(), numSlices, numTimePoints);
        } else {
            resultImage = fileIO.readOrderedARGB(fileList, numChannels, channelMap, true, subsampleDimension,
                    chkForceUBYTE.isSelected() && enableCheckbox.isSelected());
        }

        if (resultImage != null) {
            new ViewJFrameImage(resultImage);

        } else {
            MipavUtil.displayError("Unable to open image.");
        }

        // System.out.println(" ... " + (System.currentTimeMillis() - time));
        // Profile.stop();
        // Profile.setFileName( "profile_viewOpenImageSequence_openImage" );
        // Profile.shutdown();

        // -javaagent:C:\projects\mipav\src\lib\profile.jar
        // -Dprofile.properties=C:\projects\mipav\src\lib\profile.properties

    }

    /**
     * This method calculates the width needed to encompass the table data for each column. Without this method, the
     * column widths would not be set correctly and the user would potentialy have to resize each column to view the
     * filename it contains.
     */
    protected void setColumnWidths() {
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        final TableColumnModel columnModel = table.getColumnModel();
        final JTableHeader tableHeader = table.getTableHeader();

        // get FontMetrics object for the table header and the table rows
        final FontMetrics tableHeaderFontMetrics = tableHeader.getFontMetrics(tableHeader.getFont());
        final FontMetrics rowValueFontMetrics = table.getFontMetrics(table.getFont());

        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            final TableColumn column = columnModel.getColumn(i);
            int headerWidth = tableHeaderFontMetrics.stringWidth((String) column.getHeaderValue());

            for (int j = 0; j < tableModel.getRowCount(); j++) {
                final String rowValue = (String) tableModel.getValueAt(j, i);
                final int rowValueWidth = rowValueFontMetrics.stringWidth(rowValue);

                // the column width is the greater of the header width and the row data width
                headerWidth = Math.max(headerWidth, rowValueWidth);
            }

            column.setMinWidth(headerWidth + 4); // the + 4 is a padding - otherwise the text gets cut off
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected boolean subsamplingSanityCheck() {

        try {
            final int height = Integer.parseInt(txtHeight.getText());
            final int width = Integer.parseInt(txtWidth.getText());

            if ( (height < 1) || (width < 1)) {
                return false;
            }

            return true;
        } catch (final NumberFormatException nfe) {
            return false;
        }
    }

    /**
     * Makes an image preview of the image currentPath/selectedFilename and draws it in the image preview area.
     * 
     * @param currentPath String
     * @param selectedFilename String
     */
    private void makePreview(final String currentPath, final String selectedFilename) {
        if ( (selectedFilename == null) || (currentPath == null) || selectedFilename.equals(previewFilename)) {
            return; // prevent multiple image loads/redraws
        } else {
            previewFilename = selectedFilename;
        }

        previewPanel.removeAll();

        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        final ModelImage modelImage = fileIO.readOneImage(selectedFilename, currentPath);

        if (modelImage == null) {
            Preferences.debug("Preview image loading failed (modelImage object is null).");

            return;
        }

        try {
            final ViewJComponentPreviewImage previewImage = new ViewJComponentPreviewImage(modelImage, modelImage
                    .getExtents(), this);

            final int[] imageData = new int[modelImage.getExtents()[0] * modelImage.getExtents()[1]];
            modelImage.exportData(0, imageData.length, imageData);

            previewImage.importImage(imageData);
            previewImage.createImg(0);

            lblOrigDim.setText("Image dimensions: " + modelImage.getExtents()[0] + "x" + modelImage.getExtents()[1]);

            previewPanel.add(previewImage);
            previewPanel.validate();
            previewPanel.repaint();

            if (txtWidth.getText().equals("") && txtHeight.getText().equals("") && enableCheckbox.isSelected()) {
                txtWidth.setText(String.valueOf(modelImage.getExtents()[0] / 2));
                txtHeight.setText(String.valueOf(modelImage.getExtents()[1] / 2));
            }
        } catch (final Exception e) {
            //System.out.println("errr");
            Preferences.debug(e.getMessage());
        } finally {
            modelImage.disposeLocal();
        }
    }

    /**
     * Method removed selected items from the file list.
     */
    private void removeSelected() {
        final int[] selectedRows = filenameTable.getSelectedRows();
        if (selectedRows.length > 0) {
            final Vector<String> fileVect = new Vector<String>();
            final int rowCount = filenameTableModel.getRowCount();
            for (int i = 0; i < rowCount; i++) {
                if ( !filenameTable.isRowSelected(i)) {
                    fileVect.addElement((String) filenameTableModel.getValueAt(i, 0));
                }
            }
            for (int i = rowCount - 1; i >= 0; i--) {
                filenameTableModel.removeRow(i);
            }
            for (int i = 0; i < fileVect.size(); i++) {
                final Vector<String> newRow = new Vector<String>();
                newRow.addElement(fileVect.get(i));
                filenameTableModel.addRow(newRow);
            }
        }
    }

    /**
     * Method keep selected items from the file list.
     */
    private void keepSelected() {
        final int[] selectedRows = filenameTable.getSelectedRows();
        if (selectedRows.length > 0) {
            final Vector<String> fileVect = new Vector<String>();
            final int rowCount = filenameTableModel.getRowCount();
            for (int i = 0; i < rowCount; i++) {
                if (filenameTable.isRowSelected(i)) {
                    fileVect.addElement((String) filenameTableModel.getValueAt(i, 0));
                }
            }
            for (int i = rowCount - 1; i >= 0; i--) {
                filenameTableModel.removeRow(i);
            }
            for (int i = 0; i < fileVect.size(); i++) {
                final Vector<String> newRow = new Vector<String>();
                newRow.addElement(fileVect.get(i));
                filenameTableModel.addRow(newRow);
            }
        }
    }

    /**
     * The purpose of this method is to load the previous values in from the preferences file.
     */
    private void setDefaults() {
        txtSlices.setText(Preferences.getLastOpenSequenceSlices());
        txtChannels.setText(Preferences.getLastOpenSequenceChannels());
        txtTimePoints.setText(Preferences.getLastOpenSequenceTimePoints());

        final String ordering = Preferences.getLastOpenSequenceOrdering();

        try {
            final int selectedSequence = Integer.parseInt(ordering);

            switch (selectedSequence) {

                case ZCT:
                    radZCT.setSelected(true);
                    break;

                case ZTC:
                    radZTC.setSelected(true);
                    break;

                case CTZ:
                    radCTZ.setSelected(true);
                    break;

                case CZT:
                    radCZT.setSelected(true);
                    break;

                case TZC:
                    radTZC.setSelected(true);
                    break;

                case TCZ:
                    radTCZ.setSelected(true);
                    break;
            }
        } catch (final NumberFormatException nfe) {}
    }

}
